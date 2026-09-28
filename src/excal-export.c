/* excal-export.c --- PNG and SVG export, embedded scenes  -*- c-file-style: "linux" -*-
 * Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Port of upstream scene/export.ts `exportToCanvas' (PNG) on top of the
 * regular renderer, with data/image.ts `encodePngMetadata' for the
 * embedded scene.  SVG output is Cairo's SVG surface rather than
 * upstream's hand-built DOM; excal-export.el adds the metadata.
 */

#include "excal-export.h"
#include "excal-frame.h"
#include "excal-overlay.h"

#include <cairo.h>
#ifdef CAIRO_HAS_SVG_SURFACE
#include <cairo-svg.h>
#endif
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <zlib.h>

typedef struct {
	unsigned char *data;
	size_t len, capacity;
	bool failed;
} Buffer;

static bool buffer_add(Buffer *b, const void *data, size_t length)
{
	if (b->failed)
		return false;
	if (b->len + length + 1 > b->capacity) {
		size_t capacity = (b->capacity + length + 1) * 2;
		unsigned char *grown = realloc(b->data, capacity);
		if (!grown) {
			b->failed = true;
			return false;
		}
		b->data = grown;
		b->capacity = capacity;
	}
	memcpy(b->data + b->len, data, length);
	b->len += length;
	b->data[b->len] = '\0';
	return true;
}

static cairo_status_t write_stream(void *closure, const unsigned char *data,
                                   unsigned int length)
{
	return buffer_add(closure, data, length) ? CAIRO_STATUS_SUCCESS
	                                         : CAIRO_STATUS_WRITE_ERROR;
}

static int hex_digit(char c)
{
	if (c >= '0' && c <= '9')
		return c - '0';
	if (c >= 'a' && c <= 'f')
		return c - 'a' + 10;
	if (c >= 'A' && c <= 'F')
		return c - 'A' + 10;
	return -1;
}

/* Parse "#rgb", "#rgba", "#rrggbb" or "#rrggbbaa".  */
static bool parse_hex_color(const char *s, double rgba[4])
{
	if (!s || s[0] != '#')
		return false;
	size_t len = strlen(s + 1);
	int v[8];
	if (len != 3 && len != 4 && len != 6 && len != 8)
		return false;
	for (size_t i = 0; i < len; ++i)
		if ((v[i] = hex_digit(s[1 + i])) < 0)
			return false;
	bool shortform = len <= 4;
	for (int c = 0; c < 4; ++c) {
		if ((size_t)c >= (shortform ? len : len / 2)) {
			rgba[c] = 1;
			continue;
		}
		rgba[c] = shortform ? v[c] * 17 / 255.0
		                    : (v[2 * c] * 16 + v[2 * c + 1]) / 255.0;
	}
	return true;
}

static void render_scene(cairo_t *cr, const ExcalElement *elements,
                         size_t count, const ExcalExport *opts,
                         double scale)
{
	double bg[4];
	if (opts->background && parse_hex_color(opts->background, bg)) {
		cairo_save(cr);
		cairo_set_source_rgba(cr, bg[0], bg[1], bg[2], bg[3]);
		cairo_paint(cr);
		cairo_restore(cr);
	}
	cairo_scale(cr, scale, scale);
	cairo_translate(cr, -opts->x, -opts->y);
	/* Exports render at zoom 1; frame names come as text elements.  */
	ExcalFrameConfig config = {1.0, opts->outline, opts->clip, false};
	excal_render_prepare(false);
	excal_frame_begin_pass(elements, count, &config);
	for (size_t i = 0; i < count; ++i) {
		if (excal_overlay_p(elements[i].type))
			continue;
		ExcalElement scratch;
		const ExcalElement *e =
		        excal_frame_clip_begin(cr, &elements[i], &scratch);
		excal_draw_element(cr, e);
		excal_frame_clip_end(cr);
	}
	excal_frame_end_pass();
}

static void put_u32(unsigned char *p, uint32_t v)
{
	p[0] = (unsigned char)(v >> 24);
	p[1] = (unsigned char)(v >> 16);
	p[2] = (unsigned char)(v >> 8);
	p[3] = (unsigned char)v;
}

static uint32_t get_u32(const unsigned char *p)
{
	return (uint32_t)p[0] << 24 | (uint32_t)p[1] << 16 |
	       (uint32_t)p[2] << 8 | p[3];
}

/* Offset of the IEND chunk in PNG, or 0.  */
static size_t find_iend(const unsigned char *png, size_t len)
{
	size_t pos = 8;
	while (pos + 12 <= len) {
		uint32_t length = get_u32(png + pos);
		if (memcmp(png + pos + 4, "IEND", 4) == 0)
			return pos;
		if (length > len - pos - 12)
			return 0;
		pos += 12 + length;
	}
	return 0;
}

bool excal_export_png(const ExcalElement *elements, size_t count,
                      const ExcalExport *opts, const char *keyword,
                      const unsigned char *text, size_t text_len,
                      const char *path)
{
	/* Canvas sizes truncate: canvas.width = width * exportScale.  */
	int w = (int)(opts->width * opts->scale);
	int h = (int)(opts->height * opts->scale);
	if (w <= 0 || h <= 0 || w > 32767 || h > 32767)
		return false;
	cairo_surface_t *surface =
	        cairo_image_surface_create(CAIRO_FORMAT_ARGB32, w, h);
	if (cairo_surface_status(surface) != CAIRO_STATUS_SUCCESS) {
		cairo_surface_destroy(surface);
		return false;
	}
	cairo_t *cr = cairo_create(surface);
	render_scene(cr, elements, count, opts, opts->scale);
	cairo_destroy(cr);
	Buffer png = {0};
	cairo_status_t status =
	        cairo_surface_write_to_png_stream(surface, write_stream, &png);
	cairo_surface_destroy(surface);
	size_t iend = status == CAIRO_STATUS_SUCCESS
	                      ? find_iend(png.data, png.len)
	                      : 0;
	bool ok = iend > 0;
	FILE *f = ok ? fopen(path, "wb") : NULL;
	ok = f != NULL;
	if (ok && text && keyword) {
		size_t klen = strlen(keyword);
		size_t data_len = klen + 1 + text_len;
		unsigned char *chunk = malloc(12 + data_len);
		ok = chunk && data_len <= 0x7fffffff;
		if (ok) {
			put_u32(chunk, (uint32_t)data_len);
			memcpy(chunk + 4, "tEXt", 4);
			memcpy(chunk + 8, keyword, klen);
			chunk[8 + klen] = 0;
			memcpy(chunk + 9 + klen, text, text_len);
			uLong crc = crc32(0L, Z_NULL, 0);
			crc = crc32(crc, chunk + 4, (uInt)(4 + data_len));
			put_u32(chunk + 8 + data_len, (uint32_t)crc);
			ok = fwrite(png.data, 1, iend, f) == iend &&
			     fwrite(chunk, 1, 12 + data_len, f) ==
			             12 + data_len &&
			     fwrite(png.data + iend, 1, png.len - iend, f) ==
			             png.len - iend;
		}
		free(chunk);
	} else if (ok) {
		ok = fwrite(png.data, 1, png.len, f) == png.len;
	}
	if (f)
		ok = fclose(f) == 0 && ok;
	free(png.data);
	return ok;
}

char *excal_export_svg(const ExcalElement *elements, size_t count,
                       const ExcalExport *opts, size_t *len)
{
#ifdef CAIRO_HAS_SVG_SURFACE
	Buffer svg = {0};
	cairo_surface_t *surface = cairo_svg_surface_create_for_stream(
	        write_stream, &svg, opts->width, opts->height);
#if CAIRO_VERSION >= CAIRO_VERSION_ENCODE(1, 16, 0)
	cairo_svg_surface_set_document_unit(surface, CAIRO_SVG_UNIT_USER);
#endif
	cairo_t *cr = cairo_create(surface);
	render_scene(cr, elements, count, opts, 1.0);
	cairo_destroy(cr);
	cairo_surface_finish(surface);
	bool ok = cairo_surface_status(surface) == CAIRO_STATUS_SUCCESS &&
	          !svg.failed && svg.data;
	cairo_surface_destroy(surface);
	if (!ok) {
		free(svg.data);
		return NULL;
	}
	*len = svg.len;
	return (char *)svg.data;
#else
	(void)elements;
	(void)count;
	(void)opts;
	(void)len;
	return NULL;
#endif
}

unsigned char *excal_zlib_compress(const unsigned char *data, size_t len,
                                   size_t *out_len)
{
	uLongf bound = compressBound((uLong)len);
	unsigned char *out = malloc(bound ? bound : 1);
	if (!out)
		return NULL;
	/* pako's default level.  */
	if (compress2(out, &bound, data, (uLong)len, 6) != Z_OK) {
		free(out);
		return NULL;
	}
	*out_len = bound;
	return out;
}

unsigned char *excal_zlib_decompress(const unsigned char *data, size_t len,
                                     size_t *out_len)
{
	z_stream zs;
	memset(&zs, 0, sizeof zs);
	if (inflateInit(&zs) != Z_OK)
		return NULL;
	Buffer out = {0};
	unsigned char chunk[16384];
	zs.next_in = (Bytef *)data;
	zs.avail_in = (uInt)len;
	int status;
	do {
		zs.next_out = chunk;
		zs.avail_out = sizeof chunk;
		status = inflate(&zs, Z_NO_FLUSH);
		if (status != Z_OK && status != Z_STREAM_END)
			break;
		buffer_add(&out, chunk, sizeof chunk - zs.avail_out);
	} while (status != Z_STREAM_END && (zs.avail_in > 0 || zs.avail_out == 0));
	inflateEnd(&zs);
	if (status != Z_STREAM_END || out.failed) {
		free(out.data);
		return NULL;
	}
	if (!out.data)
		out.data = calloc(1, 1);
	*out_len = out.len;
	return out.data;
}
