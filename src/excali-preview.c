/* excali-preview.c --- Approximate zoom previews from rendered pixels  -*- c-file-style: "linux" -*- */
/* Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later */

#include <math.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include <cairo.h>

#include "excali-preview.h"

static cairo_surface_t *wrap(const uint32_t *pixels, int width, int height)
{
	return cairo_image_surface_create_for_data((unsigned char *)pixels,
	                                           CAIRO_FORMAT_ARGB32, width,
	                                           height, width * 4);
}

bool excali_zoom_preview(uint32_t *dst, const uint32_t *src, int width,
                        int height, double scale, double tx, double ty)
{
	size_t bytes = (size_t)width * height * 4;
	uint32_t *copy = NULL;
	if (dst == src) {
		copy = malloc(bytes);
		if (!copy)
			return false;
		memcpy(copy, src, bytes);
		src = copy;
	}
	cairo_surface_t *source = wrap(src, width, height);
	cairo_surface_t *target = wrap(dst, width, height);
	cairo_t *cr = cairo_create(target);
	cairo_set_operator(cr, CAIRO_OPERATOR_SOURCE);

	/* Where the scaled image lands, snapped to whole pixels so that the
	   clip below stays a fast pixel-aligned rectangle.  */
	double x1 = round(tx), y1 = round(ty);
	double x2 = round(tx + scale * width), y2 = round(ty + scale * height);
	x1 = fmax(x1, 0), y1 = fmax(y1, 0);
	x2 = fmin(x2, width), y2 = fmin(y2, height);
	bool covered = x2 > x1 && y2 > y1;

	/* White outside the image: the whole buffer minus its rectangle.  */
	cairo_set_fill_rule(cr, CAIRO_FILL_RULE_EVEN_ODD);
	cairo_rectangle(cr, 0, 0, width, height);
	if (covered)
		cairo_rectangle(cr, x1, y1, x2 - x1, y2 - y1);
	cairo_set_source_rgb(cr, 1, 1, 1);
	cairo_fill(cr);

	if (covered) {
		cairo_rectangle(cr, x1, y1, x2 - x1, y2 - y1);
		cairo_clip(cr);
		cairo_translate(cr, tx, ty);
		cairo_scale(cr, scale, scale);
		cairo_set_source_surface(cr, source, 0, 0);
		cairo_pattern_t *pattern = cairo_get_source(cr);
		cairo_pattern_set_filter(pattern, CAIRO_FILTER_BILINEAR);
		/* Rounding the clip can expose up to a pixel past the image
		   edge; repeat the edge there instead of blending in black.  */
		cairo_pattern_set_extend(pattern, CAIRO_EXTEND_PAD);
		cairo_paint(cr);
	}

	cairo_destroy(cr);
	cairo_surface_flush(target);
	cairo_surface_destroy(target);
	cairo_surface_destroy(source);
	free(copy);
	return true;
}

double excali_mean_diff(const uint32_t *a, const uint32_t *b, int width,
                       int height)
{
	size_t n = (size_t)width * height;
	if (n == 0)
		return 0;
	uint64_t sum = 0;
	for (size_t i = 0; i < n; ++i)
		for (int shift = 0; shift < 24; shift += 8)
			sum += (uint64_t)abs((int)((a[i] >> shift) & 0xff) -
			                     (int)((b[i] >> shift) & 0xff));
	return (double)sum / (3.0 * n);
}
