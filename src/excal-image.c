/* excal-image.c --- Image elements: decoding, cache and drawing  -*- c-file-style: "linux" -*-
 *
 * Images live in a session-wide cache keyed by file id, filled once per
 * file from its data URL (`excal-native-image-register') and emptied by
 * `excal-native-image-forget'.  PNG is decoded by Cairo itself; SVG needs
 * librsvg (EXCAL_HAVE_RSVG), WebP libwebp (EXCAL_HAVE_WEBP), and JPEG,
 * GIF (first frame), BMP, ICO and the rest gdk-pixbuf (EXCAL_HAVE_PIXBUF).
 * An image that cannot be decoded is drawn as upstream's error
 * placeholder.
 */

#include "excal-image.h"

#include <ctype.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifdef EXCAL_HAVE_RSVG
#include <librsvg/rsvg.h>
#endif
#ifdef EXCAL_HAVE_PIXBUF
#include <gdk-pixbuf/gdk-pixbuf.h>
#endif
#ifdef EXCAL_HAVE_WEBP
#include <webp/decode.h>
#endif

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

typedef struct Image {
	char *id;
	char *mime;
	double width, height; /* Natural size in CSS pixels.  */
	cairo_surface_t *surface; /* Raster images.  */
#ifdef EXCAL_HAVE_RSVG
	RsvgHandle *svg; /* Vector images, drawn at any scale.  */
#endif
	struct Image *next;
} Image;

static Image *images;

/* Data URLs.  */

static int base64_value(unsigned char c)
{
	if (c >= 'A' && c <= 'Z')
		return c - 'A';
	if (c >= 'a' && c <= 'z')
		return c - 'a' + 26;
	if (c >= '0' && c <= '9')
		return c - '0' + 52;
	if (c == '+' || c == '-')
		return 62;
	if (c == '/' || c == '_')
		return 63;
	return -1;
}

static unsigned char *base64_decode(const char *s, size_t len, size_t *out)
{
	unsigned char *buf = malloc(len / 4 * 3 + 4);
	if (!buf)
		return NULL;
	size_t n = 0;
	uint32_t acc = 0;
	int bits = 0;
	for (size_t i = 0; i < len; ++i) {
		unsigned char c = (unsigned char)s[i];
		if (c == '=')
			break;
		int v = base64_value(c);
		if (v < 0) {
			if (isspace(c))
				continue;
			free(buf);
			return NULL;
		}
		acc = (acc << 6) | (uint32_t)v;
		bits += 6;
		if (bits >= 8) {
			bits -= 8;
			buf[n++] = (unsigned char)(acc >> bits);
		}
	}
	*out = n;
	return buf;
}

static int hex_value(char c)
{
	if (c >= '0' && c <= '9')
		return c - '0';
	if (c >= 'a' && c <= 'f')
		return c - 'a' + 10;
	if (c >= 'A' && c <= 'F')
		return c - 'A' + 10;
	return -1;
}

static unsigned char *percent_decode(const char *s, size_t len, size_t *out)
{
	unsigned char *buf = malloc(len + 1);
	if (!buf)
		return NULL;
	size_t n = 0;
	for (size_t i = 0; i < len; ++i) {
		int hi, lo;
		if (s[i] == '%' && i + 2 < len &&
		    (hi = hex_value(s[i + 1])) >= 0 &&
		    (lo = hex_value(s[i + 2])) >= 0) {
			buf[n++] = (unsigned char)(hi * 16 + lo);
			i += 2;
		} else {
			buf[n++] = (unsigned char)s[i];
		}
	}
	*out = n;
	return buf;
}

unsigned char *excal_data_url_decode(const char *url, size_t len, char **mime,
                                     size_t *len_out)
{
	*mime = NULL;
	if (len < 5 || strncmp(url, "data:", 5) != 0)
		return NULL;
	const char *comma = memchr(url, ',', len);
	if (!comma)
		return NULL;
	const char *meta = url + 5;
	size_t meta_len = (size_t)(comma - meta);
	size_t mime_len = 0;
	while (mime_len < meta_len && meta[mime_len] != ';')
		++mime_len;
	bool base64 = false;
	for (size_t i = mime_len; i < meta_len;) {
		size_t j = i + 1;
		while (j < meta_len && meta[j] != ';')
			++j;
		if (j - i - 1 == 6 && strncmp(meta + i + 1, "base64", 6) == 0)
			base64 = true;
		i = j;
	}
	*mime = malloc(mime_len + 1);
	if (!*mime)
		return NULL;
	for (size_t i = 0; i < mime_len; ++i)
		(*mime)[i] = (char)tolower((unsigned char)meta[i]);
	(*mime)[mime_len] = '\0';
	const char *data = comma + 1;
	size_t data_len = len - (size_t)(data - url);
	unsigned char *bytes = base64 ? base64_decode(data, data_len, len_out)
	                              : percent_decode(data, data_len, len_out);
	if (!bytes) {
		free(*mime);
		*mime = NULL;
	}
	return bytes;
}

/* Decoders.  */

typedef struct {
	const unsigned char *data;
	size_t len, pos;
} Reader;

static cairo_status_t read_bytes(void *closure, unsigned char *out,
                                 unsigned int length)
{
	Reader *r = closure;
	if (r->len - r->pos < length)
		return CAIRO_STATUS_READ_ERROR;
	memcpy(out, r->data + r->pos, length);
	r->pos += length;
	return CAIRO_STATUS_SUCCESS;
}

static cairo_surface_t *decode_png(const unsigned char *data, size_t len)
{
	Reader r = {data, len, 0};
	cairo_surface_t *s =
	        cairo_image_surface_create_from_png_stream(read_bytes, &r);
	if (cairo_surface_status(s) != CAIRO_STATUS_SUCCESS) {
		cairo_surface_destroy(s);
		return NULL;
	}
	return s;
}

/* Premultiplied ARGB32 surface from straight-alpha RGB(A) PIXELS.  */
__attribute__((unused)) static cairo_surface_t *
surface_from_rgba(const unsigned char *pixels, int width, int height,
                  int stride, int channels)
{
	cairo_surface_t *s =
	        cairo_image_surface_create(CAIRO_FORMAT_ARGB32, width, height);
	if (cairo_surface_status(s) != CAIRO_STATUS_SUCCESS) {
		cairo_surface_destroy(s);
		return NULL;
	}
	cairo_surface_flush(s);
	unsigned char *dst = cairo_image_surface_get_data(s);
	int dst_stride = cairo_image_surface_get_stride(s);
	for (int y = 0; y < height; ++y) {
		const unsigned char *p = pixels + (size_t)y * stride;
		uint32_t *out = (uint32_t *)(dst + (size_t)y * dst_stride);
		for (int x = 0; x < width; ++x, p += channels) {
			uint32_t a = channels == 4 ? p[3] : 255;
			uint32_t r = (p[0] * a + 127) / 255;
			uint32_t g = (p[1] * a + 127) / 255;
			uint32_t b = (p[2] * a + 127) / 255;
			out[x] = a << 24 | r << 16 | g << 8 | b;
		}
	}
	cairo_surface_mark_dirty(s);
	return s;
}

#ifdef EXCAL_HAVE_WEBP
static cairo_surface_t *decode_webp(const unsigned char *data, size_t len)
{
	int w, h;
	uint8_t *rgba = WebPDecodeRGBA(data, len, &w, &h);
	if (!rgba)
		return NULL;
	cairo_surface_t *s = surface_from_rgba(rgba, w, h, w * 4, 4);
	WebPFree(rgba);
	return s;
}
#endif

#ifdef EXCAL_HAVE_PIXBUF
static cairo_surface_t *decode_pixbuf(const unsigned char *data, size_t len)
{
	GdkPixbufLoader *loader = gdk_pixbuf_loader_new();
	GError *error = NULL;
	bool ok = gdk_pixbuf_loader_write(loader, data, len, &error);
	ok = gdk_pixbuf_loader_close(loader, ok ? &error : NULL) && ok;
	g_clear_error(&error);
	cairo_surface_t *s = NULL;
	/* For animations this is the first frame.  */
	GdkPixbuf *pixbuf = ok ? gdk_pixbuf_loader_get_pixbuf(loader) : NULL;
	if (pixbuf) {
		/* Browsers honour EXIF orientation.  */
		GdkPixbuf *oriented =
		        gdk_pixbuf_apply_embedded_orientation(pixbuf);
		if (oriented && gdk_pixbuf_get_bits_per_sample(oriented) == 8)
			s = surface_from_rgba(
			        gdk_pixbuf_read_pixels(oriented),
			        gdk_pixbuf_get_width(oriented),
			        gdk_pixbuf_get_height(oriented),
			        gdk_pixbuf_get_rowstride(oriented),
			        gdk_pixbuf_get_n_channels(oriented));
		if (oriented)
			g_object_unref(oriented);
	}
	g_object_unref(loader);
	return s;
}
#endif

#ifdef EXCAL_HAVE_RSVG
static RsvgHandle *decode_svg(const unsigned char *data, size_t len,
                              double *width, double *height)
{
	GError *error = NULL;
	RsvgHandle *h = rsvg_handle_new_from_data(data, len, &error);
	if (!h) {
		g_clear_error(&error);
		return NULL;
	}
	rsvg_handle_set_dpi(h, 96);
	gdouble w = 0, hh = 0;
	if (rsvg_handle_get_intrinsic_size_in_pixels(h, &w, &hh) && w > 0 &&
	    hh > 0) {
		*width = w;
		*height = hh;
		return h;
	}
	/* No absolute size: browsers use the default object size, 300 wide,
	   keeping the viewBox's aspect ratio when there is one.  */
	gboolean has_w, has_h, has_vb;
	RsvgLength lw, lh;
	RsvgRectangle vb;
	rsvg_handle_get_intrinsic_dimensions(h, &has_w, &lw, &has_h, &lh,
	                                     &has_vb, &vb);
	*width = 300;
	*height = has_vb && vb.width > 0 ? 300 * vb.height / vb.width : 150;
	return h;
}
#endif

static bool looks_like_svg(const char *mime, const unsigned char *data,
                           size_t len)
{
	if (mime && strcmp(mime, "image/svg+xml") == 0)
		return true;
	size_t i = 0;
	if (len >= 3 && data[0] == 0xef && data[1] == 0xbb && data[2] == 0xbf)
		i = 3;
	while (i < len && isspace(data[i]))
		++i;
	return i < len && data[i] == '<';
}

static void image_free(Image *img)
{
	if (img->surface)
		cairo_surface_destroy(img->surface);
#ifdef EXCAL_HAVE_RSVG
	if (img->svg)
		g_object_unref(img->svg);
#endif
	free(img->id);
	free(img->mime);
	free(img);
}

static Image *image_find(const char *id)
{
	if (!id)
		return NULL;
	for (Image *img = images; img; img = img->next)
		if (strcmp(img->id, id) == 0)
			return img;
	return NULL;
}

bool excal_image_forget(const char *id)
{
	for (Image **p = &images; *p; p = &(*p)->next)
		if (strcmp((*p)->id, id) == 0) {
			Image *img = *p;
			*p = img->next;
			image_free(img);
			return true;
		}
	return false;
}

bool excal_image_register(const char *id, const char *url, size_t len,
                          const char **error)
{
	char *mime = NULL;
	size_t n = 0;
	unsigned char *data = excal_data_url_decode(url, len, &mime, &n);
	*error = NULL;
	if (!data) {
		*error = "invalid data URL";
		free(mime);
		return false;
	}
	Image *img = calloc(1, sizeof *img);
	img->id = strdup(id);
	img->mime = mime;
	static const unsigned char png_sig[8] = {0x89, 'P',  'N',  'G',
	                                         '\r', '\n', 0x1a, '\n'};
	bool png = n >= 8 && memcmp(data, png_sig, 8) == 0;
	bool webp = n >= 12 && memcmp(data, "RIFF", 4) == 0 &&
	            memcmp(data + 8, "WEBP", 4) == 0;
	if (png) {
		img->surface = decode_png(data, n);
	} else if (webp) {
#if defined EXCAL_HAVE_WEBP
		img->surface = decode_webp(data, n);
#elif defined EXCAL_HAVE_PIXBUF
		img->surface = decode_pixbuf(data, n);
#endif
	} else if (looks_like_svg(mime, data, n)) {
#ifdef EXCAL_HAVE_RSVG
		img->svg = decode_svg(data, n, &img->width, &img->height);
#endif
	} else {
#ifdef EXCAL_HAVE_PIXBUF
		img->surface = decode_pixbuf(data, n);
#endif
	}
	free(data);
	if (img->surface) {
		img->width = cairo_image_surface_get_width(img->surface);
		img->height = cairo_image_surface_get_height(img->surface);
	}
	bool ok = img->surface != NULL;
#ifdef EXCAL_HAVE_RSVG
	ok = ok || img->svg != NULL;
#endif
	if (!ok || img->width <= 0 || img->height <= 0) {
		*error = "cannot decode image";
		image_free(img);
		return false;
	}
	excal_image_forget(id);
	img->next = images;
	images = img;
	return true;
}

bool excal_image_info(const char *id, double *width, double *height,
                      const char **mime)
{
	Image *img = image_find(id);
	if (!img)
		return false;
	*width = img->width;
	*height = img->height;
	*mime = img->mime ? img->mime : "";
	return true;
}

size_t excal_image_count(void)
{
	size_t n = 0;
	for (Image *img = images; img; img = img->next)
		++n;
	return n;
}

typedef struct {
	unsigned char *data;
	size_t len, capacity;
} Buffer;

static cairo_status_t write_bytes(void *closure, const unsigned char *data,
                                  unsigned int length)
{
	Buffer *b = closure;
	if (b->len + length > b->capacity) {
		size_t capacity = (b->capacity + length) * 2;
		unsigned char *grown = realloc(b->data, capacity);
		if (!grown)
			return CAIRO_STATUS_WRITE_ERROR;
		b->data = grown;
		b->capacity = capacity;
	}
	memcpy(b->data + b->len, data, length);
	b->len += length;
	return CAIRO_STATUS_SUCCESS;
}

unsigned char *excal_image_png(const char *id, int max_size, size_t *len)
{
	Image *img = image_find(id);
	if (!img || !img->surface || max_size <= 0)
		return NULL;
	double scale = fmin(1.0, max_size / fmax(img->width, img->height));
	int w = (int)fmax(1, round(img->width * scale));
	int h = (int)fmax(1, round(img->height * scale));
	cairo_surface_t *s =
	        cairo_image_surface_create(CAIRO_FORMAT_ARGB32, w, h);
	cairo_t *cr = cairo_create(s);
	cairo_scale(cr, w / img->width, h / img->height);
	cairo_set_source_surface(cr, img->surface, 0, 0);
	cairo_pattern_set_filter(cairo_get_source(cr), CAIRO_FILTER_GOOD);
	cairo_paint(cr);
	cairo_destroy(cr);
	Buffer b = {0};
	cairo_status_t status =
	        cairo_surface_write_to_png_stream(s, write_bytes, &b);
	cairo_surface_destroy(s);
	if (status != CAIRO_STATUS_SUCCESS) {
		free(b.data);
		return NULL;
	}
	*len = b.len;
	return b.data;
}

/* SVG path data, enough for the placeholder icons.  */

static const char *skip_separators(const char *p)
{
	while (*p && (isspace((unsigned char)*p) || *p == ','))
		++p;
	return p;
}

/* Parse a number at *PP without depending on the C locale.  */
static bool parse_number(const char **pp, double *out)
{
	const char *p = skip_separators(*pp);
	double sign = 1;
	if (*p == '-' || *p == '+')
		sign = *p++ == '-' ? -1 : 1;
	if (!isdigit((unsigned char)*p) &&
	    !(*p == '.' && isdigit((unsigned char)p[1])))
		return false;
	double value = 0;
	while (isdigit((unsigned char)*p))
		value = value * 10 + (*p++ - '0');
	if (*p == '.') {
		++p;
		double scale = 0.1;
		while (isdigit((unsigned char)*p)) {
			value += (*p++ - '0') * scale;
			scale /= 10;
		}
	}
	if ((*p == 'e' || *p == 'E') &&
	    (isdigit((unsigned char)p[1]) ||
	     ((p[1] == '-' || p[1] == '+') && isdigit((unsigned char)p[2])))) {
		++p;
		int esign = 1, exp = 0;
		if (*p == '-' || *p == '+')
			esign = *p++ == '-' ? -1 : 1;
		while (isdigit((unsigned char)*p))
			exp = exp * 10 + (*p++ - '0');
		value *= pow(10, esign * exp);
	}
	*out = sign * value;
	*pp = p;
	return true;
}

void excal_svg_path(cairo_t *cr, const char *d)
{
	double cx = 0, cy = 0, sx = 0, sy = 0; /* Current, subpath start.  */
	double lcx = 0, lcy = 0;               /* Last cubic control point.  */
	char cmd = 0, prev = 0;
	const char *p = d;
	for (;;) {
		p = skip_separators(p);
		if (!*p)
			break;
		if (isalpha((unsigned char)*p))
			cmd = *p++;
		else if (!cmd)
			break;
		bool rel = islower((unsigned char)cmd);
		double ox = rel ? cx : 0, oy = rel ? cy : 0;
		double a[6];
		int need;
		switch (cmd) {
		case 'Z':
		case 'z':
			cairo_close_path(cr);
			cx = sx, cy = sy;
			prev = cmd;
			cmd = 0;
			continue;
		case 'H': case 'h': case 'V': case 'v':
			need = 1;
			break;
		case 'M': case 'm': case 'L': case 'l': case 'T': case 't':
			need = 2;
			break;
		case 'S': case 's': case 'Q': case 'q':
			need = 4;
			break;
		case 'C': case 'c':
			need = 6;
			break;
		default:
			return; /* Arcs are not needed.  */
		}
		for (int i = 0; i < need; ++i)
			if (!parse_number(&p, &a[i]))
				return;
		switch (cmd) {
		case 'M': case 'm':
			cx = ox + a[0], cy = oy + a[1];
			cairo_move_to(cr, cx, cy);
			sx = cx, sy = cy;
			cmd = rel ? 'l' : 'L'; /* Implicit lineto.  */
			break;
		case 'L': case 'l': case 'T': case 't':
			cx = ox + a[0], cy = oy + a[1];
			cairo_line_to(cr, cx, cy);
			break;
		case 'H': case 'h':
			cx = ox + a[0];
			cairo_line_to(cr, cx, cy);
			break;
		case 'V': case 'v':
			cy = oy + a[0];
			cairo_line_to(cr, cx, cy);
			break;
		case 'C': case 'c':
			cairo_curve_to(cr, ox + a[0], oy + a[1], ox + a[2],
			               oy + a[3], ox + a[4], oy + a[5]);
			lcx = ox + a[2], lcy = oy + a[3];
			cx = ox + a[4], cy = oy + a[5];
			break;
		case 'S': case 's': {
			bool smooth = prev && strchr("CcSs", prev) != NULL;
			double x1 = smooth ? 2 * cx - lcx : cx;
			double y1 = smooth ? 2 * cy - lcy : cy;
			cairo_curve_to(cr, x1, y1, ox + a[0], oy + a[1],
			               ox + a[2], oy + a[3]);
			lcx = ox + a[0], lcy = oy + a[1];
			cx = ox + a[2], cy = oy + a[3];
			break;
		}
		case 'Q': case 'q': {
			double qx = ox + a[0], qy = oy + a[1];
			double ex = ox + a[2], ey = oy + a[3];
			cairo_curve_to(cr, cx + 2.0 / 3 * (qx - cx),
			               cy + 2.0 / 3 * (qy - cy),
			               ex + 2.0 / 3 * (qx - ex),
			               ey + 2.0 / 3 * (qy - ey), ex, ey);
			cx = ex, cy = ey;
			break;
		}
		}
		prev = cmd;
	}
}

/* Placeholders: upstream IMAGE_PLACEHOLDER_IMG and
   IMAGE_ERROR_PLACEHOLDER_IMG (Font Awesome "image", plus "ban").  */

static const char image_icon[] =
        "M464 448H48c-26.51 0-48-21.49-48-48V112c0-26.51 21.49-48 48-48h416"
        "c26.51 0 48 21.49 48 48v288c0 26.51-21.49 48-48 48zM112 120"
        "c-30.928 0-56 25.072-56 56s25.072 56 56 56 56-25.072 56-56"
        "-25.072-56-56-56zM64 384h384V272l-87.515-87.515c-4.686-4.686"
        "-12.284-4.686-16.971 0L208 320l-55.515-55.515c-4.686-4.686"
        "-12.284-4.686-16.971 0L64 336v48z";

static const char ban_icon[] =
        "M256 8C119.034 8 8 119.033 8 256c0 136.967 111.034 248 248 248"
        "s248-111.034 248-248S392.967 8 256 8Zm130.108 117.892c65.448 65.448"
        " 70 165.481 20.677 235.637L150.47 105.216c70.204-49.356 170.226"
        "-44.735 235.638 20.676ZM125.892 386.108c-65.448-65.448-70-165.481"
        "-20.677-235.637L361.53 406.784c-70.203 49.356-170.226 44.736"
        "-235.638-20.676Z";

static void draw_placeholder(cairo_t *cr, const ExcalElement *e, double w,
                             double h)
{
	cairo_set_source_rgb(cr, 0xE7 / 255.0, 0xE7 / 255.0, 0xE7 / 255.0);
	cairo_rectangle(cr, 0, 0, w, h);
	cairo_fill(cr);
	double m = fmin(w, h);
	double size = fmin(m, fmin(m * 0.4, 100));
	if (!(size > 0))
		return;
	cairo_save(cr);
	cairo_translate(cr, w / 2 - size / 2, h / 2 - size / 2);
	cairo_set_source_rgb(cr, 0x88 / 255.0, 0x88 / 255.0, 0x88 / 255.0);
	cairo_set_fill_rule(cr, CAIRO_FILL_RULE_WINDING);
	cairo_new_path(cr);
	if (e->media.error) {
		cairo_scale(cr, size / 668, size / 668);
		cairo_matrix_t m1 = {.81709, 0, 0, .81709, 124.825, 145.825};
		cairo_matrix_t m2 = {.30366, 0, 0, .30366, 506.822, 60.065};
		cairo_save(cr);
		cairo_transform(cr, &m1);
		excal_svg_path(cr, image_icon);
		cairo_restore(cr);
		cairo_fill(cr);
		cairo_save(cr);
		cairo_transform(cr, &m2);
		excal_svg_path(cr, ban_icon);
		cairo_restore(cr);
		cairo_fill(cr);
	} else {
		cairo_scale(cr, size / 512, size / 512);
		excal_svg_path(cr, image_icon);
		cairo_fill(cr);
	}
	cairo_restore(cr);
}

/* Canvas roundRect(0, 0, W, H, R) as a closed path.  */
static void round_rect(cairo_t *cr, double w, double h, double r)
{
	r = fmin(r, fmin(w, h) / 2);
	cairo_new_sub_path(cr);
	cairo_arc(cr, w - r, r, r, -M_PI / 2, 0);
	cairo_arc(cr, w - r, h - r, r, 0, M_PI / 2);
	cairo_arc(cr, r, h - r, r, M_PI / 2, M_PI);
	cairo_arc(cr, r, r, r, M_PI, 3 * M_PI / 2);
	cairo_close_path(cr);
}

void excal_draw_image(cairo_t *cr, const ExcalElement *e)
{
	double w = fabs(e->width), h = fabs(e->height);
	if (!(w > 0) || !(h > 0))
		return;
	double sx = e->media.scale[0] < 0 ? -1 : 1;
	double sy = e->media.scale[1] < 0 ? -1 : 1;
	cairo_save(cr);
	/* Upstream rotates about the centre, then scales (flips); the
	   rotation is already applied by `draw_element'.  */
	cairo_translate(cr, e->x + w / 2, e->y + h / 2);
	cairo_scale(cr, sx, sy);
	cairo_translate(cr, -w / 2, -h / 2);
	Image *img = image_find(e->media.file_id);
	if (!img) {
		draw_placeholder(cr, e, w, h);
		cairo_restore(cr);
		return;
	}
	cairo_new_path(cr);
	if (e->media.radius > 0)
		round_rect(cr, w, h, e->media.radius);
	else
		cairo_rectangle(cr, 0, 0, w, h);
	cairo_clip(cr);
	double cx = 0, cy = 0, cw = img->width, ch = img->height;
	if (e->media.has_crop && e->media.crop[2] > 0 &&
	    e->media.crop[3] > 0) {
		cx = e->media.crop[0];
		cy = e->media.crop[1];
		cw = e->media.crop[2];
		ch = e->media.crop[3];
	}
	cairo_scale(cr, w / cw, h / ch);
	cairo_translate(cr, -cx, -cy);
	if (img->surface) {
		cairo_set_source_surface(cr, img->surface, 0, 0);
		cairo_pattern_t *pattern = cairo_get_source(cr);
		cairo_pattern_set_filter(pattern, CAIRO_FILTER_GOOD);
		cairo_pattern_set_extend(pattern, CAIRO_EXTEND_PAD);
		cairo_paint(cr);
	}
#ifdef EXCAL_HAVE_RSVG
	else if (img->svg) {
		RsvgRectangle viewport = {0, 0, img->width, img->height};
		rsvg_handle_render_document(img->svg, cr, &viewport, NULL);
	}
#endif
	cairo_restore(cr);
}

void excal_media_free(ExcalMedia *m)
{
	free(m->id);
	free(m->frame_id);
	free(m->name);
	free(m->file_id);
}
