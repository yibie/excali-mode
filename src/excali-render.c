/* excali-render.c --- Excalidraw scene rasterizer for Emacs Canvas  -*- c-file-style: "linux" -*-
 * Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Shapes come from excali-shape.c, a port of Excalidraw's shape.ts on
 * top of a roughjs port (excali-rough.c); this file turns their ops into
 * Cairo paths the way roughjs' canvas renderer does.
 */

#include "excali-render.h"
#include "excali-frame.h"
#include "excali-image.h"
#include "excali-overlay.h"
#include "excali-shape.h"
#include "excali-sticky.h"
#include "excali-text.h"

#include <cairo.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

/* M_PI is POSIX, not C11; glibc and MinGW hide it under -std=c11.  */
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

/* Colors.  */

typedef struct {
	double r, g, b, a;
} Rgba;

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

void excali_dark_filter(double rgb[3])
{
	/* invert(0.93): c * (1 - p) + (1 - c) * p.  */
	double c[3];
	for (int i = 0; i < 3; ++i)
		c[i] = rgb[i] * (1 - 0.93) + (1 - rgb[i]) * 0.93;
	/* hue-rotate(180deg): the CSS filter matrix with cos = -1, sin = 0.  */
	static const double m[3][3] = {{-0.574, 1.430, 0.144},
	                               {0.426, 0.430, 0.144},
	                               {0.426, 1.430, -0.856}};
	for (int i = 0; i < 3; ++i) {
		double v = m[i][0] * c[0] + m[i][1] * c[1] + m[i][2] * c[2];
		rgb[i] = fmin(1, fmax(0, v));
	}
}

/* Whether the render in progress uses the dark theme.  Rendering is
   single-threaded, and every render sets it.  */
static bool render_dark;

static bool parse_raw_color(const char *s, Rgba *out);

/* Parse color S as drawn in the current theme.  */
static bool parse_color(const char *s, Rgba *out)
{
	if (!parse_raw_color(s, out))
		return false;
	if (render_dark) {
		double rgb[3] = {out->r, out->g, out->b};
		excali_dark_filter(rgb);
		out->r = rgb[0], out->g = rgb[1], out->b = rgb[2];
	}
	return true;
}

static bool parse_raw_color(const char *s, Rgba *out)
{
	if (!s || !*s || strcmp(s, "transparent") == 0)
		return false;
	if (s[0] != '#')
		return false;
	size_t len = strlen(s + 1);
	int v[8];
	for (size_t i = 0; i < len && i < 8; ++i)
		if ((v[i] = hex_digit(s[1 + i])) < 0)
			return false;
	if (len == 3 || len == 4) {
		out->r = v[0] * 17 / 255.0;
		out->g = v[1] * 17 / 255.0;
		out->b = v[2] * 17 / 255.0;
		out->a = len == 4 ? v[3] * 17 / 255.0 : 1.0;
	} else if (len == 6 || len == 8) {
		out->r = (v[0] * 16 + v[1]) / 255.0;
		out->g = (v[2] * 16 + v[3]) / 255.0;
		out->b = (v[4] * 16 + v[5]) / 255.0;
		out->a = len == 8 ? (v[6] * 16 + v[7]) / 255.0 : 1.0;
	} else {
		return false;
	}
	return out->a > 0;
}


/* Shapes.  */

static void ops_path(cairo_t *cr, const RoughOps *ops)
{
	cairo_new_path(cr);
	for (size_t i = 0; i < ops->count; ++i) {
		const double *d = ops->ops[i].data;
		switch (ops->ops[i].op) {
		case ROUGH_MOVE:
			cairo_move_to(cr, d[0], d[1]);
			break;
		case ROUGH_LINE_TO:
			cairo_line_to(cr, d[0], d[1]);
			break;
		case ROUGH_BCURVE_TO:
			cairo_curve_to(cr, d[0], d[1], d[2], d[3], d[4], d[5]);
			break;
		}
	}
}

static void set_source(cairo_t *cr, const Rgba *c)
{
	cairo_set_source_rgba(cr, c->r, c->g, c->b, c->a);
}

/* roughjs `RoughCanvas.draw'.  STROKE and FILL are NULL when invisible.  */
static void draw_drawable(cairo_t *cr, const ExcaliDrawable *d,
                          const Rgba *stroke, const Rgba *fill)
{
	const RoughOptions *o = &d->rough.options;
	for (int i = 0; i < d->rough.set_count; ++i) {
		const RoughSet *set = &d->rough.sets[i];
		switch (set->type) {
		case ROUGH_SET_PATH:
			if (!stroke)
				break;
			ops_path(cr, &set->ops);
			set_source(cr, stroke);
			cairo_set_line_width(cr, o->stroke_width);
			cairo_set_dash(cr, d->dash_count ? d->dash : NULL,
			               d->dash_count, 0);
			cairo_stroke(cr);
			break;
		case ROUGH_SET_FILL_PATH:
			if (!fill)
				break;
			ops_path(cr, &set->ops);
			set_source(cr, fill);
			cairo_set_fill_rule(cr, rough_fill_evenodd(&d->rough)
			                                ? CAIRO_FILL_RULE_EVEN_ODD
			                                : CAIRO_FILL_RULE_WINDING);
			cairo_fill(cr);
			break;
		case ROUGH_SET_FILL_SKETCH:
			if (!fill)
				break;
			/* Hachure is many thin, light strokes; Cairo's coarser
			   antialiasing renders them about 3x faster and the
			   difference is hard to see.  */
			cairo_save(cr);
			cairo_set_antialias(cr, CAIRO_ANTIALIAS_FAST);
			ops_path(cr, &set->ops);
			set_source(cr, fill);
			cairo_set_line_width(cr, o->fill_weight < 0
			                                 ? o->stroke_width / 2
			                                 : o->fill_weight);
			cairo_set_dash(cr, NULL, 0, 0);
			cairo_stroke(cr);
			cairo_restore(cr);
			break;
		}
	}
}

/* Fill the freedraw outline: `getSvgPathFromStroke' drawn by Path2D.  */
static void fill_outline(cairo_t *cr, const RoughPoints *outline,
                         const Rgba *color)
{
	size_t n = outline->count / 2;
	if (n == 0)
		return;
	const double *p = outline->xy;
	cairo_new_path(cr);
	cairo_move_to(cr, p[0], p[1]);
	double cx = p[0], cy = p[1];
	for (size_t i = 0; i < n; ++i) {
		/* Q control end, as a cubic.  */
		double qx = p[4 * i], qy = p[4 * i + 1];
		double ex = p[4 * i + 2], ey = p[4 * i + 3];
		cairo_curve_to(cr, cx + 2.0 / 3 * (qx - cx),
		               cy + 2.0 / 3 * (qy - cy),
		               ex + 2.0 / 3 * (qx - ex),
		               ey + 2.0 / 3 * (qy - ey), ex, ey);
		cx = ex;
		cy = ey;
	}
	cairo_line_to(cr, p[0], p[1]);
	cairo_close_path(cr);
	set_source(cr, color);
	cairo_set_fill_rule(cr, CAIRO_FILL_RULE_WINDING);
	cairo_fill(cr);
}

static void draw_element(cairo_t *cr, const ExcaliElement *e,
                         const Rgba *canvas)
{
	if (excali_overlay_p(e->type))
		return;
	Rgba stroke = {0.118, 0.118, 0.118, 1};
	bool has_stroke = parse_color(e->stroke_color, &stroke);
	Rgba fill;
	bool has_fill = parse_color(e->background_color, &fill);

	ExcaliShape shape;
	bool shaped = e->type != EXCALI_TEXT && e->type != EXCALI_STICKYNOTE &&
	              e->type != EXCALI_IMAGE && e->type != EXCALI_FRAME;
	if (shaped)
		excali_shape_generate(e, &shape);

	cairo_save(cr);
	if (e->angle != 0) {
		/* Excalidraw rotates about the centre of
		   getElementAbsoluteCoords.  */
		double cx = e->x + e->width / 2, cy = e->y + e->height / 2;
		if (shaped) {
			cx = e->x + (shape.x1 + shape.x2) / 2;
			cy = e->y + (shape.y1 + shape.y2) / 2;
		}
		cairo_translate(cr, cx, cy);
		cairo_rotate(cr, e->angle);
		cairo_translate(cr, -cx, -cy);
	}
	if (e->opacity < 100)
		cairo_push_group(cr);

	if (shaped) {
		/* An arrow's bound label punches an even-odd hole in the
		   shaft and heads; cairo_restore below undoes the clip.  */
		if (e->type == EXCALI_ARROW)
			excali_text_clip_label_hole(cr, e);
		cairo_translate(cr, e->x, e->y);
		if (shape.butt_caps) {
			cairo_set_line_cap(cr, CAIRO_LINE_CAP_BUTT);
			cairo_set_line_join(cr, CAIRO_LINE_JOIN_MITER);
		} else {
			cairo_set_line_cap(cr, CAIRO_LINE_CAP_ROUND);
			cairo_set_line_join(cr, CAIRO_LINE_JOIN_ROUND);
		}
		for (int i = 0; i < shape.count; ++i) {
			const ExcaliDrawable *d = &shape.items[i];
			const Rgba *f = d->fill_source == EXCALI_FILL_STROKE
			                        ? (has_stroke ? &stroke : NULL)
			                : d->fill_source == EXCALI_FILL_CANVAS
			                        ? canvas
			                        : (has_fill ? &fill : NULL);
			draw_drawable(cr, d, has_stroke ? &stroke : NULL, f);
		}
		if (has_stroke)
			fill_outline(cr, &shape.outline, &stroke);
		excali_shape_free(&shape);
	} else if (e->type == EXCALI_STICKYNOTE) {
		cairo_translate(cr, e->x, e->y);
		excali_draw_sticky(cr, e, has_fill,
		                  (double[]){fill.r, fill.g, fill.b, fill.a},
		                  (double[]){stroke.r, stroke.g, stroke.b, stroke.a});
	} else if (e->type == EXCALI_IMAGE) {
		excali_draw_image(cr, e);
	} else if (e->type == EXCALI_FRAME) {
		excali_draw_frame(cr, e);
	} else {
		excali_draw_text(cr, e, stroke.r, stroke.g, stroke.b, stroke.a);
	}

	if (e->opacity < 100) {
		cairo_pop_group_to_source(cr);
		cairo_paint_with_alpha(cr, fmax(e->opacity, 0) / 100.0);
	}
	cairo_restore(cr);
}

void excali_render_prepare(bool dark)
{
	render_dark = dark;
}

bool excali_render_dark(void)
{
	return render_dark;
}

void excali_draw_element(cairo_t *cr, const ExcaliElement *e)
{
	/* Exports draw on white; see excali_render_prepare for the theme.  */
	static const Rgba white = {1, 1, 1, 1};
	draw_element(cr, e, &white);
}

/* Unrotated scene-space extent of E: its box, or its points.  */
static void element_extent(const ExcaliElement *e, double *x1, double *y1,
                           double *x2, double *y2)
{
	if (e->point_count > 0) {
		*x1 = *x2 = e->x + e->points[0];
		*y1 = *y2 = e->y + e->points[1];
		for (size_t i = 1; i < e->point_count; ++i) {
			*x1 = fmin(*x1, e->x + e->points[2 * i]);
			*y1 = fmin(*y1, e->y + e->points[2 * i + 1]);
			*x2 = fmax(*x2, e->x + e->points[2 * i]);
			*y2 = fmax(*y2, e->y + e->points[2 * i + 1]);
		}
		return;
	}
	*x1 = fmin(e->x, e->x + e->width);
	*y1 = fmin(e->y, e->y + e->height);
	*x2 = fmax(e->x, e->x + e->width);
	*y2 = fmax(e->y, e->y + e->height);
}

/* Conservative scene-space bounds of E, including rough jitter, curve
   overshoot and arrowheads.  */
static void element_bounds(const ExcaliElement *e, double *x1, double *y1,
                           double *x2, double *y2)
{
	double left, top, right, bottom;
	element_extent(e, &left, &top, &right, &bottom);
	double pad = excali_shape_padding(e);
	if (e->angle != 0) {
		/* Point-based elements rotate about the centre of their drawn
		   curve, which lies within PAD of the extent's centre.  */
		double cx = (left + right) / 2, cy = (top + bottom) / 2;
		double r = hypot(right - left, bottom - top) / 2 + 2 * pad;
		left = cx - r, right = cx + r, top = cy - r, bottom = cy + r;
	}
	*x1 = left - pad, *y1 = top - pad, *x2 = right + pad, *y2 = bottom + pad;
}

void excali_element_bounds(const ExcaliElement *e, double *x1, double *y1,
                          double *x2, double *y2)
{
	element_bounds(e, x1, y1, x2, y2);
}

size_t excali_render(uint32_t *pixels, const ExcaliView *view,
                    const ExcaliElement *elements, size_t count)
{
	render_dark = view->dark;
	cairo_surface_t *surface = cairo_image_surface_create_for_data(
	        (unsigned char *)pixels, CAIRO_FORMAT_ARGB32, view->width,
	        view->height, view->width * 4);
	cairo_t *cr = cairo_create(surface);
	if (view->clip_count > 0) {
		/* Antialiased edges crossing the clip may differ from a full
		   render by a few levels; Cairo does not rasterize clipped
		   geometry bit-identically.  */
		for (int i = 0; i < view->clip_count; ++i)
			cairo_rectangle(cr, view->clips[i].x, view->clips[i].y,
			                view->clips[i].width,
			                view->clips[i].height);
		cairo_clip(cr);
	}
	Rgba canvas = {1, 1, 1, 1}, parsed;
	if (parse_color(view->background_color, &parsed)) {
		canvas = parsed;
	} else if (render_dark) {
		double rgb[3] = {1, 1, 1};
		excali_dark_filter(rgb);
		canvas = (Rgba){rgb[0], rgb[1], rgb[2], 1};
	}
	cairo_set_source_rgba(cr, canvas.r, canvas.g, canvas.b, canvas.a);
	cairo_paint(cr);

	double scale = view->pixel_scale * view->zoom;
	cairo_scale(cr, scale, scale);
	cairo_translate(cr, view->scroll_x, view->scroll_y);

	/* Repainted regions in scene coordinates.  */
	ExcaliRect whole = {0, 0, view->width, view->height};
	const ExcaliRect *regions = view->clip_count > 0 ? view->clips : &whole;
	int region_count = view->clip_count > 0 ? view->clip_count : 1;
	double sx1[EXCALI_MAX_CLIPS], sy1[EXCALI_MAX_CLIPS];
	double sx2[EXCALI_MAX_CLIPS], sy2[EXCALI_MAX_CLIPS];
	for (int r = 0; r < region_count; ++r) {
		sx1[r] = regions[r].x / scale - view->scroll_x;
		sy1[r] = regions[r].y / scale - view->scroll_y;
		sx2[r] = (regions[r].x + regions[r].width) / scale -
		         view->scroll_x;
		sy2[r] = (regions[r].y + regions[r].height) / scale -
		         view->scroll_y;
	}
	/* The grid overlay goes below everything.  */
	for (size_t i = 0; i < count; ++i)
		if (elements[i].type == EXCALI_OV_GRID)
			excali_draw_grid(cr, &elements[i], view->zoom,
			                view->pixel_scale, view->dark);
	bool *visible = calloc(count ? count : 1, sizeof *visible);
	size_t drawn = 0;
	excali_frame_begin_pass(elements, count,
	                       &(ExcaliFrameConfig){view->zoom, true, true, true});
	for (size_t i = 0; i < count; ++i) {
		double x1, y1, x2, y2;
		element_bounds(&elements[i], &x1, &y1, &x2, &y2);
		for (int r = 0; r < region_count && !visible[i]; ++r)
			visible[i] = x2 >= sx1[r] && x1 <= sx2[r] &&
			             y2 >= sy1[r] && y1 <= sy2[r];
		if (visible[i]) {
			ExcaliElement scratch;
			const ExcaliElement *e =
			        excali_frame_clip_begin(cr, &elements[i], &scratch);
			draw_element(cr, e, &canvas);
			excali_frame_clip_end(cr);
			++drawn;
		}
	}
	excali_frame_end_pass();
	/* Editor overlays go on top of every element.  */
	for (size_t i = 0; i < count; ++i)
		if (visible[i] && excali_overlay_p(elements[i].type))
			excali_draw_overlay(cr, &elements[i], view->zoom);
	free(visible);

	cairo_destroy(cr);
	cairo_surface_flush(surface);
	cairo_surface_destroy(surface);
	return drawn;
}

void excali_scroll(uint32_t *pixels, int width, int height, int dx, int dy)
{
	if (abs(dx) >= width || abs(dy) >= height || (dx == 0 && dy == 0))
		return;
	int copy_width = width - abs(dx);
	int src_x = dx < 0 ? -dx : 0, dst_x = dx > 0 ? dx : 0;
	size_t bytes = (size_t)copy_width * 4;
	/* Walk rows away from the destination so no source row is
	   overwritten before it is copied.  */
	if (dy > 0)
		for (int y = height - 1; y >= dy; --y)
			memmove(pixels + (size_t)y * width + dst_x,
			        pixels + (size_t)(y - dy) * width + src_x, bytes);
	else
		for (int y = 0; y < height + dy; ++y)
			memmove(pixels + (size_t)y * width + dst_x,
			        pixels + (size_t)(y - dy) * width + src_x, bytes);
}

bool excali_write_png(uint32_t *pixels, int width, int height, const char *path)
{
	cairo_surface_t *surface = cairo_image_surface_create_for_data(
	        (unsigned char *)pixels, CAIRO_FORMAT_ARGB32, width, height,
	        width * 4);
	cairo_status_t status = cairo_surface_write_to_png(surface, path);
	cairo_surface_destroy(surface);
	return status == CAIRO_STATUS_SUCCESS;
}
