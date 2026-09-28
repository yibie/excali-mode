/* excal-render.c --- Excalidraw scene rasterizer for Emacs Canvas  -*- c-file-style: "linux" -*-
 *
 * Hand-drawn strokes follow the roughjs algorithms (line bowing, ellipse
 * point jitter, Catmull-Rom curve fitting) closely enough to look like
 * Excalidraw, but they are not bit-identical to the web renderer.
 */

#include "excal-render.h"

#include <cairo.h>
#include <math.h>
#include <pango/pangocairo.h>
#include <stdlib.h>
#include <string.h>

/* M_PI is POSIX, not C11; glibc and MinGW hide it under -std=c11.  */
#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

/* Seeded PRNG matching roughjs `Random.next'.  */
typedef struct {
	int32_t seed;
} Rng;

static double rng_next(Rng *rng)
{
	rng->seed = (int32_t)((uint32_t)48271u * (uint32_t)rng->seed);
	return (double)(rng->seed & 0x7fffffff) / 2147483648.0;
}

typedef struct {
	Rng rng;
	double roughness;
	double bowing;
	double max_offset;
	double curve_fitting;
} Rough;

static double rough_offset(Rough *r, double min, double max, double gain)
{
	return r->roughness * gain * (rng_next(&r->rng) * (max - min) + min);
}

static double rough_offset_opt(Rough *r, double x, double gain)
{
	return rough_offset(r, -x, x, gain);
}

/* roughjs `_line'.  */
static void rough_line_once(cairo_t *cr, Rough *r, double x1, double y1,
                            double x2, double y2, bool overlay)
{
	double length_sq = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2);
	double length = sqrt(length_sq);
	double gain = length < 200 ? 1.0
	              : length > 500 ? 0.4
	                             : -0.0016668 * length + 1.233334;
	double offset = r->max_offset;
	if (offset * offset * 100 > length_sq)
		offset = length / 10;
	double random = overlay ? offset / 2 : offset;
	double diverge = 0.2 + rng_next(&r->rng) * 0.2;
	double mid_x = r->bowing * r->max_offset * (y2 - y1) / 200;
	double mid_y = r->bowing * r->max_offset * (x1 - x2) / 200;
	mid_x = rough_offset_opt(r, mid_x, gain);
	mid_y = rough_offset_opt(r, mid_y, gain);

	cairo_move_to(cr, x1 + rough_offset_opt(r, random, gain),
	              y1 + rough_offset_opt(r, random, gain));
	double c1x = mid_x + x1 + (x2 - x1) * diverge +
	             rough_offset_opt(r, random, gain);
	double c1y = mid_y + y1 + (y2 - y1) * diverge +
	             rough_offset_opt(r, random, gain);
	double c2x = mid_x + x1 + 2 * (x2 - x1) * diverge +
	             rough_offset_opt(r, random, gain);
	double c2y = mid_y + y1 + 2 * (y2 - y1) * diverge +
	             rough_offset_opt(r, random, gain);
	double ex = x2 + rough_offset_opt(r, random, gain);
	double ey = y2 + rough_offset_opt(r, random, gain);
	cairo_curve_to(cr, c1x, c1y, c2x, c2y, ex, ey);
}

static void rough_line(cairo_t *cr, Rough *r, double x1, double y1, double x2,
                       double y2, bool multi)
{
	if (r->roughness <= 0.0) {
		cairo_move_to(cr, x1, y1);
		cairo_line_to(cr, x2, y2);
		return;
	}
	rough_line_once(cr, r, x1, y1, x2, y2, false);
	if (multi)
		rough_line_once(cr, r, x1, y1, x2, y2, true);
}

/* roughjs `_curve': Catmull-Rom through POINTS (flat xy array).  */
static void rough_curve(cairo_t *cr, const double *p, size_t n)
{
	if (n < 2)
		return;
	if (n < 4) {
		cairo_move_to(cr, p[0], p[1]);
		for (size_t i = 1; i < n; ++i)
			cairo_line_to(cr, p[2 * i], p[2 * i + 1]);
		return;
	}
	const double s = 1.0; /* 1 - curveTightness */
	cairo_move_to(cr, p[2], p[3]);
	for (size_t i = 1; i + 2 < n; ++i) {
		const double *prev = p + 2 * (i - 1);
		const double *cur = p + 2 * i;
		const double *next = p + 2 * (i + 1);
		const double *after = p + 2 * (i + 2);
		cairo_curve_to(cr, cur[0] + (s * next[0] - s * prev[0]) / 6,
		               cur[1] + (s * next[1] - s * prev[1]) / 6,
		               next[0] + (s * cur[0] - s * after[0]) / 6,
		               next[1] + (s * cur[1] - s * after[1]) / 6, next[0],
		               next[1]);
	}
}

/* roughjs `_computeEllipsePoints'; returns a malloc'ed flat array.  */
static double *ellipse_points(Rough *r, double increment, double cx, double cy,
                              double rx, double ry, double offset,
                              double overlap, size_t *count)
{
	double rad_offset = r->roughness == 0
	                            ? 0
	                            : rough_offset_opt(r, 0.5, 1) - M_PI / 2;
	size_t capacity = (size_t)(2 * M_PI / increment) + 8;
	double *pts = malloc(sizeof(double) * 2 * capacity);
	size_t n = 0;
#define PUSH(X, Y)                           \
	do {                                 \
		if (n < capacity) {          \
			pts[2 * n] = (X);    \
			pts[2 * n + 1] = (Y); \
			++n;                 \
		}                            \
	} while (0)
	PUSH(rough_offset_opt(r, offset, 1) + cx +
	             0.9 * rx * cos(rad_offset - increment),
	     rough_offset_opt(r, offset, 1) + cy +
	             0.9 * ry * sin(rad_offset - increment));
	double end_angle = 2 * M_PI + rad_offset - 0.01;
	for (double a = rad_offset; a < end_angle; a += increment)
		PUSH(rough_offset_opt(r, offset, 1) + cx + rx * cos(a),
		     rough_offset_opt(r, offset, 1) + cy + ry * sin(a));
	PUSH(rough_offset_opt(r, offset, 1) + cx +
	             rx * cos(rad_offset + 2 * M_PI + overlap * 0.5),
	     rough_offset_opt(r, offset, 1) + cy +
	             ry * sin(rad_offset + 2 * M_PI + overlap * 0.5));
	PUSH(rough_offset_opt(r, offset, 1) + cx +
	             0.98 * rx * cos(rad_offset + overlap),
	     rough_offset_opt(r, offset, 1) + cy +
	             0.98 * ry * sin(rad_offset + overlap));
	PUSH(rough_offset_opt(r, offset, 1) + cx +
	             0.9 * rx * cos(rad_offset + overlap * 0.5),
	     rough_offset_opt(r, offset, 1) + cy +
	             0.9 * ry * sin(rad_offset + overlap * 0.5));
#undef PUSH
	*count = n;
	return pts;
}

static void rough_ellipse(cairo_t *cr, Rough *r, double cx, double cy,
                          double width, double height, bool multi)
{
	if (r->roughness <= 0.0) {
		cairo_save(cr);
		cairo_translate(cr, cx, cy);
		cairo_scale(cr, fmax(width / 2, 0.01), fmax(height / 2, 0.01));
		cairo_new_sub_path(cr);
		cairo_arc(cr, 0, 0, 1, 0, 2 * M_PI);
		cairo_restore(cr);
		return;
	}
	const double curve_step_count = 9;
	double psq = sqrt(M_PI * 2 *
	                  sqrt((pow(width / 2, 2) + pow(height / 2, 2)) / 2));
	double steps = ceil(fmax(curve_step_count,
	                         (curve_step_count / sqrt(200)) * psq));
	double increment = 2 * M_PI / steps;
	double rx = fabs(width / 2), ry = fabs(height / 2);
	double fit = 1 - r->curve_fitting;
	rx += rough_offset_opt(r, rx * fit, 1);
	ry += rough_offset_opt(r, ry * fit, 1);

	size_t n;
	double overlap = increment *
	                 rough_offset(r, 0.1, rough_offset(r, 0.4, 1, 1), 1);
	double *p = ellipse_points(r, increment, cx, cy, rx, ry, 1, overlap, &n);
	rough_curve(cr, p, n);
	free(p);
	if (multi) {
		p = ellipse_points(r, increment, cx, cy, rx, ry, 1.5, 0, &n);
		rough_curve(cr, p, n);
		free(p);
	}
}

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

static bool parse_color(const char *s, Rgba *out)
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

/* Text.  */

static const char *font_family_name(int family)
{
	switch (family) {
	case 1:
		return "Virgil, Xiaolai, PingFang SC";
	case 2:
		return "Helvetica, PingFang SC";
	case 3:
		return "Cascadia Code, Menlo, PingFang SC";
	case 6:
		return "Nunito, PingFang SC";
	case 7:
		return "Lilita One, PingFang SC";
	case 8:
		return "Comic Shanns, PingFang SC";
	default:
		return "Excalifont, Xiaolai, Virgil, PingFang SC";
	}
}

static PangoLayout *text_layout(cairo_t *cr, const char *text,
                                double font_size, int family,
                                double line_height)
{
	PangoLayout *layout = pango_cairo_create_layout(cr);
	/* Layout must not depend on zoom, so disable metric hinting.  */
	cairo_font_options_t *options = cairo_font_options_create();
	cairo_font_options_set_hint_metrics(options, CAIRO_HINT_METRICS_OFF);
	cairo_font_options_set_hint_style(options, CAIRO_HINT_STYLE_NONE);
	pango_cairo_context_set_font_options(pango_layout_get_context(layout),
	                                     options);
	cairo_font_options_destroy(options);
	pango_layout_context_changed(layout);

	PangoFontDescription *desc =
	        pango_font_description_from_string(font_family_name(family));
	pango_font_description_set_absolute_size(desc, font_size * PANGO_SCALE);
	pango_layout_set_font_description(layout, desc);
	pango_font_description_free(desc);

	PangoAttrList *attrs = pango_attr_list_new();
	pango_attr_list_insert(attrs, pango_attr_line_height_new_absolute((
	                                      int)(font_size * line_height *
	                                           PANGO_SCALE)));
	pango_layout_set_attributes(layout, attrs);
	pango_attr_list_unref(attrs);
	pango_layout_set_text(layout, text, -1);
	return layout;
}

void excal_measure_text(const char *text, double font_size, int font_family,
                        double line_height, double *width, double *height)
{
	cairo_surface_t *surface =
	        cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 1, 1);
	cairo_t *cr = cairo_create(surface);
	PangoLayout *layout =
	        text_layout(cr, text, font_size, font_family, line_height);
	PangoRectangle logical;
	pango_layout_get_extents(layout, NULL, &logical);
	*width = (double)logical.width / PANGO_SCALE;
	*height = (double)logical.height / PANGO_SCALE;
	g_object_unref(layout);
	cairo_destroy(cr);
	cairo_surface_destroy(surface);
}

static void draw_text(cairo_t *cr, const ExcalElement *e, const Rgba *stroke)
{
	if (!e->text || !*e->text)
		return;
	PangoLayout *layout = text_layout(cr, e->text, e->font_size,
	                                  e->font_family, e->line_height);
	if (e->text_align && strcmp(e->text_align, "left") != 0) {
		pango_layout_set_width(layout, (int)(e->width * PANGO_SCALE));
		pango_layout_set_alignment(layout,
		                           strcmp(e->text_align, "center") == 0
		                                   ? PANGO_ALIGN_CENTER
		                                   : PANGO_ALIGN_RIGHT);
	}
	cairo_set_source_rgba(cr, stroke->r, stroke->g, stroke->b, stroke->a);
	cairo_move_to(cr, e->x, e->y);
	pango_cairo_show_layout(cr, layout);
	g_object_unref(layout);
}

/* Shapes.  */

static void clean_shape_path(cairo_t *cr, const ExcalElement *e)
{
	double x = e->x, y = e->y, w = e->width, h = e->height;
	cairo_new_path(cr);
	switch (e->type) {
	case EXCAL_RECTANGLE:
		if (e->rounded) {
			double r = fmin(fmin(fabs(w), fabs(h)) * 0.25, 32);
			cairo_new_sub_path(cr);
			cairo_arc(cr, x + w - r, y + r, r, -M_PI / 2, 0);
			cairo_arc(cr, x + w - r, y + h - r, r, 0, M_PI / 2);
			cairo_arc(cr, x + r, y + h - r, r, M_PI / 2, M_PI);
			cairo_arc(cr, x + r, y + r, r, M_PI, 3 * M_PI / 2);
			cairo_close_path(cr);
		} else {
			cairo_rectangle(cr, x, y, w, h);
		}
		break;
	case EXCAL_ELLIPSE:
		cairo_save(cr);
		cairo_translate(cr, x + w / 2, y + h / 2);
		cairo_scale(cr, fmax(w / 2, 0.01), fmax(h / 2, 0.01));
		cairo_arc(cr, 0, 0, 1, 0, 2 * M_PI);
		cairo_restore(cr);
		break;
	case EXCAL_DIAMOND:
		cairo_move_to(cr, x + w / 2, y);
		cairo_line_to(cr, x + w, y + h / 2);
		cairo_line_to(cr, x + w / 2, y + h);
		cairo_line_to(cr, x, y + h / 2);
		cairo_close_path(cr);
		break;
	default:
		break;
	}
}

static void fill_shape(cairo_t *cr, Rough *r, const ExcalElement *e,
                       const Rgba *fill)
{
	if (!fill)
		return;
	cairo_save(cr);
	cairo_set_source_rgba(cr, fill->r, fill->g, fill->b, fill->a);
	const char *style = e->fill_style ? e->fill_style : "hachure";
	if (strcmp(style, "solid") == 0) {
		clean_shape_path(cr, e);
		cairo_fill(cr);
		cairo_restore(cr);
		return;
	}
	/* Hachure: rough parallel strokes clipped to the shape.  */
	clean_shape_path(cr, e);
	cairo_clip(cr);
	cairo_new_path(cr);
	double gap = fmax(e->stroke_width * 4, 4);
	cairo_set_line_width(cr, fmax(e->stroke_width / 2, 0.5));
	double cx = e->x + e->width / 2, cy = e->y + e->height / 2;
	double radius = hypot(e->width, e->height) / 2 + gap;
	int passes = strcmp(style, "cross-hatch") == 0 ? 2 : 1;
	for (int pass = 0; pass < passes; ++pass) {
		double angle = (pass == 0 ? -41.0 : 49.0) * M_PI / 180 + M_PI / 2;
		double dx = cos(angle), dy = sin(angle);
		for (double t = -radius; t <= radius; t += gap) {
			double px = cx - dy * t, py = cy + dx * t;
			rough_line(cr, r, px - dx * radius, py - dy * radius,
			           px + dx * radius, py + dy * radius, false);
		}
	}
	cairo_stroke(cr);
	cairo_restore(cr);
}

static void stroke_rectangle(cairo_t *cr, Rough *r, const ExcalElement *e,
                             bool multi)
{
	double x = e->x, y = e->y, w = e->width, h = e->height;
	if (!e->rounded) {
		rough_line(cr, r, x, y, x + w, y, multi);
		rough_line(cr, r, x + w, y, x + w, y + h, multi);
		rough_line(cr, r, x + w, y + h, x, y + h, multi);
		rough_line(cr, r, x, y + h, x, y, multi);
		return;
	}
	double rad = fmin(fmin(fabs(w), fabs(h)) * 0.25, 32);
	rough_line(cr, r, x + rad, y, x + w - rad, y, multi);
	rough_line(cr, r, x + w, y + rad, x + w, y + h - rad, multi);
	rough_line(cr, r, x + w - rad, y + h, x + rad, y + h, multi);
	rough_line(cr, r, x, y + h - rad, x, y + rad, multi);
	double k = 0.55;
	double corners[4][6] = {
	        {x + w - rad, y, x + w, y, x + w, y + rad},
	        {x + w, y + h - rad, x + w, y + h, x + w - rad, y + h},
	        {x + rad, y + h, x, y + h, x, y + h - rad},
	        {x, y + rad, x, y, x + rad, y},
	};
	for (int i = 0; i < 4; ++i) {
		double *c = corners[i];
		cairo_move_to(cr, c[0], c[1]);
		cairo_curve_to(cr, c[0] + (c[2] - c[0]) * k,
		               c[1] + (c[3] - c[1]) * k,
		               c[4] + (c[2] - c[4]) * k,
		               c[5] + (c[3] - c[5]) * k, c[4], c[5]);
	}
}

static void stroke_linear(cairo_t *cr, Rough *r, const ExcalElement *e,
                          bool multi)
{
	size_t n = e->point_count;
	if (n < 2)
		return;
	double *abs_pts = malloc(sizeof(double) * 2 * (n + 2));
	for (size_t i = 0; i < n; ++i) {
		abs_pts[2 * (i + 1)] = e->x + e->points[2 * i];
		abs_pts[2 * (i + 1) + 1] = e->y + e->points[2 * i + 1];
	}
	if (e->rounded && n > 2) {
		/* Duplicate the endpoints so the curve passes through them.  */
		abs_pts[0] = abs_pts[2];
		abs_pts[1] = abs_pts[3];
		abs_pts[2 * (n + 1)] = abs_pts[2 * n];
		abs_pts[2 * (n + 1) + 1] = abs_pts[2 * n + 1];
		rough_curve(cr, abs_pts, n + 2);
	} else {
		for (size_t i = 1; i < n; ++i)
			rough_line(cr, r, abs_pts[2 * i], abs_pts[2 * i + 1],
			           abs_pts[2 * i + 2], abs_pts[2 * i + 3],
			           multi);
	}
	free(abs_pts);
}

/* Draw arrowhead KIND with its tip at TX,TY, pointing away from FX,FY.
   Sizes follow Excalidraw's getArrowheadSize/getArrowheadAngle.  */
static void draw_arrowhead(cairo_t *cr, Rough *r, const ExcalElement *e,
                           const char *kind, double tx, double ty, double fx,
                           double fy, const Rgba *stroke, bool multi)
{
	double seg = hypot(tx - fx, ty - fy);
	if (!kind || seg < 0.01)
		return;
	bool diamond = strncmp(kind, "diamond", 7) == 0;
	bool outline = strstr(kind, "_outline") != NULL;
	double size = strcmp(kind, "arrow") == 0 ? 25 : diamond ? 12 : 15;
	size = fmin(size, seg * (diamond ? 0.25 : 0.5));
	double ux = (tx - fx) / seg, uy = (ty - fy) / seg;
	double px = -uy, py = ux; /* Perpendicular.  */

	cairo_save(cr);
	cairo_set_dash(cr, NULL, 0, 0);
	cairo_new_path(cr);
	if (strcmp(kind, "bar") == 0) {
		rough_line(cr, r, tx + px * size, ty + py * size,
		           tx - px * size, ty - py * size, multi);
		cairo_stroke(cr);
	} else if (strncmp(kind, "triangle", 8) == 0 ||
	           strncmp(kind, "circle", 6) == 0 || diamond) {
		if (strncmp(kind, "circle", 6) == 0) {
			cairo_arc(cr, tx - ux * size / 2, ty - uy * size / 2,
			          size / 2, 0, 2 * M_PI);
		} else if (diamond) {
			double bx = tx - ux * size * 2, by = ty - uy * size * 2;
			double mx = tx - ux * size, my = ty - uy * size;
			cairo_move_to(cr, tx, ty);
			cairo_line_to(cr, mx + px * size / 2, my + py * size / 2);
			cairo_line_to(cr, bx, by);
			cairo_line_to(cr, mx - px * size / 2, my - py * size / 2);
			cairo_close_path(cr);
		} else {
			double a = 25 * M_PI / 180;
			double ca = cos(a), sa = sin(a);
			cairo_move_to(cr, tx, ty);
			cairo_line_to(cr, tx - size * (ux * ca - px * sa),
			              ty - size * (uy * ca - py * sa));
			cairo_line_to(cr, tx - size * (ux * ca + px * sa),
			              ty - size * (uy * ca + py * sa));
			cairo_close_path(cr);
		}
		/* Outlines are filled with the canvas so the line stays hidden.  */
		if (outline)
			cairo_set_source_rgb(cr, 1, 1, 1);
		cairo_fill_preserve(cr);
		cairo_set_source_rgba(cr, stroke->r, stroke->g, stroke->b,
		                      stroke->a);
		cairo_stroke(cr);
	} else {
		/* "arrow", and a fallback for kinds not drawn yet.  */
		double a = 20 * M_PI / 180;
		double ca = cos(a), sa = sin(a);
		for (int side = -1; side <= 1; side += 2)
			rough_line(cr, r,
			           tx - size * (ux * ca + side * px * sa),
			           ty - size * (uy * ca + side * py * sa), tx, ty,
			           multi);
		cairo_stroke(cr);
	}
	cairo_restore(cr);
}

static void draw_arrowheads(cairo_t *cr, Rough *r, const ExcalElement *e,
                            const Rgba *stroke, bool multi)
{
	size_t n = e->point_count;
	if (n < 2)
		return;
	const double *p = e->points;
	draw_arrowhead(cr, r, e, e->end_arrowhead, e->x + p[2 * n - 2],
	               e->y + p[2 * n - 1], e->x + p[2 * n - 4],
	               e->y + p[2 * n - 3], stroke, multi);
	draw_arrowhead(cr, r, e, e->start_arrowhead, e->x + p[0], e->y + p[1],
	               e->x + p[2], e->y + p[3], stroke, multi);
}

static void stroke_freedraw(cairo_t *cr, const ExcalElement *e)
{
	size_t n = e->point_count;
	if (n == 0)
		return;
	cairo_save(cr);
	cairo_set_line_width(cr, e->stroke_width * 2.5);
	cairo_new_path(cr);
	if (n == 1) {
		cairo_arc(cr, e->x + e->points[0], e->y + e->points[1],
		          e->stroke_width * 1.25, 0, 2 * M_PI);
		cairo_fill(cr);
		cairo_restore(cr);
		return;
	}
	/* Quadratic midpoint smoothing; perfect-freehand is not ported.  */
	cairo_move_to(cr, e->x + e->points[0], e->y + e->points[1]);
	for (size_t i = 1; i + 1 < n; ++i) {
		double x0 = e->x + e->points[2 * i];
		double y0 = e->y + e->points[2 * i + 1];
		double x1 = e->x + e->points[2 * i + 2];
		double y1 = e->y + e->points[2 * i + 3];
		double mx = (x0 + x1) / 2, my = (y0 + y1) / 2;
		double cx, cy;
		cairo_get_current_point(cr, &cx, &cy);
		cairo_curve_to(cr, cx + 2.0 / 3 * (x0 - cx),
		               cy + 2.0 / 3 * (y0 - cy),
		               mx + 2.0 / 3 * (x0 - mx),
		               my + 2.0 / 3 * (y0 - my), mx, my);
	}
	cairo_line_to(cr, e->x + e->points[2 * n - 2],
	              e->y + e->points[2 * n - 1]);
	cairo_stroke(cr);
	cairo_restore(cr);
}

static void apply_stroke_style(cairo_t *cr, const ExcalElement *e)
{
	cairo_set_line_width(cr, e->stroke_width);
	cairo_set_line_cap(cr, CAIRO_LINE_CAP_ROUND);
	cairo_set_line_join(cr, CAIRO_LINE_JOIN_ROUND);
	if (e->stroke_style && strcmp(e->stroke_style, "dashed") == 0) {
		double dash[] = {8, 8 + e->stroke_width};
		cairo_set_dash(cr, dash, 2, 0);
	} else if (e->stroke_style &&
	           strcmp(e->stroke_style, "dotted") == 0) {
		double dash[] = {1.5, 6 + e->stroke_width};
		cairo_set_dash(cr, dash, 2, 0);
	} else {
		cairo_set_dash(cr, NULL, 0, 0);
	}
}

static void draw_element(cairo_t *cr, const ExcalElement *e)
{
	Rough rough = {
	        .rng = {.seed = e->seed ? e->seed : 1},
	        .roughness = e->roughness,
	        .bowing = 1,
	        .max_offset = 2,
	        .curve_fitting = 0.95,
	};
	Rgba stroke = {0.118, 0.118, 0.118, 1};
	bool has_stroke = parse_color(e->stroke_color, &stroke);
	Rgba fill;
	bool has_fill = parse_color(e->background_color, &fill);
	bool multi = !e->stroke_style || strcmp(e->stroke_style, "solid") == 0;

	cairo_save(cr);
	if (e->angle != 0) {
		double cx = e->x + e->width / 2, cy = e->y + e->height / 2;
		cairo_translate(cr, cx, cy);
		cairo_rotate(cr, e->angle);
		cairo_translate(cr, -cx, -cy);
	}
	if (e->opacity < 100)
		cairo_push_group(cr);

	switch (e->type) {
	case EXCAL_RECTANGLE:
	case EXCAL_ELLIPSE:
	case EXCAL_DIAMOND:
		fill_shape(cr, &rough, e, has_fill ? &fill : NULL);
		if (has_stroke) {
			cairo_new_path(cr);
			apply_stroke_style(cr, e);
			cairo_set_source_rgba(cr, stroke.r, stroke.g, stroke.b,
			                      stroke.a);
			if (e->type == EXCAL_RECTANGLE) {
				stroke_rectangle(cr, &rough, e, multi);
			} else if (e->type == EXCAL_ELLIPSE) {
				rough_ellipse(cr, &rough, e->x + e->width / 2,
				              e->y + e->height / 2, e->width,
				              e->height, multi);
			} else {
				double x = e->x, y = e->y, w = e->width,
				       h = e->height;
				rough_line(cr, &rough, x + w / 2, y, x + w,
				           y + h / 2, multi);
				rough_line(cr, &rough, x + w, y + h / 2,
				           x + w / 2, y + h, multi);
				rough_line(cr, &rough, x + w / 2, y + h, x,
				           y + h / 2, multi);
				rough_line(cr, &rough, x, y + h / 2, x + w / 2,
				           y, multi);
			}
			cairo_stroke(cr);
		}
		break;
	case EXCAL_LINE:
	case EXCAL_ARROW:
		if (has_stroke) {
			cairo_new_path(cr);
			apply_stroke_style(cr, e);
			cairo_set_source_rgba(cr, stroke.r, stroke.g, stroke.b,
			                      stroke.a);
			stroke_linear(cr, &rough, e, multi);
			cairo_stroke(cr);
			draw_arrowheads(cr, &rough, e, &stroke, multi);
		}
		break;
	case EXCAL_FREEDRAW:
		if (has_stroke) {
			apply_stroke_style(cr, e);
			cairo_set_source_rgba(cr, stroke.r, stroke.g, stroke.b,
			                      stroke.a);
			stroke_freedraw(cr, e);
		}
		break;
	case EXCAL_TEXT:
		draw_text(cr, e, &stroke);
		break;
	default:
		break;
	}

	if (e->opacity < 100) {
		cairo_pop_group_to_source(cr);
		cairo_paint_with_alpha(cr, fmax(e->opacity, 0) / 100.0);
	}
	cairo_restore(cr);
}

/* Unrotated scene-space extent of E: its box, or its points.  */
static void element_extent(const ExcalElement *e, double *x1, double *y1,
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

/* Dashed box plus resize handles.  Keep the handle layout in sync with
   `excal--handles'.  */
static void draw_selection(cairo_t *cr, const ExcalElement *e, double zoom,
                           bool with_handles)
{
	double pad = 6 / zoom;
	double x1, y1, x2, y2;
	element_extent(e, &x1, &y1, &x2, &y2);
	double x = x1 - pad, y = y1 - pad;
	double w = x2 - x1 + 2 * pad, h = y2 - y1 + 2 * pad;
	cairo_save(cr);
	if (e->angle != 0) {
		double cx = (x1 + x2) / 2, cy = (y1 + y2) / 2;
		cairo_translate(cr, cx, cy);
		cairo_rotate(cr, e->angle);
		cairo_translate(cr, -cx, -cy);
	}
	cairo_set_source_rgb(cr, 0.41, 0.40, 0.87);
	cairo_set_line_width(cr, 1 / zoom);
	double dash[] = {4 / zoom, 4 / zoom};
	cairo_set_dash(cr, dash, 2, 0);
	cairo_rectangle(cr, x, y, w, h);
	cairo_stroke(cr);
	cairo_set_dash(cr, NULL, 0, 0);
	/* Resizing rotated elements is not supported yet.  */
	if (with_handles && e->angle == 0) {
		double hs = 8 / zoom;
		double hx[] = {x, x + w, x, x + w, x + w / 2, x + w / 2, x, x + w};
		double hy[] = {y, y, y + h, y + h, y, y + h, y + h / 2, y + h / 2};
		/* Text scales uniformly, so it only gets corner handles.  */
		int handles = e->type == EXCAL_TEXT ? 4 : 8;
		for (int i = 0; i < handles; ++i) {
			cairo_rectangle(cr, hx[i] - hs / 2, hy[i] - hs / 2, hs,
			                hs);
			cairo_set_source_rgb(cr, 1, 1, 1);
			cairo_fill_preserve(cr);
			cairo_set_source_rgb(cr, 0.41, 0.40, 0.87);
			cairo_stroke(cr);
		}
	}
	cairo_restore(cr);
}

static void draw_marquee(cairo_t *cr, const ExcalElement *e, double zoom)
{
	double x1, y1, x2, y2;
	element_extent(e, &x1, &y1, &x2, &y2);
	cairo_save(cr);
	cairo_rectangle(cr, x1, y1, x2 - x1, y2 - y1);
	cairo_set_source_rgba(cr, 0.41, 0.40, 0.87, 0.08);
	cairo_fill_preserve(cr);
	cairo_set_source_rgb(cr, 0.41, 0.40, 0.87);
	cairo_set_line_width(cr, 1 / zoom);
	cairo_stroke(cr);
	cairo_restore(cr);
}

/* Conservative scene-space bounds of E, including rough jitter.  */
static void element_bounds(const ExcalElement *e, double *x1, double *y1,
                           double *x2, double *y2)
{
	double left, top, right, bottom;
	element_extent(e, &left, &top, &right, &bottom);
	if (e->angle != 0) {
		double cx = (left + right) / 2, cy = (top + bottom) / 2;
		double r = hypot(right - left, bottom - top) / 2;
		left = cx - r, right = cx + r, top = cy - r, bottom = cy + r;
	}
	/* Arrowheads, stroke width, roughness offsets, selection handles.  */
	double pad = 30 + e->stroke_width * 2 + e->roughness * 6;
	*x1 = left - pad, *y1 = top - pad, *x2 = right + pad, *y2 = bottom + pad;
}

size_t excal_render(uint32_t *pixels, const ExcalView *view,
                    const ExcalElement *elements, size_t count)
{
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
	cairo_set_source_rgb(cr, 1, 1, 1);
	cairo_paint(cr);

	double scale = view->pixel_scale * view->zoom;
	cairo_scale(cr, scale, scale);
	cairo_translate(cr, view->scroll_x, view->scroll_y);

	/* Repainted regions in scene coordinates.  */
	ExcalRect whole = {0, 0, view->width, view->height};
	const ExcalRect *regions = view->clip_count > 0 ? view->clips : &whole;
	int region_count = view->clip_count > 0 ? view->clip_count : 1;
	double sx1[EXCAL_MAX_CLIPS], sy1[EXCAL_MAX_CLIPS];
	double sx2[EXCAL_MAX_CLIPS], sy2[EXCAL_MAX_CLIPS];
	for (int r = 0; r < region_count; ++r) {
		sx1[r] = regions[r].x / scale - view->scroll_x;
		sy1[r] = regions[r].y / scale - view->scroll_y;
		sx2[r] = (regions[r].x + regions[r].width) / scale -
		         view->scroll_x;
		sy2[r] = (regions[r].y + regions[r].height) / scale -
		         view->scroll_y;
	}
	bool *visible = calloc(count ? count : 1, sizeof *visible);
	size_t drawn = 0;
	for (size_t i = 0; i < count; ++i) {
		double x1, y1, x2, y2;
		element_bounds(&elements[i], &x1, &y1, &x2, &y2);
		for (int r = 0; r < region_count && !visible[i]; ++r)
			visible[i] = x2 >= sx1[r] && x1 <= sx2[r] &&
			             y2 >= sy1[r] && y1 <= sy2[r];
		if (visible[i]) {
			draw_element(cr, &elements[i]);
			++drawn;
		}
	}
	for (size_t i = 0; i < count; ++i)
		if (!visible[i])
			continue;
		else if (elements[i].type == EXCAL_MARQUEE)
			draw_marquee(cr, &elements[i], view->zoom);
		else if (elements[i].type == EXCAL_SELECTION)
			draw_selection(cr, &elements[i], view->zoom, true);
		else if (elements[i].selection > 0)
			draw_selection(cr, &elements[i], view->zoom,
			               elements[i].selection == 2);
	free(visible);

	cairo_destroy(cr);
	cairo_surface_flush(surface);
	cairo_surface_destroy(surface);
	return drawn;
}

void excal_scroll(uint32_t *pixels, int width, int height, int dx, int dy)
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

bool excal_write_png(uint32_t *pixels, int width, int height, const char *path)
{
	cairo_surface_t *surface = cairo_image_surface_create_for_data(
	        (unsigned char *)pixels, CAIRO_FORMAT_ARGB32, width, height,
	        width * 4);
	cairo_status_t status = cairo_surface_write_to_png(surface, path);
	cairo_surface_destroy(surface);
	return status == CAIRO_STATUS_SUCCESS;
}
