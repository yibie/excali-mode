/* excal-overlay.c --- Editor overlays: selection UI  -*- c-file-style: "linux" -*-
 *
 * Elisp computes the selection UI geometry (see excal-select.el) and
 * passes it as overlay pseudo-elements:
 *
 *   ov-rect    x, y, width, height, angle (about the rect center);
 *              strokeColor, backgroundColor (fill, optional),
 *              strokeWidth (px), strokeStyle "solid"/"dashed"/"dotted".
 *   ov-handle  a transform handle square at x, y, width, height.
 *   ov-circle  a circle inscribed in x, y, width, height.
 */

#include "excal-overlay.h"

#include <math.h>
#include <stdlib.h>
#include <string.h>

bool excal_overlay_p(ExcalType type)
{
	return type == EXCAL_OV_RECT || type == EXCAL_OV_HANDLE ||
	       type == EXCAL_OV_CIRCLE;
}

/* Set the source to "#rrggbb" or "#rrggbbaa" S; return false for none.  */
static bool set_color(cairo_t *cr, const char *s)
{
	if (!s || s[0] != '#')
		return false;
	size_t len = strlen(s + 1);
	if (len != 6 && len != 8)
		return false;
	unsigned long v = strtoul(s + 1, NULL, 16);
	double a = 1;
	if (len == 8) {
		a = (v & 0xff) / 255.0;
		v >>= 8;
	}
	cairo_set_source_rgba(cr, ((v >> 16) & 0xff) / 255.0,
	                      ((v >> 8) & 0xff) / 255.0, (v & 0xff) / 255.0, a);
	return a > 0;
}

static void set_line(cairo_t *cr, const ExcalElement *e, double zoom)
{
	cairo_set_line_width(cr, (e->stroke_width > 0 ? e->stroke_width : 1) /
	                                 zoom);
	if (e->stroke_style && strcmp(e->stroke_style, "dashed") == 0) {
		double dash[] = {8 / zoom, 4 / zoom};
		cairo_set_dash(cr, dash, 2, 0);
	} else if (e->stroke_style && strcmp(e->stroke_style, "dotted") == 0) {
		double dash[] = {2 / zoom};
		cairo_set_dash(cr, dash, 1, 0);
	} else {
		cairo_set_dash(cr, NULL, 0, 0);
	}
}

static void rounded_rect(cairo_t *cr, double x, double y, double w, double h,
                         double r)
{
	cairo_new_sub_path(cr);
	cairo_arc(cr, x + w - r, y + r, r, -M_PI / 2, 0);
	cairo_arc(cr, x + w - r, y + h - r, r, 0, M_PI / 2);
	cairo_arc(cr, x + r, y + h - r, r, M_PI / 2, M_PI);
	cairo_arc(cr, x + r, y + r, r, M_PI, 3 * M_PI / 2);
	cairo_close_path(cr);
}

void excal_draw_overlay(cairo_t *cr, const ExcalElement *e, double zoom)
{
	double x = e->x, y = e->y, w = e->width, h = e->height;
	cairo_save(cr);
	cairo_new_path(cr);
	if (e->angle != 0) {
		cairo_translate(cr, x + w / 2, y + h / 2);
		cairo_rotate(cr, e->angle);
		cairo_translate(cr, -(x + w / 2), -(y + h / 2));
	}
	set_line(cr, e, zoom);
	switch (e->type) {
	case EXCAL_OV_RECT:
		cairo_rectangle(cr, x, y, w, h);
		break;
	case EXCAL_OV_HANDLE:
		rounded_rect(cr, x, y, w, h, fmin(2 / zoom, fmin(w, h) / 2));
		break;
	case EXCAL_OV_CIRCLE:
		cairo_arc(cr, x + w / 2, y + h / 2, fmin(w, h) / 2, 0, 2 * M_PI);
		break;
	default:
		break;
	}
	if (set_color(cr, e->background_color))
		cairo_fill_preserve(cr);
	if (set_color(cr, e->stroke_color))
		cairo_stroke(cr);
	cairo_new_path(cr);
	cairo_restore(cr);
}
