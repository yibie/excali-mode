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
 *   ov-ellipse, ov-diamond
 *              outlines inscribed in the rotated rect, like ov-rect.
 *   ov-poly    a polyline through `points' (relative to x, y).
 *   ov-grid    the background grid over x, y, width, height, with the
 *              grid size in strokeWidth and the bold-line step in fontSize.
 */

#include "excal-overlay.h"

#include <math.h>
#include <stdlib.h>
#include <string.h>

bool excal_overlay_p(ExcalType type)
{
	return type == EXCAL_OV_RECT || type == EXCAL_OV_HANDLE ||
	       type == EXCAL_OV_CIRCLE || type == EXCAL_OV_ELLIPSE ||
	       type == EXCAL_OV_DIAMOND || type == EXCAL_OV_POLY ||
	       type == EXCAL_OV_GRID;
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
	if (e->type == EXCAL_OV_GRID)
		return; /* Drawn below the elements by excal_draw_grid.  */
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
	case EXCAL_OV_ELLIPSE:
		cairo_save(cr);
		cairo_translate(cr, x + w / 2, y + h / 2);
		cairo_scale(cr, fmax(w / 2, 0.01), fmax(h / 2, 0.01));
		cairo_arc(cr, 0, 0, 1, 0, 2 * M_PI);
		cairo_restore(cr);
		break;
	case EXCAL_OV_POLY:
		for (size_t i = 0; i < e->point_count; ++i) {
			double px = x + e->points[2 * i], py = y + e->points[2 * i + 1];
			if (i == 0)
				cairo_move_to(cr, px, py);
			else
				cairo_line_to(cr, px, py);
		}
		break;
	case EXCAL_OV_DIAMOND:
		cairo_move_to(cr, x + w / 2, y);
		cairo_line_to(cr, x + w, y + h / 2);
		cairo_line_to(cr, x + w / 2, y + h);
		cairo_line_to(cr, x, y + h / 2);
		cairo_close_path(cr);
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

/* Upstream strokeGrid: lines at multiples of the grid size, every STEP-th
   one solid #dddddd, the rest dashed #e5e5e5 and hidden when closer than
   10 screen px; all one device pixel wide and pixel-aligned.  */
void excal_draw_grid(cairo_t *cr, const ExcalElement *e, double zoom,
                     double pixel_scale, bool dark)
{
	double bold_rgb[3] = {0xdd / 255.0, 0xdd / 255.0, 0xdd / 255.0};
	double minor_rgb[3] = {0xe5 / 255.0, 0xe5 / 255.0, 0xe5 / 255.0};
	if (dark) {
		excal_dark_filter(bold_rgb);
		excal_dark_filter(minor_rgb);
	}
	double size = e->stroke_width;
	int step = (int)e->font_size;
	if (size < 1)
		return;
	double device = zoom * pixel_scale; /* Device pixels per scene unit.  */
	double width = 1 / device;
	bool minor = size * zoom >= 10;
	double x1 = floor(e->x / size) * size, y1 = floor(e->y / size) * size;
	double x2 = e->x + e->width, y2 = e->y + e->height;
	cairo_save(cr);
	cairo_set_line_width(cr, width);
	for (int axis = 0; axis < 2; ++axis) {
		double from = axis ? y1 : x1, to = axis ? y2 : x2;
		for (double v = from; v <= to + size; v += size) {
			long index = lround(v / size);
			bool bold = step > 1 && index % step == 0;
			if (!bold && !minor)
				continue;
			/* Center a one-device-pixel line on a device pixel.  */
			double pos = (floor(v * device) + 0.5) / device;
			if (bold) {
				cairo_set_dash(cr, NULL, 0, 0);
				cairo_set_source_rgb(cr, bold_rgb[0], bold_rgb[1],
				                     bold_rgb[2]);
			} else {
				double space = 1 / zoom;
				double dash[] = {width * 3, space + width + space};
				cairo_set_dash(cr, dash, 2, 0);
				cairo_set_source_rgb(cr, minor_rgb[0], minor_rgb[1],
				                     minor_rgb[2]);
			}
			if (axis) {
				cairo_move_to(cr, x1 - size, pos);
				cairo_line_to(cr, x2 + size, pos);
			} else {
				cairo_move_to(cr, pos, y1 - size);
				cairo_line_to(cr, pos, y2 + size);
			}
			cairo_stroke(cr);
		}
	}
	cairo_restore(cr);
}
