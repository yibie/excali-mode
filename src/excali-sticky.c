/* excali-sticky.c --- Sticky note rendering  -*- c-file-style: "linux" -*-
 * Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Port of Excalidraw's stickyNote.ts drawing (docs/excalidraw-spec.md
 * §2a.11): plain canvas paths, no roughjs.  A shadow offset by 3 px, the
 * paper, an inner edge shadow and a date footer.  Corners jitter by
 * seededRandom (mulberry32) scaled by roughness; with roughness 2 one
 * corner curls up.
 */

#include "excali-sticky.h"

#include <math.h>
#include <string.h>

#define STICKY_SHADOW_OFFSET 3.0
#define STICKY_SHADOW_OPACITY 0.16
#define STICKY_EDGE_SHADOW_WIDTH 0.5
#define STICKY_EDGE_SHADOW_OPACITY 0.08
#define STICKY_CORNER_RADIUS_RATIO 0.04
#define STICKY_MAX_CORNER_RADIUS 16.0
#define STICKY_MIN_SIZE 75.0
#define STICKY_PADDING 16.0
#define STICKY_FOOTER_FONT_SIZE 12.0
#define STICKY_FOOTER_BASELINE 14.0

static const double render_roughness[] = {0, 1.5, 8};

void excali_mulberry_init(ExcaliMulberry *m, double seed)
{
	/* `seed >>> 0': ToUint32.  */
	m->value = (uint32_t)(int64_t)fmod(seed, 4294967296.0);
}

double excali_mulberry_next(ExcaliMulberry *m)
{
	m->value += 0x6d2b79f5u;
	uint32_t next = m->value;
	next = (next ^ (next >> 15)) * (next | 1u);
	next ^= next + (next ^ (next >> 7)) * (next | 61u);
	return (double)(next ^ (next >> 14)) / 4294967296.0;
}

typedef struct {
	double x, y;
} Pt;

static double jitter(ExcaliMulberry *m, double amount)
{
	return (excali_mulberry_next(m) * 2 - 1) * amount;
}

/* getStickyNoteRenderPoints: TL, TR, BR, BL.  */
static void render_points(const ExcaliElement *e, double ox, double oy,
                          double seed_offset, Pt p[4])
{
	double w = e->width, h = e->height;
	int roughness = (int)fmax(0, fmin(2, round(e->roughness)));
	double amount = fmin(render_roughness[roughness], fmin(w, h) * 0.012);
	p[0] = (Pt){ox, oy};
	p[1] = (Pt){ox + w, oy};
	p[2] = (Pt){ox + w, oy + h};
	p[3] = (Pt){ox, oy + h};
	if (amount == 0)
		return;
	ExcaliMulberry m;
	excali_mulberry_init(&m, (double)e->seed + seed_offset);
	/* Evaluation order matches the object literals: x then y.  */
	for (int i = 0; i < 4; ++i) {
		double jx = jitter(&m, amount);
		double jy = jitter(&m, amount);
		p[i].x += jx;
		p[i].y += jy;
	}
}

static Pt at_distance(Pt from, Pt to, double distance)
{
	double dx = to.x - from.x, dy = to.y - from.y;
	double len = hypot(dx, dy);
	if (len == 0)
		return from;
	return (Pt){from.x + dx * distance / len, from.y + dy * distance / len};
}

/* Append a quadratic Bezier from the current point as a cubic.  */
static void quad_to(cairo_t *cr, Pt c, Pt p)
{
	double x0, y0;
	cairo_get_current_point(cr, &x0, &y0);
	cairo_curve_to(cr, x0 + 2.0 / 3 * (c.x - x0), y0 + 2.0 / 3 * (c.y - y0),
	               p.x + 2.0 / 3 * (c.x - p.x), p.y + 2.0 / 3 * (c.y - p.y),
	               p.x, p.y);
}

typedef struct {
	int count;
	/* Each corner: up to 4 commands; kind 0 line, 1 quadratic.  */
	struct {
		int kind;
		Pt control, point;
	} cmd[4];
} Corner;

/* getStickyNotePathCommands, drawn onto CR.  */
static void sticky_path(cairo_t *cr, const ExcaliElement *e, bool shadow)
{
	Pt p[4];
	render_points(e, shadow ? STICKY_SHADOW_OFFSET : 0,
	              shadow ? STICKY_SHADOW_OFFSET : 0, shadow ? 1 : 0, p);
	double radius = e->rounded ? fmin(fmin(e->width, e->height) *
	                                          STICKY_CORNER_RADIUS_RATIO,
	                                  STICKY_MAX_CORNER_RADIUS)
	                           : 0;
	int lifted = -1;
	if (e->roughness == 2) {
		ExcaliMulberry m;
		excali_mulberry_init(&m, (double)e->seed);
		lifted = (int)floor(excali_mulberry_next(&m) * 4);
	}
	cairo_new_path(cr);
	if (radius == 0 && lifted == -1) {
		cairo_move_to(cr, p[0].x, p[0].y);
		for (int i = 1; i < 4; ++i)
			cairo_line_to(cr, p[i].x, p[i].y);
		cairo_close_path(cr);
		return;
	}
	Corner corners[4];
	for (int i = 0; i < 4; ++i) {
		Pt point = p[i], prev = p[(i + 3) % 4], next = p[(i + 1) % 4];
		double cr_ = fmin(radius,
		                  fmin(hypot(point.x - prev.x, point.y - prev.y) / 2,
		                       hypot(point.x - next.x, point.y - next.y) / 2));
		Corner *c = &corners[i];
		if (i == lifted) {
			double size = fmin(e->width, e->height);
			double reach = fmin(size * 0.18, 40);
			double lift = fmin(size * 0.02, 5) * (shadow ? 0.5 : 1);
			Pt tip = {point.x + ((i == 1 || i == 2) ? -lift : lift),
			          point.y + (i >= 2 ? -lift : lift)};
			Pt start = at_distance(point, prev, reach);
			Pt end = at_distance(point, next, reach);
			c->count = 4;
			c->cmd[0].kind = 0;
			c->cmd[0].point = start;
			c->cmd[1].kind = 1;
			c->cmd[1].control = at_distance(point, prev, reach / 2);
			c->cmd[1].point = at_distance(tip, start, cr_);
			c->cmd[2].kind = 1;
			c->cmd[2].control = tip;
			c->cmd[2].point = at_distance(tip, end, cr_);
			c->cmd[3].kind = 1;
			c->cmd[3].control = at_distance(point, next, reach / 2);
			c->cmd[3].point = end;
		} else {
			c->count = 2;
			c->cmd[0].kind = 0;
			c->cmd[0].point = at_distance(point, prev, cr_);
			c->cmd[1].kind = 1;
			c->cmd[1].control = point;
			c->cmd[1].point = at_distance(point, next, cr_);
		}
	}
	Pt start = corners[0].cmd[corners[0].count - 1].point;
	cairo_move_to(cr, start.x, start.y);
	for (int k = 1; k <= 4; ++k) {
		Corner *c = &corners[k % 4];
		for (int j = 0; j < c->count; ++j)
			if (c->cmd[j].kind == 0)
				cairo_line_to(cr, c->cmd[j].point.x, c->cmd[j].point.y);
			else
				quad_to(cr, c->cmd[j].control, c->cmd[j].point);
	}
	cairo_close_path(cr);
}

void excali_draw_sticky(cairo_t *cr, const ExcaliElement *e, bool has_fill,
                       const double fill[4], const double stroke[4])
{
	cairo_save(cr);
	/* 1. Shadow.  */
	sticky_path(cr, e, true);
	cairo_set_source_rgba(cr, 0, 0, 0, STICKY_SHADOW_OPACITY);
	cairo_fill(cr);
	/* 2. Paper.  */
	sticky_path(cr, e, false);
	if (has_fill) {
		cairo_set_source_rgba(cr, fill[0], fill[1], fill[2], fill[3]);
		cairo_fill_preserve(cr);
	}
	/* 3. Inner edge shadow: stroke the paper path clipped to itself.  */
	cairo_save(cr);
	cairo_clip_preserve(cr);
	cairo_set_line_width(cr, STICKY_EDGE_SHADOW_WIDTH * 2);
	cairo_set_source_rgba(cr, 0, 0, 0, STICKY_EDGE_SHADOW_OPACITY);
	cairo_stroke(cr);
	cairo_restore(cr);
	cairo_new_path(cr);
	/* 4. Date footer, right-aligned on its baseline.  */
	if (e->sticky_footer && *e->sticky_footer && e->width >= STICKY_MIN_SIZE &&
	    e->height >= STICKY_MIN_SIZE) {
		cairo_select_font_face(cr, "Helvetica", CAIRO_FONT_SLANT_NORMAL,
		                       CAIRO_FONT_WEIGHT_NORMAL);
		cairo_set_font_size(cr, STICKY_FOOTER_FONT_SIZE);
		cairo_text_extents_t ext;
		cairo_text_extents(cr, e->sticky_footer, &ext);
		cairo_move_to(cr, e->width - STICKY_PADDING - ext.x_advance,
		              e->height - STICKY_FOOTER_BASELINE);
		cairo_set_source_rgba(cr, stroke[0], stroke[1], stroke[2], stroke[3]);
		cairo_show_text(cr, e->sticky_footer);
	}
	cairo_restore(cr);
}
