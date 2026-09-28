/* excali-rough.c --- Port of roughjs 4.6.4 shape generation  -*- c-file-style: "linux" -*-
 * Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Function names follow the roughjs sources they port.  JS evaluates
 * operands left to right while C does not, so every random draw is its
 * own statement, in roughjs order.
 */

#include "excali-rough.h"

#include <math.h>
#include <stdlib.h>
#include <string.h>

/* Round every operation like JS does: no fused multiply-add, which
   changes results (the Makefile also passes -ffp-contract=off).  */
#ifdef __clang__
#pragma STDC FP_CONTRACT OFF
#endif

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

/* Random (math.ts).  */

uint32_t rough_to_uint32(double x)
{
	if (!isfinite(x))
		return 0;
	x = trunc(x);
	x = fmod(x, 4294967296.0);
	if (x < 0)
		x += 4294967296.0;
	return (uint32_t)x;
}

void rough_random_init(RoughRandom *r, double seed)
{
	r->seed = rough_to_uint32(seed);
}

double rough_random_next(RoughRandom *r)
{
	/* roughjs falls back to Math.random for seed 0; stay deterministic
	   instead.  Nonzero states never reach 0 since 48271 is odd.  */
	if (r->seed == 0)
		r->seed = 1;
	r->seed = 48271u * r->seed;
	return (double)(r->seed & 0x7fffffffu) / 2147483648.0;
}

RoughFillStyle rough_fill_style(const char *name)
{
	if (!name)
		return ROUGH_FILL_HACHURE;
	if (strcmp(name, "solid") == 0)
		return ROUGH_FILL_SOLID;
	if (strcmp(name, "zigzag") == 0)
		return ROUGH_FILL_ZIGZAG;
	if (strcmp(name, "cross-hatch") == 0)
		return ROUGH_FILL_CROSS_HATCH;
	/* "dots", "dashed" and "zigzag-line" exist in roughjs but Excalidraw
	   never produces them.  */
	return ROUGH_FILL_HACHURE;
}

RoughOptions rough_default_options(void)
{
	return (RoughOptions){
	        .max_randomness_offset = 2,
	        .roughness = 1,
	        .bowing = 1,
	        .stroke_width = 1,
	        .curve_tightness = 0,
	        .curve_fitting = 0.95,
	        .curve_step_count = 9,
	        .fill_style = ROUGH_FILL_HACHURE,
	        .fill_weight = -1,
	        .hachure_angle = -41,
	        .hachure_gap = -1,
	        .fill_shape_roughness_gain = 0.8,
	        .seed = 0,
	        .stroke = true,
	};
}

/* `_o': a copy of OPTIONS with no randomizer yet.  */
static RoughOptions resolve(const RoughOptions *options)
{
	RoughOptions o = *options;
	o.randomizer = NULL;
	return o;
}

static double random_next(RoughOptions *o)
{
	if (!o->randomizer) {
		o->randomizer = &o->randomizer_storage;
		rough_random_init(o->randomizer, o->seed);
	}
	return rough_random_next(o->randomizer);
}

static RoughOptions clone_options_alter_seed(const RoughOptions *o)
{
	RoughOptions result = *o;
	result.randomizer = NULL;
	if (o->seed != 0)
		result.seed = o->seed + 1;
	return result;
}

static double offset_range(double min, double max, RoughOptions *o,
                           double gain)
{
	double r = random_next(o);
	return o->roughness * gain * ((r * (max - min)) + min);
}

static double offset_opt(double x, RoughOptions *o, double gain)
{
	return offset_range(-x, x, o, gain);
}

/* Growable arrays.  */

static void ops_push(RoughOps *ops, RoughOpKind kind, const double *data,
                     int n)
{
	if (ops->count == ops->capacity) {
		size_t cap = ops->capacity ? ops->capacity * 2 : 16;
		RoughOp *grown = realloc(ops->ops, cap * sizeof *grown);
		if (!grown)
			return;
		ops->ops = grown;
		ops->capacity = cap;
	}
	RoughOp *op = &ops->ops[ops->count++];
	op->op = kind;
	memset(op->data, 0, sizeof op->data);
	memcpy(op->data, data, sizeof(double) * n);
}

static void ops_move(RoughOps *ops, double x, double y)
{
	ops_push(ops, ROUGH_MOVE, (double[]){x, y}, 2);
}

static void ops_line_to(RoughOps *ops, double x, double y)
{
	ops_push(ops, ROUGH_LINE_TO, (double[]){x, y}, 2);
}

static void ops_bcurve(RoughOps *ops, double x1, double y1, double x2,
                       double y2, double x, double y)
{
	ops_push(ops, ROUGH_BCURVE_TO, (double[]){x1, y1, x2, y2, x, y}, 6);
}

static void ops_free(RoughOps *ops)
{
	free(ops->ops);
	ops->ops = NULL;
	ops->count = ops->capacity = 0;
}

void rough_points_push(RoughPoints *p, double x, double y)
{
	if (p->count == p->capacity) {
		size_t cap = p->capacity ? p->capacity * 2 : 16;
		double *grown = realloc(p->xy, cap * 2 * sizeof *grown);
		if (!grown)
			return;
		p->xy = grown;
		p->capacity = cap;
	}
	p->xy[2 * p->count] = x;
	p->xy[2 * p->count + 1] = y;
	++p->count;
}

void rough_points_free(RoughPoints *p)
{
	free(p->xy);
	p->xy = NULL;
	p->count = p->capacity = 0;
}

void rough_drawable_free(RoughDrawable *d)
{
	for (int i = 0; i < d->set_count; ++i)
		ops_free(&d->sets[i].ops);
	d->set_count = 0;
}

static RoughOps *add_set(RoughDrawable *d, RoughSetType type)
{
	if (d->set_count >= ROUGH_MAX_SETS)
		return NULL;
	RoughSet *set = &d->sets[d->set_count++];
	set->type = type;
	set->ops = (RoughOps){0};
	return &set->ops;
}

/* Take the ops of a temporary set into a new set of D.  */
static void adopt_set(RoughDrawable *d, RoughSetType type, RoughOps *ops)
{
	RoughOps *dst = add_set(d, type);
	if (!dst) {
		ops_free(ops);
		return;
	}
	*dst = *ops;
	*ops = (RoughOps){0};
}

bool rough_fill_evenodd(const RoughDrawable *d)
{
	return d->shape == ROUGH_SHAPE_CURVE || d->shape == ROUGH_SHAPE_POLYGON ||
	       d->shape == ROUGH_SHAPE_PATH;
}

const RoughOps *rough_curve_path_ops(const RoughDrawable *d)
{
	for (int i = 0; i < d->set_count; ++i)
		if (d->sets[i].type == ROUGH_SET_PATH)
			return &d->sets[i].ops;
	return d->set_count > 0 ? &d->sets[0].ops : NULL;
}

/* Renderer (renderer.ts).  */

static void line_ops(RoughOps *ops, double x1, double y1, double x2,
                     double y2, RoughOptions *o, bool move, bool overlay)
{
	double length_sq = pow(x1 - x2, 2) + pow(y1 - y2, 2);
	double length = sqrt(length_sq);
	double gain = 1;
	if (length < 200)
		gain = 1;
	else if (length > 500)
		gain = 0.4;
	else
		gain = (-0.0016668) * length + 1.233334;

	double offset = o->max_randomness_offset;
	if ((offset * offset * 100) > length_sq)
		offset = length / 10;
	double half = offset / 2;
	double diverge = 0.2 + random_next(o) * 0.2;
	double mid_x = o->bowing * o->max_randomness_offset * (y2 - y1) / 200;
	double mid_y = o->bowing * o->max_randomness_offset * (x1 - x2) / 200;
	mid_x = offset_opt(mid_x, o, gain);
	mid_y = offset_opt(mid_y, o, gain);
	double r = overlay ? half : offset;
	bool keep = o->preserve_vertices;

	if (move) {
		double mx = x1, my = y1;
		if (!keep) {
			mx += offset_opt(r, o, gain);
			my += offset_opt(r, o, gain);
		}
		ops_move(ops, mx, my);
	}
	double d[6];
	d[0] = mid_x + x1 + (x2 - x1) * diverge;
	d[0] += offset_opt(r, o, gain);
	d[1] = mid_y + y1 + (y2 - y1) * diverge;
	d[1] += offset_opt(r, o, gain);
	d[2] = mid_x + x1 + 2 * (x2 - x1) * diverge;
	d[2] += offset_opt(r, o, gain);
	d[3] = mid_y + y1 + 2 * (y2 - y1) * diverge;
	d[3] += offset_opt(r, o, gain);
	d[4] = x2;
	d[5] = y2;
	if (!keep) {
		d[4] += offset_opt(r, o, gain);
		d[5] += offset_opt(r, o, gain);
	}
	ops_bcurve(ops, d[0], d[1], d[2], d[3], d[4], d[5]);
}

static void double_line(RoughOps *ops, double x1, double y1, double x2,
                        double y2, RoughOptions *o, bool filling)
{
	bool single = filling ? o->disable_multi_stroke_fill
	                      : o->disable_multi_stroke;
	line_ops(ops, x1, y1, x2, y2, o, true, false);
	if (!single)
		line_ops(ops, x1, y1, x2, y2, o, true, true);
}

static void linear_path_ops(RoughOps *ops, const double *p, size_t len,
                            bool close, RoughOptions *o)
{
	if (len > 2) {
		for (size_t i = 0; i + 1 < len; ++i)
			double_line(ops, p[2 * i], p[2 * i + 1], p[2 * i + 2],
			            p[2 * i + 3], o, false);
		if (close)
			double_line(ops, p[2 * len - 2], p[2 * len - 1], p[0],
			            p[1], o, false);
	} else if (len == 2) {
		double_line(ops, p[0], p[1], p[2], p[3], o, false);
	}
}

/* `_curve' with a null closePoint.  */
static void curve_ops(RoughOps *ops, const double *p, size_t len,
                      RoughOptions *o)
{
	if (len > 3) {
		double s = 1 - o->curve_tightness;
		ops_move(ops, p[2], p[3]);
		for (size_t i = 1; i + 2 < len; ++i) {
			const double *prev = p + 2 * (i - 1);
			const double *cur = p + 2 * i;
			const double *next = p + 2 * (i + 1);
			const double *after = p + 2 * (i + 2);
			ops_bcurve(ops,
			           cur[0] + (s * next[0] - s * prev[0]) / 6,
			           cur[1] + (s * next[1] - s * prev[1]) / 6,
			           next[0] + (s * cur[0] - s * after[0]) / 6,
			           next[1] + (s * cur[1] - s * after[1]) / 6,
			           next[0], next[1]);
		}
	} else if (len == 3) {
		ops_move(ops, p[2], p[3]);
		ops_bcurve(ops, p[2], p[3], p[4], p[5], p[4], p[5]);
	} else if (len == 2) {
		double_line(ops, p[0], p[1], p[2], p[3], o, false);
	}
}

static void curve_with_offset(RoughOps *ops, const double *p, size_t n,
                              double offset, RoughOptions *o)
{
	if (n == 0)
		return;
	RoughPoints ps = {0};
	for (int k = 0; k < 2; ++k) {
		double x = p[0], y = p[1];
		x += offset_opt(offset, o, 1);
		y += offset_opt(offset, o, 1);
		rough_points_push(&ps, x, y);
	}
	for (size_t i = 1; i < n; ++i) {
		double x = p[2 * i], y = p[2 * i + 1];
		x += offset_opt(offset, o, 1);
		y += offset_opt(offset, o, 1);
		rough_points_push(&ps, x, y);
		if (i == n - 1) {
			x = p[2 * i];
			y = p[2 * i + 1];
			x += offset_opt(offset, o, 1);
			y += offset_opt(offset, o, 1);
			rough_points_push(&ps, x, y);
		}
	}
	curve_ops(ops, ps.xy, ps.count, o);
	rough_points_free(&ps);
}

/* renderer.ts `curve'.  */
static void curve_outline(RoughOps *ops, const double *p, size_t n,
                          RoughOptions *o)
{
	curve_with_offset(ops, p, n, 1 * (1 + o->roughness * 0.2), o);
	if (!o->disable_multi_stroke) {
		RoughOptions o2 = clone_options_alter_seed(o);
		curve_with_offset(ops, p, n, 1.5 * (1 + o->roughness * 0.22),
		                  &o2);
	}
}

typedef struct {
	double increment, rx, ry;
} EllipseParams;

static EllipseParams ellipse_params(double width, double height,
                                    RoughOptions *o)
{
	double psq = sqrt(M_PI * 2 *
	                  sqrt((pow(width / 2, 2) + pow(height / 2, 2)) / 2));
	double steps = ceil(fmax(o->curve_step_count,
	                         (o->curve_step_count / sqrt(200)) * psq));
	EllipseParams e;
	e.increment = (M_PI * 2) / steps;
	e.rx = fabs(width / 2);
	e.ry = fabs(height / 2);
	double fit = 1 - o->curve_fitting;
	e.rx += offset_opt(e.rx * fit, o, 1);
	e.ry += offset_opt(e.ry * fit, o, 1);
	return e;
}

/* `_computeEllipsePoints'.  CORE may be NULL.  */
static void ellipse_points(double increment, double cx, double cy, double rx,
                           double ry, double offset, double overlap,
                           RoughOptions *o, RoughPoints *all,
                           RoughPoints *core)
{
	if (o->roughness == 0) {
		increment = increment / 4;
		rough_points_push(all, cx + rx * cos(-increment),
		                  cy + ry * sin(-increment));
		for (double angle = 0; angle <= M_PI * 2;
		     angle = angle + increment) {
			double x = cx + rx * cos(angle), y = cy + ry * sin(angle);
			if (core)
				rough_points_push(core, x, y);
			rough_points_push(all, x, y);
		}
		rough_points_push(all, cx + rx * cos(0), cy + ry * sin(0));
		rough_points_push(all, cx + rx * cos(increment),
		                  cy + ry * sin(increment));
		return;
	}
	double rad_offset = offset_opt(0.5, o, 1) - (M_PI / 2);
	/* `_offsetOpt(offset) + cx + 0.9 * rx * cos(...)' adds left to
	   right, so keep that association.  */
	{
		double dx = offset_opt(offset, o, 1);
		double x = dx + cx + 0.9 * rx * cos(rad_offset - increment);
		double dy = offset_opt(offset, o, 1);
		double y = dy + cy + 0.9 * ry * sin(rad_offset - increment);
		rough_points_push(all, x, y);
	}
	double end_angle = M_PI * 2 + rad_offset - 0.01;
	for (double angle = rad_offset; angle < end_angle;
	     angle = angle + increment) {
		double dx = offset_opt(offset, o, 1);
		double x = dx + cx + rx * cos(angle);
		double dy = offset_opt(offset, o, 1);
		double y = dy + cy + ry * sin(angle);
		if (core)
			rough_points_push(core, x, y);
		rough_points_push(all, x, y);
	}
	{
		double dx = offset_opt(offset, o, 1);
		double x = dx + cx +
		           rx * cos(rad_offset + M_PI * 2 + overlap * 0.5);
		double dy = offset_opt(offset, o, 1);
		double y = dy + cy +
		           ry * sin(rad_offset + M_PI * 2 + overlap * 0.5);
		rough_points_push(all, x, y);
	}
	{
		double dx = offset_opt(offset, o, 1);
		double x = dx + cx + 0.98 * rx * cos(rad_offset + overlap);
		double dy = offset_opt(offset, o, 1);
		double y = dy + cy + 0.98 * ry * sin(rad_offset + overlap);
		rough_points_push(all, x, y);
	}
	{
		double dx = offset_opt(offset, o, 1);
		double x = dx + cx + 0.9 * rx * cos(rad_offset + overlap * 0.5);
		double dy = offset_opt(offset, o, 1);
		double y = dy + cy + 0.9 * ry * sin(rad_offset + overlap * 0.5);
		rough_points_push(all, x, y);
	}
}

/* `ellipseWithParams': stroke ops into OPS, estimated points into CORE
   (may be NULL).  */
static void ellipse_with_params(RoughOps *ops, double x, double y,
                                RoughOptions *o, const EllipseParams *e,
                                RoughPoints *core)
{
	double inner = offset_range(0.4, 1, o, 1);
	double overlap = e->increment * offset_range(0.1, inner, o, 1);
	RoughPoints ap1 = {0};
	ellipse_points(e->increment, x, y, e->rx, e->ry, 1, overlap, o, &ap1,
	               core);
	curve_ops(ops, ap1.xy, ap1.count, o);
	rough_points_free(&ap1);
	if (!o->disable_multi_stroke && o->roughness != 0) {
		RoughPoints ap2 = {0};
		ellipse_points(e->increment, x, y, e->rx, e->ry, 1.5, 0, o,
		               &ap2, NULL);
		curve_ops(ops, ap2.xy, ap2.count, o);
		rough_points_free(&ap2);
	}
}

static void bezier_to(RoughOps *ops, double x1, double y1, double x2,
                      double y2, double x, double y, const double *current,
                      RoughOptions *o)
{
	double base = o->max_randomness_offset ? o->max_randomness_offset : 1;
	double ros[2] = {base, base + 0.3};
	int iterations = o->disable_multi_stroke ? 1 : 2;
	bool keep = o->preserve_vertices;
	for (int i = 0; i < iterations; ++i) {
		if (i == 0) {
			ops_move(ops, current[0], current[1]);
		} else {
			double mx = current[0], my = current[1];
			if (!keep) {
				mx += offset_opt(ros[0], o, 1);
				my += offset_opt(ros[0], o, 1);
			}
			ops_move(ops, mx, my);
		}
		double fx = x, fy = y;
		if (!keep) {
			fx += offset_opt(ros[i], o, 1);
			fy += offset_opt(ros[i], o, 1);
		}
		double d[4] = {x1, y1, x2, y2};
		for (int k = 0; k < 4; ++k)
			d[k] += offset_opt(ros[i], o, 1);
		ops_bcurve(ops, d[0], d[1], d[2], d[3], fx, fy);
	}
}

static void svg_path_ops(RoughOps *ops, const RoughPath *path,
                         RoughOptions *o)
{
	double first[2] = {0, 0}, current[2] = {0, 0};
	for (size_t i = 0; i < path->count; ++i) {
		const RoughSegment *s = &path->segments[i];
		switch (s->key) {
		case 'M':
			current[0] = first[0] = s->data[0];
			current[1] = first[1] = s->data[1];
			break;
		case 'L':
			double_line(ops, current[0], current[1], s->data[0],
			            s->data[1], o, false);
			current[0] = s->data[0];
			current[1] = s->data[1];
			break;
		case 'C':
			bezier_to(ops, s->data[0], s->data[1], s->data[2],
			          s->data[3], s->data[4], s->data[5], current, o);
			current[0] = s->data[4];
			current[1] = s->data[5];
			break;
		case 'Z':
			double_line(ops, current[0], current[1], first[0],
			            first[1], o, false);
			current[0] = first[0];
			current[1] = first[1];
			break;
		}
	}
}

/* Fills.  */

static void solid_fill_polygon(RoughOps *ops, const RoughPoints *polygons,
                               size_t count, RoughOptions *o)
{
	for (size_t k = 0; k < count; ++k) {
		const RoughPoints *points = &polygons[k];
		double offset = o->max_randomness_offset;
		size_t len = points->count;
		if (len > 2) {
			double x = points->xy[0], y = points->xy[1];
			x += offset_opt(offset, o, 1);
			y += offset_opt(offset, o, 1);
			ops_move(ops, x, y);
			for (size_t i = 1; i < len; ++i) {
				x = points->xy[2 * i];
				y = points->xy[2 * i + 1];
				x += offset_opt(offset, o, 1);
				y += offset_opt(offset, o, 1);
				ops_line_to(ops, x, y);
			}
		}
	}
}

/* hachure-fill 0.5.2.  */

static void rotate_points(double *xy, size_t n, double degrees)
{
	double angle = (M_PI / 180) * degrees;
	double c = cos(angle), s = sin(angle);
	for (size_t i = 0; i < n; ++i) {
		double x = xy[2 * i], y = xy[2 * i + 1];
		xy[2 * i] = ((x - 0) * c) - ((y - 0) * s) + 0;
		xy[2 * i + 1] = ((x - 0) * s) + ((y - 0) * c) + 0;
	}
}

/* Math.round: halves round up.  */
static double js_round(double x)
{
	double r = floor(x);
	return x - r >= 0.5 ? r + 1 : r;
}

typedef struct {
	double ymin, ymax, x, islope;
} Edge;

static int edge_compare(const Edge *e1, const Edge *e2)
{
	if (e1->ymin < e2->ymin)
		return -1;
	if (e1->ymin > e2->ymin)
		return 1;
	if (e1->x < e2->x)
		return -1;
	if (e1->x > e2->x)
		return 1;
	if (e1->ymax == e2->ymax)
		return 0;
	return (e1->ymax - e2->ymax) / fabs(e1->ymax - e2->ymax) < 0 ? -1 : 1;
}

static void straight_hachure_lines(RoughPoints *polygons, size_t count,
                                   double gap, double step,
                                   RoughPoints *lines)
{
	size_t edge_cap = 0;
	for (size_t k = 0; k < count; ++k)
		edge_cap += polygons[k].count + 1;
	Edge *edges = malloc((edge_cap ? edge_cap : 1) * sizeof *edges);
	size_t *active = malloc((edge_cap ? edge_cap : 1) * sizeof *active);
	if (!edges || !active) {
		free(edges);
		free(active);
		return;
	}
	size_t edge_count = 0;
	for (size_t k = 0; k < count; ++k) {
		const double *v = polygons[k].xy;
		size_t n = polygons[k].count;
		if (n == 0)
			continue;
		bool closed = v[0] == v[2 * n - 2] && v[1] == v[2 * n - 1];
		size_t vn = closed ? n : n + 1;
		if (vn <= 2)
			continue;
		for (size_t i = 0; i + 1 < vn; ++i) {
			const double *p1 = v + 2 * i;
			const double *p2 = (i + 1 == n) ? v : v + 2 * (i + 1);
			if (p1[1] != p2[1]) {
				double ymin = fmin(p1[1], p2[1]);
				Edge e = {
				        .ymin = ymin,
				        .ymax = fmax(p1[1], p2[1]),
				        .x = ymin == p1[1] ? p1[0] : p2[0],
				        .islope = (p2[0] - p1[0]) /
				                  (p2[1] - p1[1]),
				};
				/* Stable insertion, as Array.prototype.sort is
				   stable.  */
				size_t j = edge_count++;
				while (j > 0 && edge_compare(&edges[j - 1], &e) > 0) {
					edges[j] = edges[j - 1];
					--j;
				}
				edges[j] = e;
			}
		}
	}
	gap = fmax(gap, 0.1);
	if (edge_count == 0) {
		free(edges);
		free(active);
		return;
	}
	size_t next_edge = 0, active_count = 0;
	double y = edges[0].ymin;
	double iteration = 0;
	/* Guard against runaway loops on absurd coordinates.  */
	size_t guard = 0;
	while ((active_count || next_edge < edge_count) && guard++ < 4000000) {
		if (next_edge < edge_count) {
			size_t ix = next_edge;
			while (ix < edge_count && !(edges[ix].ymin > y))
				++ix;
			for (; next_edge < ix; ++next_edge)
				active[active_count++] = next_edge;
		}
		size_t kept = 0;
		for (size_t i = 0; i < active_count; ++i)
			if (!(edges[active[i]].ymax <= y))
				active[kept++] = active[i];
		active_count = kept;
		for (size_t i = 1; i < active_count; ++i) {
			size_t a = active[i];
			size_t j = i;
			while (j > 0 && edges[active[j - 1]].x > edges[a].x) {
				active[j] = active[j - 1];
				--j;
			}
			active[j] = a;
		}
		if (step != 1 || fmod(iteration, gap) == 0) {
			if (active_count > 1)
				for (size_t i = 0; i + 1 < active_count; i += 2) {
					const Edge *ce = &edges[active[i]];
					const Edge *ne = &edges[active[i + 1]];
					rough_points_push(lines, js_round(ce->x), y);
					rough_points_push(lines, js_round(ne->x), y);
				}
		}
		y += step;
		for (size_t i = 0; i < active_count; ++i)
			edges[active[i]].x =
			        edges[active[i]].x + (step * edges[active[i]].islope);
		iteration++;
	}
	free(edges);
	free(active);
}

void rough_hachure_lines(const RoughPoints *polygons, size_t count,
                         double gap, double angle, double step_offset,
                         RoughPoints *lines)
{
	gap = fmax(gap, 0.1);
	RoughPoints *copies = calloc(count ? count : 1, sizeof *copies);
	if (!copies)
		return;
	for (size_t k = 0; k < count; ++k)
		for (size_t i = 0; i < polygons[k].count; ++i)
			rough_points_push(&copies[k], polygons[k].xy[2 * i],
			                  polygons[k].xy[2 * i + 1]);
	if (angle)
		for (size_t k = 0; k < count; ++k)
			rotate_points(copies[k].xy, copies[k].count, angle);
	size_t first = lines->count;
	straight_hachure_lines(copies, count, gap, step_offset, lines);
	if (angle)
		rotate_points(lines->xy + 2 * first, lines->count - first,
		              -angle);
	for (size_t k = 0; k < count; ++k)
		rough_points_free(&copies[k]);
	free(copies);
}

/* scan-line-hachure.ts `polygonHachureLines'.  */
static void polygon_hachure_lines(const RoughPoints *polygons, size_t count,
                                  RoughOptions *o, RoughPoints *lines)
{
	double angle = o->hachure_angle + 90;
	double gap = o->hachure_gap;
	if (gap < 0)
		gap = o->stroke_width * 4;
	gap = fmax(gap, 0.1);
	double skip = 1;
	if (o->roughness >= 1) {
		/* `o.randomizer?.next() || Math.random()'.  The randomizer
		   always exists here because the outline was generated
		   first; if not, seed one instead of using Math.random.  */
		if (random_next(o) > 0.7)
			skip = gap;
	}
	rough_hachure_lines(polygons, count, gap, angle, skip ? skip : 1,
	                    lines);
}

static void render_lines(RoughOps *ops, const RoughPoints *lines,
                         RoughOptions *o)
{
	for (size_t i = 0; i + 1 < lines->count; i += 2) {
		const double *l = lines->xy + 2 * i;
		double_line(ops, l[0], l[1], l[2], l[3], o, true);
	}
}

static void hachure_fill(RoughOps *ops, const RoughPoints *polygons,
                         size_t count, RoughOptions *o)
{
	RoughPoints lines = {0};
	polygon_hachure_lines(polygons, count, o, &lines);
	render_lines(ops, &lines, o);
	rough_points_free(&lines);
}

static void zigzag_fill(RoughOps *ops, const RoughPoints *polygons,
                        size_t count, RoughOptions *o)
{
	double gap = o->hachure_gap;
	if (gap < 0)
		gap = o->stroke_width * 4;
	gap = fmax(gap, 0.1);
	RoughOptions o2 = *o;
	o2.hachure_gap = gap;
	RoughPoints lines = {0}, zigzag = {0};
	polygon_hachure_lines(polygons, count, &o2, &lines);
	double angle = (M_PI / 180) * o->hachure_angle;
	double dgx = gap * 0.5 * cos(angle);
	double dgy = gap * 0.5 * sin(angle);
	for (size_t i = 0; i + 1 < lines.count; i += 2) {
		const double *p1 = lines.xy + 2 * i, *p2 = p1 + 2;
		if (sqrt(pow(p1[0] - p2[0], 2) + pow(p1[1] - p2[1], 2))) {
			rough_points_push(&zigzag, p1[0] - dgx, p1[1] + dgy);
			rough_points_push(&zigzag, p2[0], p2[1]);
			rough_points_push(&zigzag, p1[0] + dgx, p1[1] - dgy);
			rough_points_push(&zigzag, p2[0], p2[1]);
		}
	}
	render_lines(ops, &zigzag, o);
	rough_points_free(&lines);
	rough_points_free(&zigzag);
}

static void pattern_fill_polygons(RoughOps *ops, const RoughPoints *polygons,
                                  size_t count, RoughOptions *o)
{
	switch (o->fill_style) {
	case ROUGH_FILL_ZIGZAG:
		zigzag_fill(ops, polygons, count, o);
		break;
	case ROUGH_FILL_CROSS_HATCH: {
		hachure_fill(ops, polygons, count, o);
		/* hachure_fill created O's randomizer, so O2 shares it.  */
		RoughOptions o2 = *o;
		o2.hachure_angle = o->hachure_angle + 90;
		hachure_fill(ops, polygons, count, &o2);
		break;
	}
	default:
		hachure_fill(ops, polygons, count, o);
		break;
	}
}

/* points-on-curve 0.2.0.  */

static double dist_sq(const double *a, const double *b)
{
	return pow(a[0] - b[0], 2) + pow(a[1] - b[1], 2);
}

static double distance_to_segment_sq(const double *p, const double *v,
                                     const double *w)
{
	double l2 = dist_sq(v, w);
	if (l2 == 0)
		return dist_sq(p, v);
	double t = ((p[0] - v[0]) * (w[0] - v[0]) +
	            (p[1] - v[1]) * (w[1] - v[1])) /
	           l2;
	t = fmax(0, fmin(1, t));
	double q[2] = {v[0] + (w[0] - v[0]) * t, v[1] + (w[1] - v[1]) * t};
	return dist_sq(p, q);
}

static void simplify_points(const double *xy, size_t start, size_t end,
                            double epsilon, RoughPoints *out)
{
	const double *s = xy + 2 * start;
	const double *e = xy + 2 * (end - 1);
	double max_dist_sq = 0;
	size_t max_ndx = 1;
	for (size_t i = start + 1; i + 1 < end; ++i) {
		double d = distance_to_segment_sq(xy + 2 * i, s, e);
		if (d > max_dist_sq) {
			max_dist_sq = d;
			max_ndx = i;
		}
	}
	if (sqrt(max_dist_sq) > epsilon) {
		simplify_points(xy, start, max_ndx + 1, epsilon, out);
		simplify_points(xy, max_ndx, end, epsilon, out);
	} else {
		if (!out->count)
			rough_points_push(out, s[0], s[1]);
		rough_points_push(out, e[0], e[1]);
	}
}

void rough_simplify(const double *xy, size_t n, double distance,
                    RoughPoints *out)
{
	if (n == 0)
		return;
	simplify_points(xy, 0, n, distance, out);
}

static double flatness(const double *p)
{
	/* p: 4 points, flat.  */
	double ux = 3 * p[2] - 2 * p[0] - p[6];
	ux *= ux;
	double uy = 3 * p[3] - 2 * p[1] - p[7];
	uy *= uy;
	double vx = 3 * p[4] - 2 * p[6] - p[0];
	vx *= vx;
	double vy = 3 * p[5] - 2 * p[7] - p[1];
	vy *= vy;
	if (ux < vx)
		ux = vx;
	if (uy < vy)
		uy = vy;
	return ux + uy;
}

static void lerp2(const double *a, const double *b, double t, double *out)
{
	out[0] = a[0] + (b[0] - a[0]) * t;
	out[1] = a[1] + (b[1] - a[1]) * t;
}

static void bezier_split_points(const double *p, double tolerance,
                                RoughPoints *out, int depth)
{
	if (flatness(p) < tolerance || depth > 24) {
		if (out->count) {
			const double *last = out->xy + 2 * (out->count - 1);
			if (sqrt(dist_sq(last, p)) > 1)
				rough_points_push(out, p[0], p[1]);
		} else {
			rough_points_push(out, p[0], p[1]);
		}
		rough_points_push(out, p[6], p[7]);
		return;
	}
	double q1[2], q2[2], q3[2], r1[2], r2[2], red[2];
	lerp2(p, p + 2, 0.5, q1);
	lerp2(p + 2, p + 4, 0.5, q2);
	lerp2(p + 4, p + 6, 0.5, q3);
	lerp2(q1, q2, 0.5, r1);
	lerp2(q2, q3, 0.5, r2);
	lerp2(r1, r2, 0.5, red);
	double a[8] = {p[0], p[1], q1[0], q1[1], r1[0], r1[1], red[0], red[1]};
	double b[8] = {red[0], red[1], r2[0], r2[1], q3[0], q3[1], p[6], p[7]};
	bezier_split_points(a, tolerance, out, depth + 1);
	bezier_split_points(b, tolerance, out, depth + 1);
}

/* `pointsOnBezierCurves' of N points (1 + 3k), simplified by DISTANCE
   when positive.  */
static void points_on_bezier_curves(const double *xy, size_t n,
                                    double tolerance, double distance,
                                    RoughPoints *out)
{
	RoughPoints pts = {0};
	size_t segments = (n - 1) / 3;
	for (size_t i = 0; i < segments; ++i)
		bezier_split_points(xy + 2 * (i * 3), tolerance, &pts, 0);
	if (distance > 0 && pts.count) {
		simplify_points(pts.xy, 0, pts.count, distance, out);
	} else {
		for (size_t i = 0; i < pts.count; ++i)
			rough_points_push(out, pts.xy[2 * i], pts.xy[2 * i + 1]);
	}
	rough_points_free(&pts);
}

/* curve-to-bezier.ts `curveToBezier' with curveTightness 0; N >= 3.  */
static void curve_to_bezier(const double *in, size_t len, RoughPoints *out)
{
	if (len == 3) {
		rough_points_push(out, in[0], in[1]);
		rough_points_push(out, in[2], in[3]);
		rough_points_push(out, in[4], in[5]);
		rough_points_push(out, in[4], in[5]);
		return;
	}
	RoughPoints pts = {0};
	rough_points_push(&pts, in[0], in[1]);
	rough_points_push(&pts, in[0], in[1]);
	for (size_t i = 1; i < len; ++i) {
		rough_points_push(&pts, in[2 * i], in[2 * i + 1]);
		if (i == len - 1)
			rough_points_push(&pts, in[2 * i], in[2 * i + 1]);
	}
	const double s = 1 - 0.0;
	const double *p = pts.xy;
	rough_points_push(out, p[0], p[1]);
	for (size_t i = 1; i + 2 < pts.count; ++i) {
		const double *prev = p + 2 * (i - 1), *cur = p + 2 * i;
		const double *next = p + 2 * (i + 1), *after = p + 2 * (i + 2);
		rough_points_push(out, cur[0] + (s * next[0] - s * prev[0]) / 6,
		                  cur[1] + (s * next[1] - s * prev[1]) / 6);
		rough_points_push(out, next[0] + (s * cur[0] - s * after[0]) / 6,
		                  next[1] + (s * cur[1] - s * after[1]) / 6);
		rough_points_push(out, next[0], next[1]);
	}
	rough_points_free(&pts);
}

/* points-on-path 0.2.1 `pointsOnPath' with tolerance 1.  Returns the
   number of sets written to *SETS (malloc'ed).  */
static size_t points_on_path(const RoughPath *path, double distance,
                             RoughPoints **sets_out)
{
	size_t cap = 4, count = 0;
	RoughPoints *sets = calloc(cap, sizeof *sets);
	RoughPoints current = {0}, pending = {0};
	double start[2] = {0, 0};
	const double tolerance = 1;
#define APPEND_PENDING_CURVE()                                              \
	do {                                                                \
		if (pending.count >= 4)                                     \
			points_on_bezier_curves(pending.xy, pending.count,  \
			                        tolerance, 0, &current);    \
		pending.count = 0;                                          \
	} while (0)
#define APPEND_PENDING_POINTS()                                             \
	do {                                                                \
		APPEND_PENDING_CURVE();                                     \
		if (current.count) {                                        \
			if (count == cap) {                                 \
				cap *= 2;                                   \
				RoughPoints *g =                            \
				        realloc(sets, cap * sizeof *sets);  \
				if (g)                                      \
					sets = g;                           \
			}                                                   \
			if (count < cap)                                    \
				sets[count++] = current;                    \
			else                                                \
				rough_points_free(&current);                \
			current = (RoughPoints){0};                         \
		}                                                           \
	} while (0)
	for (size_t i = 0; sets && i < path->count; ++i) {
		const RoughSegment *s = &path->segments[i];
		switch (s->key) {
		case 'M':
			APPEND_PENDING_POINTS();
			start[0] = s->data[0];
			start[1] = s->data[1];
			rough_points_push(&current, start[0], start[1]);
			break;
		case 'L':
			APPEND_PENDING_CURVE();
			rough_points_push(&current, s->data[0], s->data[1]);
			break;
		case 'C':
			if (!pending.count) {
				const double *last =
				        current.count
				                ? current.xy + 2 * (current.count - 1)
				                : start;
				rough_points_push(&pending, last[0], last[1]);
			}
			rough_points_push(&pending, s->data[0], s->data[1]);
			rough_points_push(&pending, s->data[2], s->data[3]);
			rough_points_push(&pending, s->data[4], s->data[5]);
			break;
		case 'Z':
			APPEND_PENDING_CURVE();
			rough_points_push(&current, start[0], start[1]);
			break;
		}
	}
	if (sets)
		APPEND_PENDING_POINTS();
#undef APPEND_PENDING_POINTS
#undef APPEND_PENDING_CURVE
	rough_points_free(&current);
	rough_points_free(&pending);
	if (sets && distance) {
		for (size_t k = 0; k < count; ++k) {
			RoughPoints simplified = {0};
			rough_simplify(sets[k].xy, sets[k].count, distance,
			               &simplified);
			rough_points_free(&sets[k]);
			sets[k] = simplified;
		}
	}
	*sets_out = sets;
	return sets ? count : 0;
}

/* Paths.  */

static void path_push(RoughPath *p, char key, const double *data, int n)
{
	if (p->count == p->capacity) {
		size_t cap = p->capacity ? p->capacity * 2 : 16;
		RoughSegment *g = realloc(p->segments, cap * sizeof *g);
		if (!g)
			return;
		p->segments = g;
		p->capacity = cap;
	}
	RoughSegment *s = &p->segments[p->count++];
	s->key = key;
	memset(s->data, 0, sizeof s->data);
	if (n)
		memcpy(s->data, data, sizeof(double) * n);
}

void rough_path_move(RoughPath *p, double x, double y)
{
	path_push(p, 'M', (double[]){x, y}, 2);
	p->cx = p->subx = x;
	p->cy = p->suby = y;
}

void rough_path_line(RoughPath *p, double x, double y)
{
	path_push(p, 'L', (double[]){x, y}, 2);
	p->cx = x;
	p->cy = y;
}

void rough_path_cubic(RoughPath *p, double x1, double y1, double x2,
                      double y2, double x, double y)
{
	path_push(p, 'C', (double[]){x1, y1, x2, y2, x, y}, 6);
	p->cx = x;
	p->cy = y;
}

void rough_path_quad(RoughPath *p, double x1, double y1, double x, double y)
{
	double cx = p->cx, cy = p->cy;
	double cx1 = cx + 2 * (x1 - cx) / 3;
	double cy1 = cy + 2 * (y1 - cy) / 3;
	double cx2 = x + 2 * (x1 - x) / 3;
	double cy2 = y + 2 * (y1 - y) / 3;
	rough_path_cubic(p, cx1, cy1, cx2, cy2, x, y);
}

void rough_path_close(RoughPath *p)
{
	path_push(p, 'Z', NULL, 0);
	p->cx = p->subx;
	p->cy = p->suby;
}

void rough_path_free(RoughPath *p)
{
	free(p->segments);
	*p = (RoughPath){0};
}

/* Generator (generator.ts).  */

static void begin(RoughDrawable *d, RoughShape shape, const RoughOptions *o)
{
	d->shape = shape;
	d->set_count = 0;
	d->options = *o;
	d->options.randomizer = NULL;
}

void rough_gen_line(RoughDrawable *d, double x1, double y1, double x2,
                    double y2, const RoughOptions *options)
{
	RoughOptions o = resolve(options);
	begin(d, ROUGH_SHAPE_LINE, &o);
	RoughOps *ops = add_set(d, ROUGH_SET_PATH);
	double_line(ops, x1, y1, x2, y2, &o, false);
}

void rough_gen_rectangle(RoughDrawable *d, double x, double y, double width,
                         double height, const RoughOptions *options)
{
	RoughOptions o = resolve(options);
	begin(d, ROUGH_SHAPE_RECTANGLE, &o);
	double pts[8] = {x, y, x + width, y, x + width, y + height, x,
	                 y + height};
	RoughOps outline = {0};
	linear_path_ops(&outline, pts, 4, true, &o);
	if (o.fill) {
		RoughPoints poly = {.xy = pts, .count = 4, .capacity = 4};
		RoughOps fill = {0};
		if (o.fill_style == ROUGH_FILL_SOLID) {
			solid_fill_polygon(&fill, &poly, 1, &o);
			adopt_set(d, ROUGH_SET_FILL_PATH, &fill);
		} else {
			pattern_fill_polygons(&fill, &poly, 1, &o);
			adopt_set(d, ROUGH_SET_FILL_SKETCH, &fill);
		}
	}
	if (o.stroke)
		adopt_set(d, ROUGH_SET_PATH, &outline);
	else
		ops_free(&outline);
}

void rough_gen_ellipse(RoughDrawable *d, double x, double y, double width,
                       double height, const RoughOptions *options)
{
	RoughOptions o = resolve(options);
	begin(d, ROUGH_SHAPE_ELLIPSE, &o);
	EllipseParams params = ellipse_params(width, height, &o);
	RoughOps outline = {0};
	RoughPoints estimated = {0};
	ellipse_with_params(&outline, x, y, &o, &params, &estimated);
	if (o.fill) {
		RoughOps fill = {0};
		if (o.fill_style == ROUGH_FILL_SOLID) {
			ellipse_with_params(&fill, x, y, &o, &params, NULL);
			adopt_set(d, ROUGH_SET_FILL_PATH, &fill);
		} else {
			pattern_fill_polygons(&fill, &estimated, 1, &o);
			adopt_set(d, ROUGH_SET_FILL_SKETCH, &fill);
		}
	}
	rough_points_free(&estimated);
	if (o.stroke)
		adopt_set(d, ROUGH_SET_PATH, &outline);
	else
		ops_free(&outline);
}

void rough_gen_circle(RoughDrawable *d, double x, double y, double diameter,
                      const RoughOptions *options)
{
	rough_gen_ellipse(d, x, y, diameter, diameter, options);
	d->shape = ROUGH_SHAPE_CIRCLE;
}

void rough_gen_linear_path(RoughDrawable *d, const double *xy, size_t n,
                           const RoughOptions *options)
{
	RoughOptions o = resolve(options);
	begin(d, ROUGH_SHAPE_LINEAR_PATH, &o);
	RoughOps *ops = add_set(d, ROUGH_SET_PATH);
	linear_path_ops(ops, xy, n, false, &o);
}

void rough_gen_polygon(RoughDrawable *d, const double *xy, size_t n,
                       const RoughOptions *options)
{
	RoughOptions o = resolve(options);
	begin(d, ROUGH_SHAPE_POLYGON, &o);
	RoughOps outline = {0};
	linear_path_ops(&outline, xy, n, true, &o);
	if (o.fill) {
		RoughPoints poly = {.xy = (double *)xy, .count = n, .capacity = n};
		RoughOps fill = {0};
		if (o.fill_style == ROUGH_FILL_SOLID) {
			solid_fill_polygon(&fill, &poly, 1, &o);
			adopt_set(d, ROUGH_SET_FILL_PATH, &fill);
		} else {
			pattern_fill_polygons(&fill, &poly, 1, &o);
			adopt_set(d, ROUGH_SET_FILL_SKETCH, &fill);
		}
	}
	if (o.stroke)
		adopt_set(d, ROUGH_SET_PATH, &outline);
	else
		ops_free(&outline);
}

/* `_mergedShape': drop every move but the first.  */
static void merged_shape(RoughOps *ops)
{
	size_t kept = 0;
	for (size_t i = 0; i < ops->count; ++i)
		if (i == 0 || ops->ops[i].op != ROUGH_MOVE)
			ops->ops[kept++] = ops->ops[i];
	ops->count = kept;
}

void rough_gen_curve(RoughDrawable *d, const double *xy, size_t n,
                     const RoughOptions *options)
{
	RoughOptions o = resolve(options);
	begin(d, ROUGH_SHAPE_CURVE, &o);
	RoughOps outline = {0};
	curve_outline(&outline, xy, n, &o);
	if (o.fill && n >= 3) {
		RoughOps fill = {0};
		if (o.fill_style == ROUGH_FILL_SOLID) {
			RoughOptions fo = o;
			fo.disable_multi_stroke = true;
			fo.roughness = o.roughness ? (o.roughness +
			                              o.fill_shape_roughness_gain)
			                           : 0;
			curve_outline(&fill, xy, n, &fo);
			merged_shape(&fill);
			adopt_set(d, ROUGH_SET_FILL_PATH, &fill);
		} else {
			RoughPoints bcurve = {0}, poly = {0};
			curve_to_bezier(xy, n, &bcurve);
			points_on_bezier_curves(bcurve.xy, bcurve.count, 10,
			                        (1 + o.roughness) / 2, &poly);
			pattern_fill_polygons(&fill, &poly, 1, &o);
			rough_points_free(&bcurve);
			rough_points_free(&poly);
			adopt_set(d, ROUGH_SET_FILL_SKETCH, &fill);
		}
	}
	if (o.stroke)
		adopt_set(d, ROUGH_SET_PATH, &outline);
	else
		ops_free(&outline);
}

void rough_gen_path(RoughDrawable *d, const RoughPath *path,
                    const RoughOptions *options)
{
	RoughOptions o = resolve(options);
	begin(d, ROUGH_SHAPE_PATH, &o);
	if (!path->count)
		return;
	double distance = (1 + o.roughness) / 2;
	RoughOps outline = {0};
	svg_path_ops(&outline, path, &o);
	if (o.fill) {
		RoughOps fill = {0};
		size_t moves = 0;
		for (size_t i = 0; i < path->count; ++i)
			moves += path->segments[i].key == 'M';
		if (o.fill_style == ROUGH_FILL_SOLID && moves == 1) {
			RoughOptions fo = o;
			fo.disable_multi_stroke = true;
			fo.roughness = o.roughness ? (o.roughness +
			                              o.fill_shape_roughness_gain)
			                           : 0;
			svg_path_ops(&fill, path, &fo);
			merged_shape(&fill);
			adopt_set(d, ROUGH_SET_FILL_PATH, &fill);
		} else {
			RoughPoints *sets = NULL;
			size_t count = points_on_path(path, distance, &sets);
			if (o.fill_style == ROUGH_FILL_SOLID) {
				solid_fill_polygon(&fill, sets, count, &o);
				adopt_set(d, ROUGH_SET_FILL_PATH, &fill);
			} else {
				pattern_fill_polygons(&fill, sets, count, &o);
				adopt_set(d, ROUGH_SET_FILL_SKETCH, &fill);
			}
			for (size_t k = 0; k < count; ++k)
				rough_points_free(&sets[k]);
			free(sets);
		}
	}
	if (o.stroke)
		adopt_set(d, ROUGH_SET_PATH, &outline);
	else
		ops_free(&outline);
}
