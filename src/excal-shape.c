/* excal-shape.c --- Excalidraw element shapes on top of roughjs  -*- c-file-style: "linux" -*- */

#include "excal-shape.h"

#include "excal-freehand.h"

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

/* common/constants.ts */
#define ROUGHNESS_CARTOONIST 2
#define ROUNDNESS_LEGACY 1
#define ROUNDNESS_PROPORTIONAL 2
#define ROUNDNESS_ADAPTIVE 3
#define DEFAULT_PROPORTIONAL_RADIUS 0.25
#define DEFAULT_ADAPTIVE_RADIUS 32
#define LINE_CONFIRM_THRESHOLD 8
#define CROWFOOT_ARROWHEAD_SIZE 15
#define CARDINALITY_MARKER_SIZE 20

static bool str_eq(const char *a, const char *b)
{
	return a && strcmp(a, b) == 0;
}

static bool is_linear(const ExcalElement *e)
{
	return e->type == EXCAL_LINE || e->type == EXCAL_ARROW;
}

static bool can_change_roundness(ExcalType type)
{
	return type == EXCAL_RECTANGLE || type == EXCAL_LINE ||
	       type == EXCAL_DIAMOND;
}

/* The roundness type Excalidraw would have: the element's own, or the
   default for its type when only "rounded" is known.  */
static int roundness_type(const ExcalElement *e)
{
	if (!e->rounded)
		return 0;
	if (e->roundness_type)
		return e->roundness_type;
	return e->type == EXCAL_RECTANGLE ? ROUNDNESS_ADAPTIVE
	                                  : ROUNDNESS_PROPORTIONAL;
}

double excal_corner_radius(double x, int type, double value)
{
	if (type == ROUNDNESS_PROPORTIONAL || type == ROUNDNESS_LEGACY)
		return x * DEFAULT_PROPORTIONAL_RADIUS;
	if (type == ROUNDNESS_ADAPTIVE) {
		double fixed = isnan(value) ? DEFAULT_ADAPTIVE_RADIUS : value;
		double cutoff = fixed / DEFAULT_PROPORTIONAL_RADIUS;
		if (x <= cutoff)
			return x * DEFAULT_PROPORTIONAL_RADIUS;
		return fixed;
	}
	return 0;
}

static double corner_radius(double x, const ExcalElement *e)
{
	return excal_corner_radius(x, roundness_type(e), e->roundness_value);
}

/* tinycolor's alpha == 0, for the colour forms the port understands.  */
static bool is_transparent(const char *c)
{
	if (!c || !*c || strcmp(c, "transparent") == 0)
		return true;
	size_t n = strlen(c);
	if (c[0] == '#' && n == 5)
		return c[4] == '0';
	if (c[0] == '#' && n == 9)
		return c[7] == '0' && c[8] == '0';
	return false;
}

static bool is_path_a_loop(const ExcalElement *e)
{
	size_t n = e->point_count;
	if (n < 3)
		return false;
	const double *p = e->points;
	double d = hypot(p[0] - p[2 * n - 2], p[1] - p[2 * n - 1]);
	return d <= LINE_CONFIRM_THRESHOLD;
}

static double adjust_roughness(const ExcalElement *e)
{
	double r = e->roughness;
	double max_size = fmax(e->width, e->height);
	double min_size = fmin(e->width, e->height);
	if ((min_size >= 20 && max_size >= 50) ||
	    (min_size >= 15 && e->rounded && can_change_roundness(e->type)) ||
	    (is_linear(e) && max_size >= 50))
		return r;
	return fmin(r / (max_size < 10 ? 3 : 2), 2.5);
}

typedef struct {
	RoughOptions o;
	int dash_count;
	double dash[2];
} Options;

/* `generateRoughOptions'.  */
static Options rough_options(const ExcalElement *e, bool continuous)
{
	Options r = {.o = rough_default_options()};
	RoughOptions *o = &r.o;
	double sw = e->stroke_width;
	o->seed = e->seed;
	if (str_eq(e->stroke_style, "dashed")) {
		r.dash_count = 2;
		r.dash[0] = 8;
		r.dash[1] = 8 + sw;
	} else if (str_eq(e->stroke_style, "dotted")) {
		r.dash_count = 2;
		r.dash[0] = 1.5;
		r.dash[1] = 6 + sw;
	}
	o->disable_multi_stroke = !str_eq(e->stroke_style, "solid");
	o->stroke_width = !str_eq(e->stroke_style, "solid") ? sw + 0.5 : sw;
	o->fill_weight = sw / 2;
	o->hachure_gap = sw * 4;
	o->roughness = adjust_roughness(e);
	o->preserve_vertices = continuous || e->roughness < ROUGHNESS_CARTOONIST;
	switch (e->type) {
	case EXCAL_RECTANGLE:
	case EXCAL_DIAMOND:
	case EXCAL_ELLIPSE:
		o->fill_style = rough_fill_style(e->fill_style);
		o->fill = !is_transparent(e->background_color);
		if (e->type == EXCAL_ELLIPSE)
			o->curve_fitting = 1;
		break;
	case EXCAL_LINE:
	case EXCAL_FREEDRAW:
		if (is_path_a_loop(e)) {
			o->fill_style = rough_fill_style(e->fill_style);
			o->fill = e->background_color &&
			          strcmp(e->background_color, "transparent") != 0;
		}
		break;
	default:
		break;
	}
	return r;
}

static ExcalDrawable *add(ExcalShape *s, const Options *opts,
                          ExcalFillSource fill)
{
	if (s->count >= EXCAL_SHAPE_MAX_DRAWABLES)
		return NULL;
	ExcalDrawable *d = &s->items[s->count++];
	memset(d, 0, sizeof *d);
	d->fill_source = fill;
	if (opts) {
		d->dash_count = opts->dash_count;
		d->dash[0] = opts->dash[0];
		d->dash[1] = opts->dash[1];
	}
	return d;
}

/* Local point I of E, or (0, 0) for an element without points.  */
static const double *local_points(const ExcalElement *e, size_t *n)
{
	static const double origin[2] = {0, 0};
	if (e->point_count == 0 || !e->points) {
		*n = 1;
		return origin;
	}
	*n = e->point_count;
	return e->points;
}

/* heading.ts `vectorToHeading' is horizontal.  */
static bool heading_is_horizontal(const double *p, const double *o)
{
	double x = p[0] - o[0], y = p[1] - o[1];
	double ax = fabs(x), ay = fabs(y);
	(void)ax;
	if (x > ay)
		return true;  /* RIGHT */
	if (x <= -ay)
		return true;  /* LEFT */
	return false;         /* DOWN or UP */
}

/* `generateElbowArrowShape' as path segments.  */
static void elbow_arrow_path(const double *pts, size_t n, double radius,
                             RoughPath *path)
{
	rough_path_move(path, pts[0], pts[1]);
	for (size_t i = 1; i + 1 < n; ++i) {
		const double *prev = pts + 2 * (i - 1);
		const double *next = pts + 2 * (i + 1);
		const double *point = pts + 2 * i;
		bool prev_h = heading_is_horizontal(point, prev);
		bool next_h = heading_is_horizontal(next, point);
		double corner = fmin(radius,
		                     fmin(hypot(point[0] - next[0],
		                                point[1] - next[1]) / 2,
		                          hypot(point[0] - prev[0],
		                                point[1] - prev[1]) / 2));
		double a[2], b[2];
		if (prev_h) {
			a[0] = prev[0] < point[0] ? point[0] - corner
			                          : point[0] + corner;
			a[1] = point[1];
		} else {
			a[0] = point[0];
			a[1] = prev[1] < point[1] ? point[1] - corner
			                          : point[1] + corner;
		}
		if (next_h) {
			b[0] = next[0] < point[0] ? point[0] - corner
			                          : point[0] + corner;
			b[1] = point[1];
		} else {
			b[0] = point[0];
			b[1] = next[1] < point[1] ? point[1] - corner
			                          : point[1] + corner;
		}
		rough_path_line(path, a[0], a[1]);
		rough_path_quad(path, point[0], point[1], b[0], b[1]);
	}
	rough_path_line(path, pts[2 * n - 2], pts[2 * n - 1]);
}

/* Arrowheads (bounds.ts).  */

double excal_arrowhead_size(const char *kind)
{
	if (str_eq(kind, "arrow"))
		return 25;
	if (str_eq(kind, "diamond") || str_eq(kind, "diamond_outline"))
		return 12;
	if (str_eq(kind, "cardinality_many") ||
	    str_eq(kind, "cardinality_one_or_many") ||
	    str_eq(kind, "cardinality_zero_or_many"))
		return CROWFOOT_ARROWHEAD_SIZE;
	if (str_eq(kind, "cardinality_one") ||
	    str_eq(kind, "cardinality_exactly_one") ||
	    str_eq(kind, "cardinality_zero_or_one"))
		return CARDINALITY_MARKER_SIZE;
	return 15;
}

double excal_arrowhead_angle(const char *kind)
{
	if (str_eq(kind, "bar"))
		return 90;
	if (str_eq(kind, "arrow"))
		return 20;
	return 25;
}

/* math `pointRotateRads': rotate P about C by ANGLE.  */
static void rotate_about(double px, double py, double cx, double cy,
                         double angle, double *ox, double *oy)
{
	double c = cos(angle), s = sin(angle);
	*ox = (px - cx) * c - (py - cy) * s + cx;
	*oy = (px - cx) * s + (py - cy) * c + cy;
}

int excal_arrowhead_points(const ExcalElement *e, const RoughDrawable *shape0,
                           bool start, const char *kind,
                           double offset_multiplier, double *out)
{
	if (!kind || !shape0)
		return 0;
	const RoughOps *ops = rough_curve_path_ops(shape0);
	if (!ops || ops->count < 2)
		return 0;
	size_t index = start ? 1 : ops->count - 1;
	const RoughOp *op = &ops->ops[index];
	if (op->op != ROUGH_BCURVE_TO)
		return 0;
	double p1[2] = {op->data[0], op->data[1]};
	double p2[2] = {op->data[2], op->data[3]};
	double p3[2] = {op->data[4], op->data[5]};
	const RoughOp *prev = &ops->ops[index - 1];
	double p0[2] = {0, 0};
	if (prev->op == ROUGH_MOVE) {
		p0[0] = prev->data[0];
		p0[1] = prev->data[1];
	} else if (prev->op == ROUGH_BCURVE_TO) {
		p0[0] = prev->data[4];
		p0[1] = prev->data[5];
	}
	double x2 = start ? p0[0] : p3[0], y2 = start ? p0[1] : p3[1];
	double t = 0.3;
	double eq[2];
	for (int k = 0; k < 2; ++k)
		eq[k] = pow(1 - t, 3) * p3[k] + 3 * t * pow(1 - t, 2) * p2[k] +
		        3 * pow(t, 2) * (1 - t) * p1[k] + p0[k] * pow(t, 3);
	double x1 = eq[0], y1 = eq[1];
	double distance = hypot(x2 - x1, y2 - y1);
	if (!(distance > 0) || !isfinite(distance))
		return 0;
	double nx = (x2 - x1) / distance, ny = (y2 - y1) / distance;
	double size = excal_arrowhead_size(kind);

	size_t n;
	const double *pts = local_points(e, &n);
	double cx, cy, px, py;
	if (start) {
		cx = pts[0];
		cy = pts[1];
	} else {
		cx = pts[2 * n - 2];
		cy = pts[2 * n - 1];
	}
	if (n > 1) {
		px = start ? pts[2] : pts[2 * n - 4];
		py = start ? pts[3] : pts[2 * n - 3];
	} else {
		px = py = 0;
	}
	double length = hypot(cx - px, cy - py);
	bool diamond = str_eq(kind, "diamond") || str_eq(kind, "diamond_outline");
	double min_size = fmin(size, length * (diamond ? 0.25 : 0.5));
	double tx = x2 - nx * min_size * offset_multiplier;
	double ty = y2 - ny * min_size * offset_multiplier;
	double xs = tx - nx * min_size, ys = ty - ny * min_size;

	if (str_eq(kind, "circle") || str_eq(kind, "circle_outline")) {
		out[0] = tx;
		out[1] = ty;
		out[2] = hypot(ys - ty, xs - tx) + e->stroke_width - 2;
		return 3;
	}
	double angle = excal_arrowhead_angle(kind);
	if (str_eq(kind, "cardinality_many") ||
	    str_eq(kind, "cardinality_one_or_many")) {
		double x3, y3, x4, y4;
		rotate_about(tx, ty, xs, ys, (-angle * M_PI) / 180, &x3, &y3);
		rotate_about(tx, ty, xs, ys, (angle * M_PI) / 180, &x4, &y4);
		double r[6] = {xs, ys, x3, y3, x4, y4};
		memcpy(out, r, sizeof r);
		return 6;
	}
	double x3, y3, x4, y4;
	rotate_about(xs, ys, tx, ty, (-angle * M_PI) / 180, &x3, &y3);
	rotate_about(xs, ys, tx, ty, (angle * M_PI) / 180, &x4, &y4);
	if (diamond) {
		double ox, oy;
		if (start) {
			double qx = n > 1 ? pts[2] : 0, qy = n > 1 ? pts[3] : 0;
			rotate_about(tx + min_size * 2, ty, tx, ty,
			             atan2(qy - ty, qx - tx), &ox, &oy);
		} else {
			double qx = n > 1 ? pts[2 * n - 4] : 0;
			double qy = n > 1 ? pts[2 * n - 3] : 0;
			rotate_about(tx - min_size * 2, ty, tx, ty,
			             atan2(ty - qy, tx - qx), &ox, &oy);
		}
		double r[8] = {tx, ty, x3, y3, ox, oy, x4, y4};
		memcpy(out, r, sizeof r);
		return 8;
	}
	double r[6] = {tx, ty, x3, y3, x4, y4};
	memcpy(out, r, sizeof r);
	return 6;
}

/* `getArrowheadLineOptions'.  */
static Options arrowhead_line_options(const ExcalElement *e,
                                      const Options *options)
{
	Options r = *options;
	if (str_eq(e->stroke_style, "dotted")) {
		double sw = e->stroke_width - 1;
		r.dash_count = 2;
		r.dash[0] = 1.5;
		r.dash[1] = (6 + sw) - 1;
	} else {
		r.dash_count = 0;
	}
	r.o.roughness = fmin(1, r.o.roughness);
	return r;
}

static void head_line(ExcalShape *s, double x1, double y1, double x2,
                      double y2, const Options *lo)
{
	ExcalDrawable *d = add(s, lo, EXCAL_FILL_ELEMENT);
	if (d)
		rough_gen_line(&d->rough, x1, y1, x2, y2, &lo->o);
}

static void head_lines_to_tip(ExcalShape *s, const double *p, int n,
                              const Options *lo)
{
	if (n < 6)
		return;
	head_line(s, p[2], p[3], p[0], p[1], lo);
	head_line(s, p[4], p[5], p[0], p[1], lo);
}

static void head_cardinality_one(ExcalShape *s, const double *p, int n,
                                 const Options *lo)
{
	if (n < 6)
		return;
	head_line(s, p[2], p[3], p[4], p[5], lo);
}

static void head_circle(ExcalShape *s, const Options *options,
                        const double *p, int n, ExcalFillSource fill,
                        double scale)
{
	if (n < 3)
		return;
	Options co = *options;
	co.o.fill = true;
	co.o.fill_style = ROUGH_FILL_SOLID;
	co.o.stroke = true;
	co.o.roughness = fmin(0.5, options->o.roughness);
	co.dash_count = 0;
	ExcalDrawable *d = add(s, &co, fill);
	if (d)
		rough_gen_circle(&d->rough, p[0], p[1], p[2] * scale, &co.o);
}

static void head_polygon(ExcalShape *s, const Options *options,
                         const double *p, int n, ExcalFillSource fill)
{
	Options po = *options;
	po.o.fill = true;
	po.o.fill_style = ROUGH_FILL_SOLID;
	po.o.roughness = fmin(1, options->o.roughness);
	po.dash_count = 0;
	double pts[12];
	int count = 0;
	for (int i = 0; i + 1 < n && count < 5; i += 2, ++count) {
		pts[2 * count] = p[i];
		pts[2 * count + 1] = p[i + 1];
	}
	pts[2 * count] = p[0];
	pts[2 * count + 1] = p[1];
	++count;
	ExcalDrawable *d = add(s, &po, fill);
	if (d)
		rough_gen_polygon(&d->rough, pts, (size_t)count, &po.o);
}

/* `getArrowheadShapes'.  */
static void arrowhead_shapes(ExcalShape *s, const ExcalElement *e,
                             bool start, const char *head,
                             const Options *options)
{
	if (!head || s->count == 0)
		return;
	const RoughDrawable *shape0 = &s->items[0].rough;
	double p[8];
	int n;
	if (str_eq(head, "circle") || str_eq(head, "circle_outline")) {
		n = excal_arrowhead_points(e, shape0, start, head, 0, p);
		head_circle(s, options, p, n,
		            str_eq(head, "circle_outline") ? EXCAL_FILL_CANVAS
		                                           : EXCAL_FILL_STROKE,
		            1);
	} else if (str_eq(head, "triangle") ||
	           str_eq(head, "triangle_outline")) {
		n = excal_arrowhead_points(e, shape0, start, head, 0, p);
		if (n == 6)
			head_polygon(s, options, p, 6,
			             str_eq(head, "triangle_outline")
			                     ? EXCAL_FILL_CANVAS
			                     : EXCAL_FILL_STROKE);
	} else if (str_eq(head, "diamond") || str_eq(head, "diamond_outline")) {
		n = excal_arrowhead_points(e, shape0, start, head, 0, p);
		if (n == 8)
			head_polygon(s, options, p, 8,
			             str_eq(head, "diamond_outline")
			                     ? EXCAL_FILL_CANVAS
			                     : EXCAL_FILL_STROKE);
	} else if (str_eq(head, "cardinality_one")) {
		Options lo = arrowhead_line_options(e, options);
		n = excal_arrowhead_points(e, shape0, start, head, 0, p);
		head_cardinality_one(s, p, n, &lo);
	} else if (str_eq(head, "cardinality_many")) {
		Options lo = arrowhead_line_options(e, options);
		n = excal_arrowhead_points(e, shape0, start, head, 0, p);
		head_lines_to_tip(s, p, n, &lo);
	} else if (str_eq(head, "cardinality_one_or_many")) {
		Options lo = arrowhead_line_options(e, options);
		n = excal_arrowhead_points(e, shape0, start, "cardinality_many",
		                           0, p);
		head_lines_to_tip(s, p, n, &lo);
		n = excal_arrowhead_points(e, shape0, start, "cardinality_one",
		                           -0.25, p);
		head_cardinality_one(s, p, n, &lo);
	} else if (str_eq(head, "cardinality_exactly_one")) {
		Options lo = arrowhead_line_options(e, options);
		n = excal_arrowhead_points(e, shape0, start, "cardinality_one",
		                           -0.5, p);
		head_cardinality_one(s, p, n, &lo);
		n = excal_arrowhead_points(e, shape0, start, "cardinality_one",
		                           0, p);
		head_cardinality_one(s, p, n, &lo);
	} else if (str_eq(head, "cardinality_zero_or_one")) {
		Options lo = arrowhead_line_options(e, options);
		n = excal_arrowhead_points(e, shape0, start, "circle_outline",
		                           1.5, p);
		head_circle(s, options, p, n, EXCAL_FILL_CANVAS, 0.8);
		n = excal_arrowhead_points(e, shape0, start, "cardinality_one",
		                           -0.5, p);
		head_cardinality_one(s, p, n, &lo);
	} else if (str_eq(head, "cardinality_zero_or_many")) {
		Options lo = arrowhead_line_options(e, options);
		n = excal_arrowhead_points(e, shape0, start, "cardinality_many",
		                           0, p);
		head_lines_to_tip(s, p, n, &lo);
		n = excal_arrowhead_points(e, shape0, start, "circle_outline",
		                           1.5, p);
		head_circle(s, options, p, n, EXCAL_FILL_CANVAS, 0.8);
	} else {
		/* "arrow", "bar" and unknown kinds.  */
		Options lo = arrowhead_line_options(e, options);
		n = excal_arrowhead_points(e, shape0, start, head, 0, p);
		head_lines_to_tip(s, p, n, &lo);
	}
}

/* bounds.ts `getCubicBezierCurveBound' helpers.  */

static double bezier_value(double t, double p0, double p1, double p2,
                           double p3)
{
	double m = 1 - t;
	return pow(m, 3) * p0 + 3 * pow(m, 2) * t * p1 + 3 * m * pow(t, 2) * p2 +
	       pow(t, 3) * p3;
}

static void cubic_extent(double p0, double p1, double p2, double p3,
                         double *lo, double *hi)
{
	*lo = fmin(p0, p3);
	*hi = fmax(p0, p3);
	double i = p1 - p0, j = p2 - p1, k = p3 - p2;
	double a = 3 * i - 6 * j + 3 * k;
	double b = 6 * j - 6 * i;
	double c = 3 * i;
	double disc = b * b - 4 * a * c;
	if (!(disc >= 0))
		return;
	double t1, t2;
	if (a == 0) {
		t1 = t2 = -c / b;
	} else {
		t1 = (-b + sqrt(disc)) / (2 * a);
		t2 = (-b - sqrt(disc)) / (2 * a);
	}
	if (t1 >= 0 && t1 <= 1) {
		double v = bezier_value(t1, p0, p1, p2, p3);
		*lo = fmin(*lo, v);
		*hi = fmax(*hi, v);
	}
	if (t2 >= 0 && t2 <= 1) {
		double v = bezier_value(t2, p0, p1, p2, p3);
		*lo = fmin(*lo, v);
		*hi = fmax(*hi, v);
	}
}

/* `getMinMaxXYFromCurvePathOps'.  Returns false when there is no curve.  */
static bool curve_ops_bounds(const RoughOps *ops, double *x1, double *y1,
                             double *x2, double *y2)
{
	double cx = 0, cy = 0;
	double minx = INFINITY, miny = INFINITY, maxx = -INFINITY,
	       maxy = -INFINITY;
	for (size_t i = 0; ops && i < ops->count; ++i) {
		const RoughOp *op = &ops->ops[i];
		if (op->op == ROUGH_MOVE) {
			cx = op->data[0];
			cy = op->data[1];
		} else if (op->op == ROUGH_BCURVE_TO) {
			double lo, hi;
			cubic_extent(cx, op->data[0], op->data[2], op->data[4],
			             &lo, &hi);
			minx = fmin(minx, lo);
			maxx = fmax(maxx, hi);
			cubic_extent(cy, op->data[1], op->data[3], op->data[5],
			             &lo, &hi);
			miny = fmin(miny, lo);
			maxy = fmax(maxy, hi);
			cx = op->data[4];
			cy = op->data[5];
		}
	}
	if (!(minx <= maxx && miny <= maxy))
		return false;
	*x1 = minx, *y1 = miny, *x2 = maxx, *y2 = maxy;
	return true;
}

static void points_bounds(const ExcalElement *e, double *x1, double *y1,
                          double *x2, double *y2)
{
	size_t n;
	const double *p = local_points(e, &n);
	*x1 = *x2 = p[0];
	*y1 = *y2 = p[1];
	for (size_t i = 1; i < n; ++i) {
		*x1 = fmin(*x1, p[2 * i]);
		*y1 = fmin(*y1, p[2 * i + 1]);
		*x2 = fmax(*x2, p[2 * i]);
		*y2 = fmax(*y2, p[2 * i + 1]);
	}
}

static void generate_linear(const ExcalElement *e, ExcalShape *s)
{
	size_t n;
	const double *pts = local_points(e, &n);
	Options options = rough_options(e, false);
	ExcalDrawable *d;
	if (e->elbowed) {
		bool huge = false;
		for (size_t i = 0; i < 2 * n; ++i)
			huge |= fabs(pts[i]) > 1e6;
		if (huge)
			return;
		Options eo = rough_options(e, true);
		RoughPath path = {0};
		elbow_arrow_path(pts, n, 16, &path);
		d = add(s, &eo, EXCAL_FILL_ELEMENT);
		rough_gen_path(&d->rough, &path, &eo.o);
		rough_path_free(&path);
	} else if (!e->rounded) {
		d = add(s, &options, EXCAL_FILL_ELEMENT);
		if (options.o.fill)
			rough_gen_polygon(&d->rough, pts, n, &options.o);
		else
			rough_gen_linear_path(&d->rough, pts, n, &options.o);
	} else {
		d = add(s, &options, EXCAL_FILL_ELEMENT);
		rough_gen_curve(&d->rough, pts, n, &options.o);
	}
	if (e->type == EXCAL_ARROW) {
		arrowhead_shapes(s, e, true, e->start_arrowhead, &options);
		arrowhead_shapes(s, e, false, e->end_arrowhead, &options);
	}
}

static void generate_freedraw(const ExcalElement *e, ExcalShape *s)
{
	s->butt_caps = true;
	if (is_path_a_loop(e)) {
		Options options = rough_options(e, false);
		options.o.stroke = false;
		RoughPoints simplified = {0};
		rough_simplify(e->points, e->point_count, 0.75, &simplified);
		ExcalDrawable *d = add(s, &options, EXCAL_FILL_ELEMENT);
		rough_gen_curve(&d->rough, simplified.xy, simplified.count,
		                &options.o);
		rough_points_free(&simplified);
	}
	RoughPoints raw = {0};
	if (e->constant_width)
		excal_freehand_constant(e->points, e->point_count,
		                        e->stroke_width, e->streamline, &raw);
	else
		excal_freehand_variable(e->points, e->point_count, e->pressures,
		                        e->pressure_count, e->simulate_pressure,
		                        e->stroke_width, e->streamline, &raw);
	/* getSvgPathFromStroke: "M p0 Q p0 mid(p0,p1) p1 mid(p1,p2) ...
	   pN mid(pN,p0) L p0 Z", every number truncated to 2 decimals.
	   Store the truncated control/end points pairwise.  */
	size_t m = raw.count;
	for (size_t i = 0; i < m; ++i) {
		const double *p = raw.xy + 2 * i;
		const double *q = raw.xy + 2 * ((i + 1) % m);
		if (!isfinite(p[0]) || !isfinite(p[1]) || !isfinite(q[0]) ||
		    !isfinite(q[1]))
			continue;
		rough_points_push(&s->outline, excal_freehand_truncate(p[0]),
		                  excal_freehand_truncate(p[1]));
		rough_points_push(&s->outline,
		                  excal_freehand_truncate((p[0] + q[0]) / 2),
		                  excal_freehand_truncate((p[1] + q[1]) / 2));
	}
	rough_points_free(&raw);
}

void excal_shape_generate(const ExcalElement *e, ExcalShape *s)
{
	memset(s, 0, sizeof *s);
	double w = e->width, h = e->height;
	ExcalDrawable *d;
	switch (e->type) {
	case EXCAL_RECTANGLE:
		if (e->rounded) {
			double r = corner_radius(fmin(w, h), e);
			Options o = rough_options(e, true);
			RoughPath p = {0};
			rough_path_move(&p, r, 0);
			rough_path_line(&p, w - r, 0);
			rough_path_quad(&p, w, 0, w, r);
			rough_path_line(&p, w, h - r);
			rough_path_quad(&p, w, h, w - r, h);
			rough_path_line(&p, r, h);
			rough_path_quad(&p, 0, h, 0, h - r);
			rough_path_line(&p, 0, r);
			rough_path_quad(&p, 0, 0, r, 0);
			d = add(s, &o, EXCAL_FILL_ELEMENT);
			rough_gen_path(&d->rough, &p, &o.o);
			rough_path_free(&p);
		} else {
			Options o = rough_options(e, false);
			d = add(s, &o, EXCAL_FILL_ELEMENT);
			rough_gen_rectangle(&d->rough, 0, 0, w, h, &o.o);
		}
		break;
	case EXCAL_DIAMOND: {
		double top_x = floor(w / 2) + 1, top_y = 0;
		double right_x = w, right_y = floor(h / 2) + 1;
		double bottom_x = top_x, bottom_y = h;
		double left_x = 0, left_y = right_y;
		if (e->rounded) {
			double vr = corner_radius(fabs(top_x - left_x), e);
			double hr = corner_radius(fabs(right_y - top_y), e);
			Options o = rough_options(e, true);
			RoughPath p = {0};
			rough_path_move(&p, top_x + vr, top_y + hr);
			rough_path_line(&p, right_x - vr, right_y - hr);
			rough_path_cubic(&p, right_x, right_y, right_x, right_y,
			                 right_x - vr, right_y + hr);
			rough_path_line(&p, bottom_x + vr, bottom_y - hr);
			rough_path_cubic(&p, bottom_x, bottom_y, bottom_x,
			                 bottom_y, bottom_x - vr, bottom_y - hr);
			rough_path_line(&p, left_x + vr, left_y + hr);
			rough_path_cubic(&p, left_x, left_y, left_x, left_y,
			                 left_x + vr, left_y - hr);
			rough_path_line(&p, top_x - vr, top_y + hr);
			rough_path_cubic(&p, top_x, top_y, top_x, top_y,
			                 top_x + vr, top_y + hr);
			d = add(s, &o, EXCAL_FILL_ELEMENT);
			rough_gen_path(&d->rough, &p, &o.o);
			rough_path_free(&p);
		} else {
			double pts[8] = {top_x,    top_y,    right_x, right_y,
			                 bottom_x, bottom_y, left_x,  left_y};
			Options o = rough_options(e, false);
			d = add(s, &o, EXCAL_FILL_ELEMENT);
			rough_gen_polygon(&d->rough, pts, 4, &o.o);
		}
		break;
	}
	case EXCAL_ELLIPSE: {
		Options o = rough_options(e, false);
		d = add(s, &o, EXCAL_FILL_ELEMENT);
		rough_gen_ellipse(&d->rough, w / 2, h / 2, w, h, &o.o);
		break;
	}
	case EXCAL_LINE:
	case EXCAL_ARROW:
		generate_linear(e, s);
		break;
	case EXCAL_FREEDRAW:
		generate_freedraw(e, s);
		break;
	default:
		break;
	}

	/* getElementAbsoluteCoords, relative to x, y.  */
	s->x1 = 0, s->y1 = 0, s->x2 = w, s->y2 = h;
	if (e->type == EXCAL_FREEDRAW) {
		points_bounds(e, &s->x1, &s->y1, &s->x2, &s->y2);
	} else if (is_linear(e)) {
		if (s->count == 0 ||
		    !curve_ops_bounds(rough_curve_path_ops(&s->items[0].rough),
		                      &s->x1, &s->y1, &s->x2, &s->y2))
			points_bounds(e, &s->x1, &s->y1, &s->x2, &s->y2);
	}
}

void excal_shape_free(ExcalShape *s)
{
	for (int i = 0; i < s->count; ++i)
		rough_drawable_free(&s->items[i].rough);
	s->count = 0;
	rough_points_free(&s->outline);
}

double excal_shape_padding(const ExcalElement *e)
{
	double sw = e->stroke_width > 0 ? e->stroke_width : 0;
	double r = e->roughness > 0 ? e->roughness : 0;
	double w = e->width, h = e->height;
	double overshoot = 0, diag = hypot(w, h);
	if (e->point_count > 0) {
		double x1, y1, x2, y2;
		points_bounds(e, &x1, &y1, &x2, &y2);
		diag = hypot(x2 - x1, y2 - y1);
		/* A Catmull-Rom segment stays in the hull of control points
		   at most |p[i+1] - p[i-1]| / 6 from its vertices.  */
		if (e->rounded && e->type != EXCAL_FREEDRAW)
			for (size_t i = 1; i + 1 < e->point_count; ++i) {
				const double *p = e->points + 2 * (i - 1);
				overshoot = fmax(overshoot,
				                 hypot(p[4] - p[0], p[5] - p[1]) /
				                         6);
			}
	}
	/* Endpoint and control jitter (at most ~2.3 * roughness, fills
	   use roughness + 0.8), plus bowing up to roughness * length / 100.  */
	double jitter = (r + 0.8) * 5 + r * diag / 100;
	double pad = 2 + sw + jitter + 2 * overshoot;
	if (e->type == EXCAL_ARROW)
		pad += 30 + sw;
	if (e->type == EXCAL_FREEDRAW)
		pad += sw * 4.25 + 3;
	if (e->type == EXCAL_TEXT || e->type == EXCAL_UNKNOWN)
		pad += 30;
	return pad;
}
