/* excal-freehand.c --- Freedraw stroke outlines  -*- c-file-style: "linux" -*- */

#include "excal-freehand.h"

#include <math.h>
#include <stdbool.h>
#include <stdio.h>
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

/* perfect-freehand vec.ts on 2D points.  */

typedef struct {
	double x, y;
} V2;

static V2 v_add(V2 a, V2 b) { return (V2){a.x + b.x, a.y + b.y}; }
static V2 v_sub(V2 a, V2 b) { return (V2){a.x - b.x, a.y - b.y}; }
static V2 v_mul(V2 a, double n) { return (V2){a.x * n, a.y * n}; }
static V2 v_neg(V2 a) { return (V2){-a.x, -a.y}; }
static V2 v_per(V2 a) { return (V2){a.y, -a.x}; }
static double v_dpr(V2 a, V2 b) { return a.x * b.x + a.y * b.y; }
static bool v_equal(V2 a, V2 b) { return a.x == b.x && a.y == b.y; }
static double v_len(V2 a) { return hypot(a.x, a.y); }
static V2 v_uni(V2 a)
{
	double l = v_len(a);
	return (V2){a.x / l, a.y / l};
}
static double v_dist(V2 a, V2 b) { return hypot(a.y - b.y, a.x - b.x); }
static double v_dist2(V2 a, V2 b)
{
	V2 d = v_sub(a, b);
	return d.x * d.x + d.y * d.y;
}
static V2 v_lrp(V2 a, V2 b, double t) { return v_add(a, v_mul(v_sub(b, a), t)); }
static V2 v_prj(V2 a, V2 b, double c) { return v_add(a, v_mul(b, c)); }
static V2 v_rot_around(V2 a, V2 c, double r)
{
	double s = sin(r), co = cos(r);
	double px = a.x - c.x, py = a.y - c.y;
	double nx = px * co - py * s, ny = px * s + py * co;
	return (V2){nx + c.x, ny + c.y};
}

typedef struct {
	V2 point;
	double pressure;
	V2 vector;
	double distance, running_length;
} StrokePoint;

typedef struct {
	double x, y, p; /* p is NAN when undefined.  */
} InPoint;

#define RATE_OF_PRESSURE_CHANGE 0.275
#define FIXED_PI (M_PI + 0.0001)

/* Excalidraw's options.  */
#define THINNING 0.6
#define SMOOTHING 0.5
#define SIZE_FACTOR 4.25

static double easing(double t) { return sin((t * M_PI) / 2); }

static double stroke_radius(double size, double thinning, double pressure)
{
	return size * easing(0.5 - thinning * (0.5 - pressure));
}

/* getStrokePoints with last: true.  Returns the count.  */
static size_t stroke_points(InPoint *pts, size_t n, double size,
                            double streamline, StrokePoint *out)
{
	double t = 0.15 + (1 - streamline) * 0.85;
	size_t count = 0;
	out[count++] = (StrokePoint){
	        .point = {pts[0].x, pts[0].y},
	        .pressure = pts[0].p >= 0 ? pts[0].p : 0.25,
	        .vector = {1, 1},
	};
	bool reached = false;
	double running = 0;
	StrokePoint prev = out[0];
	size_t max = n - 1;
	for (size_t i = 1; i < n; ++i) {
		V2 input = {pts[i].x, pts[i].y};
		V2 point = i == max ? input : v_lrp(prev.point, input, t);
		if (v_equal(prev.point, point))
			continue;
		double distance = v_dist(point, prev.point);
		running += distance;
		if (i < max && !reached) {
			if (running < size)
				continue;
			reached = true;
		}
		StrokePoint sp = {
		        .point = point,
		        .pressure = pts[i].p >= 0 ? pts[i].p : 0.5,
		        .vector = v_uni(v_sub(prev.point, point)),
		        .distance = distance,
		        .running_length = running,
		};
		prev = sp;
		out[count++] = sp;
	}
	out[0].vector = count > 1 ? out[1].vector : (V2){0, 0};
	return count;
}

static void push(RoughPoints *out, V2 p)
{
	rough_points_push(out, p.x, p.y);
}

/* getStrokeOutlinePoints with Excalidraw's options (no tapers, caps on,
   last: true).  */
static void outline_points(const StrokePoint *points, size_t len,
                           double size, bool simulate, RoughPoints *out)
{
	if (len == 0 || size <= 0)
		return;
	const double thinning = THINNING;
	double total = points[len - 1].running_length;
	double min_distance = pow(size * SMOOTHING, 2);
	RoughPoints left = {0}, right = {0};

	double prev_pressure = points[0].pressure;
	for (size_t i = 0; i < len && i < 10; ++i) {
		double pressure = points[i].pressure;
		if (simulate) {
			double sp = fmin(1, points[i].distance / size);
			double rp = fmin(1, 1 - sp);
			pressure = fmin(1, prev_pressure +
			                           (rp - prev_pressure) *
			                                   (sp * RATE_OF_PRESSURE_CHANGE));
		}
		prev_pressure = (prev_pressure + pressure) / 2;
	}

	double radius = stroke_radius(size, thinning, points[len - 1].pressure);
	double first_radius = NAN;
	V2 prev_vector = points[0].vector;
	V2 pl = points[0].point, pr = pl, tl = pl, tr = pr;
	bool prev_sharp = false;

	for (size_t i = 0; i < len; ++i) {
		double pressure = points[i].pressure;
		V2 point = points[i].point, vector = points[i].vector;
		double distance = points[i].distance;
		double running = points[i].running_length;
		if (i < len - 1 && total - running < 3)
			continue;
		if (simulate) {
			double sp = fmin(1, distance / size);
			double rp = fmin(1, 1 - sp);
			pressure = fmin(1, prev_pressure +
			                           (rp - prev_pressure) *
			                                   (sp * RATE_OF_PRESSURE_CHANGE));
		}
		radius = stroke_radius(size, thinning, pressure);
		if (isnan(first_radius))
			first_radius = radius;
		radius = fmax(0.01, radius * fmin(1, 1));

		V2 next_vector = (i < len - 1 ? points[i + 1] : points[i]).vector;
		double next_dpr = i < len - 1 ? v_dpr(vector, next_vector) : 1.0;
		double prev_dpr = v_dpr(vector, prev_vector);
		bool point_sharp = prev_dpr < 0 && !prev_sharp;
		bool next_sharp = next_dpr < 0;

		if (point_sharp || next_sharp) {
			V2 offset = v_mul(v_per(prev_vector), radius);
			const double step = 1.0 / 13;
			for (double t = 0; t <= 1; t += step) {
				tl = v_rot_around(v_sub(point, offset), point,
				                  FIXED_PI * t);
				push(&left, tl);
				tr = v_rot_around(v_add(point, offset), point,
				                  FIXED_PI * -t);
				push(&right, tr);
			}
			pl = tl;
			pr = tr;
			if (next_sharp)
				prev_sharp = true;
			continue;
		}
		prev_sharp = false;

		if (i == len - 1) {
			V2 offset = v_mul(v_per(vector), radius);
			push(&left, v_sub(point, offset));
			push(&right, v_add(point, offset));
			continue;
		}

		V2 offset = v_mul(v_per(v_lrp(next_vector, vector, next_dpr)),
		                  radius);
		tl = v_sub(point, offset);
		if (i <= 1 || v_dist2(pl, tl) > min_distance) {
			push(&left, tl);
			pl = tl;
		}
		tr = v_add(point, offset);
		if (i <= 1 || v_dist2(pr, tr) > min_distance) {
			push(&right, tr);
			pr = tr;
		}
		prev_pressure = pressure;
		prev_vector = vector;
	}

	V2 first = points[0].point;
	V2 last = len > 1 ? points[len - 1].point
	                  : v_add(points[0].point, (V2){1, 1});

	if (len == 1) {
		double r = !isnan(first_radius) && first_radius ? first_radius
		                                                : radius;
		V2 start = v_prj(first, v_uni(v_per(v_sub(first, last))), -r);
		const double step = 1.0 / 13;
		for (double t = step; t <= 1; t += step)
			push(out, v_rot_around(start, first, FIXED_PI * 2 * t));
		rough_points_free(&left);
		rough_points_free(&right);
		return;
	}

	for (size_t i = 0; i < left.count; ++i)
		rough_points_push(out, left.xy[2 * i], left.xy[2 * i + 1]);
	/* End cap.  */
	V2 direction = v_per(v_neg(points[len - 1].vector));
	{
		V2 start = v_prj(last, direction, radius);
		const double step = 1.0 / 29;
		for (double t = step; t < 1; t += step)
			push(out, v_rot_around(start, last, FIXED_PI * 3 * t));
	}
	for (size_t i = right.count; i-- > 0;)
		rough_points_push(out, right.xy[2 * i], right.xy[2 * i + 1]);
	/* Start cap around the first point from the first right point.  */
	if (right.count) {
		V2 r0 = {right.xy[0], right.xy[1]};
		const double step = 1.0 / 13;
		for (double t = step; t <= 1; t += step)
			push(out, v_rot_around(r0, first, FIXED_PI * t));
	}
	rough_points_free(&left);
	rough_points_free(&right);
}

void excal_freehand_variable(const double *xy, size_t n,
                             const double *pressures, size_t pressure_count,
                             int simulate, double stroke_width,
                             double streamline, RoughPoints *out)
{
	/* `element.simulatePressure ? points : points.map(... pressures[i])',
	   or a single [0, 0, 0.5] dot.  */
	bool simulate_input = simulate == 1;
	size_t count = n ? n : (simulate_input ? 0 : 1);
	if (count == 0)
		return;
	/* getStrokePoints may expand two points to five.  */
	InPoint *pts = malloc(sizeof *pts * (count + 4));
	StrokePoint *sp = malloc(sizeof *sp * (count + 4));
	if (!pts || !sp) {
		free(pts);
		free(sp);
		return;
	}
	if (n == 0) {
		pts[0] = (InPoint){0, 0, 0.5};
	} else {
		for (size_t i = 0; i < n; ++i) {
			pts[i].x = xy[2 * i];
			pts[i].y = xy[2 * i + 1];
			pts[i].p = !simulate_input && pressures &&
			                           i < pressure_count
			                   ? pressures[i]
			                   : NAN;
		}
	}
	if (count == 2) {
		InPoint a = pts[0], b = pts[1];
		for (int i = 1; i < 5; ++i) {
			V2 p = v_lrp((V2){a.x, a.y}, (V2){b.x, b.y}, i / 4.0);
			pts[i] = (InPoint){p.x, p.y, NAN};
		}
		count = 5;
	} else if (count == 1) {
		pts[1] = (InPoint){pts[0].x + 1, pts[0].y + 1, pts[0].p};
		count = 2;
	}
	double size = stroke_width * SIZE_FACTOR;
	size_t len = stroke_points(pts, count, size, streamline, sp);
	/* getStrokeOutlinePoints defaults simulatePressure to true when the
	   element does not say.  */
	outline_points(sp, len, size, simulate != 0, out);
	free(pts);
	free(sp);
}

/* @excalidraw/laser-pointer on 2D points; pressure is always 1, so the
   size is constant.  */

static V2 l_rot(V2 a, double rad)
{
	return (V2){cos(rad) * a.x - sin(rad) * a.y,
	            sin(rad) * a.x + cos(rad) * a.y};
}

static V2 l_norm(V2 a)
{
	double m = sqrt(a.x * a.x + a.y * a.y);
	return (V2){a.x / m, a.y / m};
}

static double l_mag(V2 a) { return sqrt(a.x * a.x + a.y * a.y); }

static double l_dist(V2 a, V2 b)
{
	return sqrt((b.x - a.x) * (b.x - a.x) + (b.y - a.y) * (b.y - a.y));
}

static double l_angle(V2 p, V2 p1, V2 p2)
{
	return atan2(p2.y - p.y, p2.x - p.x) - atan2(p1.y - p.y, p1.x - p.x);
}

static double l_norm_angle(double a) { return atan2(sin(a), cos(a)); }

static void push_list(RoughPoints *list, V2 p) { push(list, p); }

void excal_freehand_constant(const double *xy, size_t n, double stroke_width,
                             double streamline, RoughPoints *out)
{
	const double size = stroke_width * 1.4; /* * max(0.1, pressure 1) */
	V2 *points = malloc(sizeof *points * (n ? n : 1));
	if (!points)
		return;
	size_t len = 0;
	for (size_t i = 0; i < n; ++i) {
		V2 p = {xy[2 * i], xy[2 * i + 1]};
		if (i > 0 && xy[2 * i - 2] == p.x && xy[2 * i - 1] == p.y)
			continue;
		if (len > 0 && streamline > 0)
			p = v_add(points[len - 1],
			          v_mul(v_sub(p, points[len - 1]), 1 - streamline));
		points[len++] = p;
	}
	if (len == 0) {
		free(points);
		return;
	}
	if (len == 1) {
		V2 c = points[0];
		if (size >= 0.5) {
			for (double theta = 0; theta <= M_PI * 2;
			     theta += M_PI / 16)
				push(out, v_add(c, v_mul(l_rot((V2){1, 0}, theta),
				                         size)));
			push(out, v_add(c, v_mul((V2){1, 0}, size)));
		}
		free(points);
		return;
	}
	if (len == 2) {
		V2 c = points[0], nn = points[1];
		if (size >= 0.5) {
			size_t start = out->count;
			double pa = l_angle(c, (V2){c.x, c.y - 100}, nn);
			for (double theta = pa; theta <= M_PI + pa;
			     theta += M_PI / 16)
				push(out, v_add(c, v_mul(l_rot((V2){1, 0}, theta),
				                         size)));
			for (double theta = M_PI + pa; theta <= M_PI * 2 + pa;
			     theta += M_PI / 16)
				push(out, v_add(nn, v_mul(l_rot((V2){1, 0}, theta),
				                          size)));
			if (out->count > start)
				rough_points_push(out, out->xy[2 * start],
				                  out->xy[2 * start + 1]);
		}
		free(points);
		return;
	}

	RoughPoints forward = {0}, backward = {0};
	double speed = 0, prev_speed = 0;
	for (size_t i = 1; i + 1 < len; ++i) {
		V2 p = points[i - 1], c = points[i], nx = points[i + 1];
		double d = l_dist(p, c);
		speed = prev_speed + (d - prev_speed) * 0.2;
		double cs = size;
		V2 dir_pc = l_norm(v_sub(p, c));
		V2 dir_nc = l_norm(v_sub(nx, c));
		V2 p1dir_pc = l_rot(dir_pc, M_PI / 2);
		V2 p2dir_pc = l_rot(dir_pc, -M_PI / 2);
		V2 p1dir_nc = l_rot(dir_nc, M_PI / 2);
		V2 p2dir_nc = l_rot(dir_nc, -M_PI / 2);
		V2 p1pc = v_add(c, v_mul(p1dir_pc, cs));
		V2 p2pc = v_add(c, v_mul(p2dir_pc, cs));
		V2 p1nc = v_add(c, v_mul(p1dir_nc, cs));
		V2 p2nc = v_add(c, v_mul(p2dir_nc, cs));
		V2 ftdir = v_add(p1dir_pc, p2dir_nc);
		V2 btdir = v_add(p2dir_pc, p1dir_nc);
		V2 pa_pc = v_add(c, v_mul(l_mag(ftdir) == 0 ? dir_pc
		                                            : l_norm(ftdir),
		                          cs));
		V2 pa_nc = v_add(c, v_mul(l_mag(btdir) == 0 ? dir_nc
		                                            : l_norm(btdir),
		                          cs));
		double c_angle = l_norm_angle(l_angle(c, p, nx));
		double d_angle = (75.0 / 180) * M_PI * (speed > 35 ? 0.5 : 1);
		if (fabs(c_angle) < d_angle) {
			double t_angle = fabs(l_norm_angle(M_PI - c_angle));
			if (t_angle == 0)
				continue;
			if (c_angle < 0) {
				push_list(&backward, p2pc);
				push_list(&backward, pa_nc);
				for (double th = 0; th <= t_angle; th += t_angle / 4)
					push_list(&forward,
					          v_add(c, l_rot(v_mul(p1dir_pc, cs),
					                         th)));
				for (double th = t_angle; th >= 0; th -= t_angle / 4)
					push_list(&backward,
					          v_add(c, l_rot(v_mul(p1dir_pc, cs),
					                         th)));
				push_list(&backward, pa_nc);
				push_list(&backward, p1nc);
			} else {
				push_list(&forward, p1pc);
				push_list(&forward, pa_pc);
				for (double th = 0; th <= t_angle; th += t_angle / 4)
					push_list(&backward,
					          v_add(c, l_rot(v_mul(p1dir_pc, -cs),
					                         -th)));
				for (double th = t_angle; th >= 0; th -= t_angle / 4)
					push_list(&forward,
					          v_add(c, l_rot(v_mul(p1dir_pc, -cs),
					                         -th)));
				push_list(&forward, pa_pc);
				push_list(&forward, p2nc);
			}
		} else {
			push_list(&forward, pa_pc);
			push_list(&backward, pa_nc);
		}
		prev_speed = speed;
	}

	V2 first = points[0], second = points[1];
	V2 penultimate = points[len - 2], ultimate = points[len - 1];
	V2 dir_fs = l_norm(v_sub(second, first));
	V2 dir_pu = l_norm(v_sub(penultimate, ultimate));
	V2 ppdir_fs = l_rot(dir_fs, -M_PI / 2);
	V2 ppdir_pu = l_rot(dir_pu, M_PI / 2);

	/* startCap is built with unshift: reverse of the loop order.  */
	RoughPoints start_cap = {0};
	if (size > 0.1) {
		for (double theta = 0; theta <= M_PI; theta += M_PI / 16)
			push_list(&start_cap,
			          v_add(first, l_rot(v_mul(ppdir_fs, size), -theta)));
		push_list(&start_cap, v_add(first, v_mul(ppdir_fs, -size)));
	} else {
		push_list(&start_cap, first);
	}
	RoughPoints end_cap = {0};
	for (double theta = 0; theta <= M_PI * 3; theta += M_PI / 16)
		push_list(&end_cap,
		          v_add(ultimate, l_rot(v_mul(ppdir_pu, -size), -theta)));

	/* startCap was unshifted when size > 0.1, so emit it reversed.  */
	bool unshifted = size > 0.1;
	for (size_t k = 0; k < start_cap.count; ++k) {
		size_t i = unshifted ? start_cap.count - 1 - k : k;
		rough_points_push(out, start_cap.xy[2 * i],
		                  start_cap.xy[2 * i + 1]);
	}
	for (size_t i = 0; i < forward.count; ++i)
		rough_points_push(out, forward.xy[2 * i], forward.xy[2 * i + 1]);
	for (size_t i = end_cap.count; i-- > 0;)
		rough_points_push(out, end_cap.xy[2 * i], end_cap.xy[2 * i + 1]);
	for (size_t i = backward.count; i-- > 0;)
		rough_points_push(out, backward.xy[2 * i],
		                  backward.xy[2 * i + 1]);
	if (start_cap.count) {
		size_t i = unshifted ? start_cap.count - 1 : 0;
		rough_points_push(out, start_cap.xy[2 * i],
		                  start_cap.xy[2 * i + 1]);
	}
	rough_points_free(&start_cap);
	rough_points_free(&end_cap);
	rough_points_free(&forward);
	rough_points_free(&backward);
	free(points);
}

/* Excalidraw's TO_FIXED_PRECISION regex applied to X's JS string.  */

static double truncate_slow(double x)
{
	char buf[64];
	int precision = 1;
	for (; precision <= 17; ++precision) {
		snprintf(buf, sizeof buf, "%.*e", precision - 1, x);
		if (strtod(buf, NULL) == x)
			break;
	}
	/* buf: [-]d[.ddd]e[+-]XX */
	bool negative = buf[0] == '-';
	const char *m = buf + negative;
	char digits[24];
	int k = 0;
	for (const char *c = m; *c && *c != 'e'; ++c)
		if (*c >= '0' && *c <= '9' && k < 23)
			digits[k++] = *c;
	digits[k] = 0;
	int exponent = atoi(strchr(buf, 'e') + 1);
	int n = exponent + 1; /* Digits before the decimal point.  */
	char js[96];
	if (n > -6 && n <= 21) {
		/* Decimal notation; keep two fraction digits.  */
		char *o = js;
		if (negative)
			*o++ = '-';
		if (n <= 0) {
			*o++ = '0';
			*o++ = '.';
			int frac = 0;
			for (int i = 0; i < -n && frac < 2; ++i, ++frac)
				*o++ = '0';
			for (int i = 0; i < k && frac < 2; ++i, ++frac)
				*o++ = digits[i];
		} else {
			for (int i = 0; i < n; ++i)
				*o++ = i < k ? digits[i] : '0';
			if (k > n) {
				*o++ = '.';
				for (int i = n; i < k && i < n + 2; ++i)
					*o++ = digits[i];
			}
		}
		*o = 0;
		return strtod(js, NULL);
	}
	/* Exponent notation: the regex keeps the mantissa's first two
	   fraction digits and drops the exponent; without a '.' the number
	   is left alone.  */
	if (k == 1)
		return x;
	snprintf(js, sizeof js, "%s%c.%c%c", negative ? "-" : "", digits[0],
	         digits[1], k > 2 ? digits[2] : '0');
	return strtod(js, NULL);
}

double excal_freehand_truncate(double x)
{
	if (!isfinite(x) || x == 0)
		return x;
	double ax = fabs(x);
	if (ax >= 1e-6 && ax < 1e15) {
		double f = x * 100;
		double t = trunc(f);
		/* Unless X * 100 is within rounding of an integer, the truncated
		   decimal string is T / 100, and T / 100 parses to the same
		   double as that string.  */
		double frac = fabs(f - t);
		double eps = 1e-9 * fmax(1, fabs(f));
		if (frac > eps && frac < 1 - eps)
			return t / 100;
	}
	return truncate_slow(x);
}
