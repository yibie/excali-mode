/* excal-fill.c --- Bucket-fill regions  -*- c-file-style: "linux" -*-
 * Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Upstream's bucketFill.ts finds the smallest closed region under the
 * click from nearby element outlines, bridging gaps up to
 * BUCKET_FILL_GAP_TOLERANCE and turning islands into holes joined by
 * keyhole bridges.  This does the same on a raster: the outlines are
 * drawn into a grid twice, as thin center lines and grown by the gap
 * tolerance; the click floods the free cells, the flood then grows back
 * up to the thin lines (so bridged gaps do not shrink the region), and
 * the cell boundary is traced, simplified and keyholed into one polygon.
 */

#include "excal-fill.h"

#include <cairo.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

enum { FREE = 0, THICK = 1, THIN = 2, REGION = 3, GROWN = 4 };

typedef struct {
	double *xy;
	size_t count, capacity;
} Poly;

static bool poly_add(Poly *p, double x, double y)
{
	if (p->count == p->capacity) {
		size_t capacity = p->capacity ? p->capacity * 2 : 64;
		double *grown = realloc(p->xy, capacity * 2 * sizeof *grown);
		if (!grown)
			return false;
		p->xy = grown;
		p->capacity = capacity;
	}
	p->xy[2 * p->count] = x;
	p->xy[2 * p->count + 1] = y;
	p->count++;
	return true;
}

/* Draw WALLS into an A8 surface of GRID with lines WIDTH scene units
   wide, and OR VALUE into MASK where they cover.  */
static bool rasterize(const ExcalFillWall *walls, size_t nwalls,
                      const ExcalFillGrid *grid, double width,
                      uint8_t *mask, uint8_t value)
{
	cairo_surface_t *surface =
	        cairo_image_surface_create(CAIRO_FORMAT_A8, grid->gw, grid->gh);
	if (cairo_surface_status(surface) != CAIRO_STATUS_SUCCESS) {
		cairo_surface_destroy(surface);
		return false;
	}
	cairo_t *cr = cairo_create(surface);
	cairo_set_antialias(cr, CAIRO_ANTIALIAS_NONE);
	cairo_scale(cr, 1 / grid->cell, 1 / grid->cell);
	cairo_translate(cr, -grid->x, -grid->y);
	cairo_set_line_width(cr, width);
	cairo_set_line_cap(cr, CAIRO_LINE_CAP_ROUND);
	cairo_set_line_join(cr, CAIRO_LINE_JOIN_ROUND);
	for (size_t w = 0; w < nwalls; ++w) {
		const ExcalFillWall *wall = &walls[w];
		if (wall->count == 0)
			continue;
		cairo_move_to(cr, wall->points[0], wall->points[1]);
		for (size_t i = 1; i < wall->count; ++i)
			cairo_line_to(cr, wall->points[2 * i],
			              wall->points[2 * i + 1]);
		if (wall->count == 1)
			cairo_line_to(cr, wall->points[0], wall->points[1]);
		if (wall->closed)
			cairo_close_path(cr);
		cairo_stroke(cr);
	}
	cairo_destroy(cr);
	cairo_surface_flush(surface);
	const unsigned char *data = cairo_image_surface_get_data(surface);
	int stride = cairo_image_surface_get_stride(surface);
	for (int y = 0; y < grid->gh; ++y)
		for (int x = 0; x < grid->gw; ++x)
			if (data[(size_t)y * stride + x])
				mask[(size_t)y * grid->gw + x] = value;
	cairo_surface_destroy(surface);
	return true;
}

/* Flood the FREE cells 4-connected to START with REGION; return false
   when the flood reaches the grid edge.  */
static bool flood(uint8_t *mask, int gw, int gh, size_t start, size_t *stack)
{
	size_t top = 0;
	bool bounded = true;
	mask[start] = REGION;
	stack[top++] = start;
	while (top) {
		size_t i = stack[--top];
		int x = (int)(i % gw), y = (int)(i / gw);
		if (x == 0 || y == 0 || x == gw - 1 || y == gh - 1)
			bounded = false;
		size_t next[4];
		int n = 0;
		if (x > 0)
			next[n++] = i - 1;
		if (x < gw - 1)
			next[n++] = i + 1;
		if (y > 0)
			next[n++] = i - gw;
		if (y < gh - 1)
			next[n++] = i + gw;
		for (int k = 0; k < n; ++k)
			if (mask[next[k]] == FREE) {
				mask[next[k]] = REGION;
				stack[top++] = next[k];
			}
	}
	return bounded;
}

/* Grow REGION by STEPS cells (8-connected, so corners fill square)
   into THICK cells, never into THIN ones.  */
static void grow(uint8_t *mask, int gw, int gh, int steps)
{
	size_t n = (size_t)gw * gh;
	for (int s = 0; s < steps; ++s) {
		bool changed = false;
		for (size_t i = 0; i < n; ++i) {
			if (mask[i] != THICK)
				continue;
			int x = (int)(i % gw), y = (int)(i / gw);
			bool near = false;
			for (int dy = -1; dy <= 1 && !near; ++dy)
				for (int dx = -1; dx <= 1 && !near; ++dx) {
					int nx = x + dx, ny = y + dy;
					near = nx >= 0 && ny >= 0 && nx < gw &&
					       ny < gh &&
					       mask[(size_t)ny * gw + nx] == REGION;
				}
			if (near) {
				mask[i] = GROWN;
				changed = true;
			}
		}
		for (size_t i = 0; i < n; ++i)
			if (mask[i] == GROWN)
				mask[i] = REGION;
		if (!changed)
			break;
	}
}

/* Directions: 0 +x, 1 +y, 2 -x, 3 -y (y grows downward).  Boundary
   edges run clockwise around region cells, which lie on their right.  */
static const int DX[4] = {1, 0, -1, 0};
static const int DY[4] = {0, 1, 0, -1};

static int popcount4(uint8_t bits)
{
	return (bits & 1) + ((bits >> 1) & 1) + ((bits >> 2) & 1) +
	       ((bits >> 3) & 1);
}

/* Trace the loop from vertex START along its only edge; append its
   corners to LOOP in cell units.  */
static bool trace(uint8_t *out, int vw, size_t start, Poly *loop)
{
	size_t v = start;
	int dir = -1;
	do {
		uint8_t bits = out[v];
		int next = -1;
		if (dir < 0) {
			for (int d = 0; d < 4 && next < 0; ++d)
				if (bits & (1 << d))
					next = d;
		} else {
			/* Prefer staying around the same cell at pinch points:
			   right turn, straight, left turn.  */
			const int order[3] = {(dir + 1) % 4, dir, (dir + 3) % 4};
			for (int k = 0; k < 3 && next < 0; ++k)
				if (bits & (1 << order[k]))
					next = order[k];
		}
		if (next < 0)
			return false;
		if (next != dir &&
		    !poly_add(loop, (double)(v % vw), (double)(v / vw)))
			return false;
		out[v] &= (uint8_t)~(1 << next);
		dir = next;
		v = (size_t)((long)v + DX[dir] + (long)DY[dir] * vw);
	} while (v != start);
	return true;
}

static double loop_area(const Poly *p)
{
	double a = 0;
	for (size_t i = 0; i < p->count; ++i) {
		size_t j = (i + 1) % p->count;
		a += p->xy[2 * i] * p->xy[2 * j + 1] -
		     p->xy[2 * j] * p->xy[2 * i + 1];
	}
	return a / 2;
}

static double segment_distance(const double *p, const double *a,
                               const double *b)
{
	double dx = b[0] - a[0], dy = b[1] - a[1];
	double len = dx * dx + dy * dy;
	double t = len > 0 ? ((p[0] - a[0]) * dx + (p[1] - a[1]) * dy) / len : 0;
	t = fmax(0, fmin(1, t));
	double ex = a[0] + t * dx - p[0], ey = a[1] + t * dy - p[1];
	return sqrt(ex * ex + ey * ey);
}

/* Douglas-Peucker on the open run FROM..TO of P, marking KEEP.  */
static void simplify_run(const Poly *p, size_t from, size_t to,
                         double tolerance, bool *keep, size_t *stack)
{
	size_t top = 0;
	stack[top++] = from;
	stack[top++] = to;
	while (top) {
		size_t b = stack[--top], a = stack[--top];
		double worst = 0;
		size_t index = a;
		for (size_t i = a + 1; i < b; ++i) {
			double d = segment_distance(&p->xy[2 * (i % p->count)],
			                            &p->xy[2 * (a % p->count)],
			                            &p->xy[2 * (b % p->count)]);
			if (d > worst) {
				worst = d;
				index = i;
			}
		}
		if (worst > tolerance) {
			keep[index % p->count] = true;
			stack[top++] = a;
			stack[top++] = index;
			stack[top++] = index;
			stack[top++] = b;
		}
	}
}

/* Simplify closed loop P in place to within TOLERANCE.  */
static bool simplify_loop(Poly *p, double tolerance)
{
	if (p->count < 4)
		return true;
	bool *keep = calloc(p->count, sizeof *keep);
	size_t *stack = malloc(4 * (p->count + 1) * sizeof *stack);
	if (!keep || !stack) {
		free(keep);
		free(stack);
		return false;
	}
	/* Anchor on vertex 0 and the vertex farthest from it.  */
	size_t far = 0;
	double best = -1;
	for (size_t i = 1; i < p->count; ++i) {
		double dx = p->xy[2 * i] - p->xy[0], dy = p->xy[2 * i + 1] - p->xy[1];
		if (dx * dx + dy * dy > best) {
			best = dx * dx + dy * dy;
			far = i;
		}
	}
	keep[0] = keep[far] = true;
	simplify_run(p, 0, far, tolerance, keep, stack);
	simplify_run(p, far, p->count, tolerance, keep, stack);
	size_t n = 0;
	for (size_t i = 0; i < p->count; ++i)
		if (keep[i]) {
			p->xy[2 * n] = p->xy[2 * i];
			p->xy[2 * n + 1] = p->xy[2 * i + 1];
			n++;
		}
	p->count = n;
	free(keep);
	free(stack);
	return true;
}

/* Splice HOLE into OUTER at their closest vertices.  */
static bool keyhole(Poly *outer, const Poly *hole)
{
	size_t bi = 0, bj = 0;
	double best = INFINITY;
	for (size_t i = 0; i < outer->count; ++i)
		for (size_t j = 0; j < hole->count; ++j) {
			double dx = outer->xy[2 * i] - hole->xy[2 * j];
			double dy = outer->xy[2 * i + 1] - hole->xy[2 * j + 1];
			if (dx * dx + dy * dy < best) {
				best = dx * dx + dy * dy;
				bi = i;
				bj = j;
			}
		}
	Poly result = {0};
	bool ok = true;
	for (size_t i = 0; i <= bi && ok; ++i)
		ok = poly_add(&result, outer->xy[2 * i], outer->xy[2 * i + 1]);
	for (size_t k = 0; k <= hole->count && ok; ++k) {
		size_t j = (bj + k) % hole->count;
		ok = poly_add(&result, hole->xy[2 * j], hole->xy[2 * j + 1]);
	}
	for (size_t i = bi; i < outer->count && ok; ++i)
		ok = poly_add(&result, outer->xy[2 * i], outer->xy[2 * i + 1]);
	if (!ok) {
		free(result.xy);
		return false;
	}
	free(outer->xy);
	*outer = result;
	return true;
}

ExcalFillStatus excal_fill_region(const ExcalFillWall *walls, size_t nwalls,
                                  const ExcalFillGrid *grid, double px,
                                  double py, double gap, double tolerance,
                                  double **out, size_t *count)
{
	int gw = grid->gw, gh = grid->gh;
	if (gw < 3 || gh < 3 || grid->cell <= 0)
		return EXCAL_FILL_FAILED;
	int cx = (int)floor((px - grid->x) / grid->cell);
	int cy = (int)floor((py - grid->y) / grid->cell);
	if (cx <= 0 || cy <= 0 || cx >= gw - 1 || cy >= gh - 1)
		return EXCAL_FILL_UNBOUNDED;
	size_t n = (size_t)gw * gh;
	int vw = gw + 1, vh = gh + 1;
	uint8_t *mask = calloc(n, 1);
	uint8_t *edges = calloc((size_t)vw * vh, 1);
	size_t *stack = malloc(n * sizeof *stack);
	Poly *loops = NULL;
	size_t nloops = 0;
	ExcalFillStatus status = EXCAL_FILL_FAILED;
	if (!mask || !edges || !stack)
		goto done;
	if (!rasterize(walls, nwalls, grid, fmax(gap, grid->cell), mask, THICK) ||
	    !rasterize(walls, nwalls, grid, 1.5 * grid->cell, mask, THIN))
		goto done;
	size_t start = (size_t)cy * gw + cx;
	if (mask[start] != FREE) {
		status = EXCAL_FILL_ON_WALL;
		goto done;
	}
	if (!flood(mask, gw, gh, start, stack)) {
		status = EXCAL_FILL_UNBOUNDED;
		goto done;
	}
	grow(mask, gw, gh, (int)ceil(gap / 2 / grid->cell));

	/* Boundary edges, clockwise around region cells.  */
	for (int y = 0; y < gh; ++y)
		for (int x = 0; x < gw; ++x) {
			if (mask[(size_t)y * gw + x] != REGION)
				continue;
			bool up = y > 0 && mask[(size_t)(y - 1) * gw + x] == REGION;
			bool down = y < gh - 1 &&
			            mask[(size_t)(y + 1) * gw + x] == REGION;
			bool left = x > 0 && mask[(size_t)y * gw + x - 1] == REGION;
			bool right = x < gw - 1 &&
			             mask[(size_t)y * gw + x + 1] == REGION;
			if (!up)
				edges[(size_t)y * vw + x] |= 1 << 0;
			if (!right)
				edges[(size_t)y * vw + x + 1] |= 1 << 1;
			if (!down)
				edges[(size_t)(y + 1) * vw + x + 1] |= 1 << 2;
			if (!left)
				edges[(size_t)(y + 1) * vw + x] |= 1 << 3;
		}
	size_t capacity = 0;
	for (size_t v = 0; v < (size_t)vw * vh; ++v) {
		if (popcount4(edges[v]) != 1)
			continue;
		if (nloops == capacity) {
			capacity = capacity ? capacity * 2 : 8;
			Poly *grown = realloc(loops, capacity * sizeof *grown);
			if (!grown)
				goto done;
			loops = grown;
		}
		loops[nloops] = (Poly){0};
		if (!trace(edges, vw, v, &loops[nloops++]))
			goto done;
	}
	if (nloops == 0)
		goto done;

	/* The outline is the largest loop; the others are holes.  Convert to
	   scene units and simplify.  */
	size_t outer = 0;
	double best = 0;
	for (size_t i = 0; i < nloops; ++i) {
		double a = fabs(loop_area(&loops[i]));
		if (a > best) {
			best = a;
			outer = i;
		}
	}
	for (size_t i = 0; i < nloops; ++i) {
		Poly *p = &loops[i];
		for (size_t k = 0; k < p->count; ++k) {
			p->xy[2 * k] = grid->x + p->xy[2 * k] * grid->cell;
			p->xy[2 * k + 1] = grid->y + p->xy[2 * k + 1] * grid->cell;
		}
		if (!simplify_loop(p, tolerance))
			goto done;
	}
	/* Drop specks: holes smaller than a few cells come from rasterizing
	   crossing lines, not from islands.  */
	double speck = 4 * grid->cell * grid->cell;
	for (size_t i = 0; i < nloops; ++i)
		if (i != outer && loops[i].count >= 3 &&
		    fabs(loop_area(&loops[i])) > speck &&
		    !keyhole(&loops[outer], &loops[i]))
			goto done;
	if (loops[outer].count < 3)
		goto done;
	*out = loops[outer].xy;
	*count = loops[outer].count;
	loops[outer].xy = NULL;
	status = EXCAL_FILL_OK;
done:
	for (size_t i = 0; i < nloops; ++i)
		free(loops[i].xy);
	free(loops);
	free(mask);
	free(edges);
	free(stack);
	return status;
}
