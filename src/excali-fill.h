/* excali-fill.h --- Bucket-fill regions  -*- c-file-style: "linux" -*- */
/* Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef EXCALI_FILL_H
#define EXCALI_FILL_H

#include <stdbool.h>
#include <stddef.h>

/* A wall: COUNT points (x, y pairs in scene units), joined into a loop
   when CLOSED.  */
typedef struct {
	const double *points;
	size_t count;
	bool closed;
} ExcaliFillWall;

/* The search grid: GW by GH cells of CELL scene units from (X, Y).  */
typedef struct {
	double x, y, cell;
	int gw, gh;
} ExcaliFillGrid;

typedef enum {
	EXCALI_FILL_OK,
	EXCALI_FILL_ON_WALL,   /* The point is on (or next to) a wall.  */
	EXCALI_FILL_UNBOUNDED, /* The region reaches the edge of the grid.  */
	EXCALI_FILL_FAILED,    /* Out of memory or a degenerate region.  */
} ExcaliFillStatus;

/* Find the smallest region around scene point (PX, PY) closed off by
   WALLS, gaps narrower than GAP scene units bridged.  Holes (islands)
   are joined to the outline by keyhole bridges, so the result is one
   polygon: *OUT gets 2 * *COUNT malloc'ed coordinates, simplified to
   within TOLERANCE scene units.  */
ExcaliFillStatus excali_fill_region(const ExcaliFillWall *walls, size_t nwalls,
                                  const ExcaliFillGrid *grid, double px,
                                  double py, double gap, double tolerance,
                                  double **out, size_t *count);

#endif /* EXCALI_FILL_H */
