/* excal-fill.h --- Bucket-fill regions  -*- c-file-style: "linux" -*- */

#ifndef EXCAL_FILL_H
#define EXCAL_FILL_H

#include <stdbool.h>
#include <stddef.h>

/* A wall: COUNT points (x, y pairs in scene units), joined into a loop
   when CLOSED.  */
typedef struct {
	const double *points;
	size_t count;
	bool closed;
} ExcalFillWall;

/* The search grid: GW by GH cells of CELL scene units from (X, Y).  */
typedef struct {
	double x, y, cell;
	int gw, gh;
} ExcalFillGrid;

typedef enum {
	EXCAL_FILL_OK,
	EXCAL_FILL_ON_WALL,   /* The point is on (or next to) a wall.  */
	EXCAL_FILL_UNBOUNDED, /* The region reaches the edge of the grid.  */
	EXCAL_FILL_FAILED,    /* Out of memory or a degenerate region.  */
} ExcalFillStatus;

/* Find the smallest region around scene point (PX, PY) closed off by
   WALLS, gaps narrower than GAP scene units bridged.  Holes (islands)
   are joined to the outline by keyhole bridges, so the result is one
   polygon: *OUT gets 2 * *COUNT malloc'ed coordinates, simplified to
   within TOLERANCE scene units.  */
ExcalFillStatus excal_fill_region(const ExcalFillWall *walls, size_t nwalls,
                                  const ExcalFillGrid *grid, double px,
                                  double py, double gap, double tolerance,
                                  double **out, size_t *count);

#endif /* EXCAL_FILL_H */
