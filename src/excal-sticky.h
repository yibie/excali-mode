/* excal-sticky.h --- Sticky note rendering  -*- c-file-style: "linux" -*- */
/* Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef EXCAL_STICKY_H
#define EXCAL_STICKY_H

#include <cairo.h>
#include <stdbool.h>
#include <stdint.h>

#include "excal-render.h"

/* Upstream seededRandom (mulberry32) state.  */
typedef struct {
	uint32_t value;
} ExcalMulberry;

void excal_mulberry_init(ExcalMulberry *m, double seed);
double excal_mulberry_next(ExcalMulberry *m);

/* Draw sticky note E in its own coordinates (the caller translates to
   e->x, e->y and applies rotation and opacity).  HAS_FILL/FILL is the
   paper color, STROKE the footer color.  */
void excal_draw_sticky(cairo_t *cr, const ExcalElement *e, bool has_fill,
                       const double fill[4], const double stroke[4]);

#endif /* EXCAL_STICKY_H */
