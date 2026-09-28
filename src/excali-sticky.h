/* excali-sticky.h --- Sticky note rendering  -*- c-file-style: "linux" -*- */
/* Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef EXCALI_STICKY_H
#define EXCALI_STICKY_H

#include <cairo.h>
#include <stdbool.h>
#include <stdint.h>

#include "excali-render.h"

/* Upstream seededRandom (mulberry32) state.  */
typedef struct {
	uint32_t value;
} ExcaliMulberry;

void excali_mulberry_init(ExcaliMulberry *m, double seed);
double excali_mulberry_next(ExcaliMulberry *m);

/* Draw sticky note E in its own coordinates (the caller translates to
   e->x, e->y and applies rotation and opacity).  HAS_FILL/FILL is the
   paper color, STROKE the footer color.  */
void excali_draw_sticky(cairo_t *cr, const ExcaliElement *e, bool has_fill,
                       const double fill[4], const double stroke[4]);

#endif /* EXCALI_STICKY_H */
