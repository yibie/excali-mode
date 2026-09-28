/* excali-freehand.h --- Freedraw stroke outlines  -*- c-file-style: "linux" -*-
 * Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Ports of perfect-freehand 1.2.0 `getStroke' (variable width) and of
 * the `LaserPointer' outline from @excalidraw/laser-pointer (constant
 * width), with the options Excalidraw's shape.ts passes.
 */

#ifndef EXCALI_FREEHAND_H
#define EXCALI_FREEHAND_H

#include <stddef.h>

#include "excali-rough.h"

/* Variable-width outline (`getVariableWidthFreedrawOutline').  XY holds
   N points; PRESSURES holds PRESSURE_COUNT values (missing ones read as
   undefined).  SIMULATE is the element's simulatePressure: 1 true,
   0 false, -1 absent.  */
void excali_freehand_variable(const double *xy, size_t n,
                             const double *pressures, size_t pressure_count,
                             int simulate, double stroke_width,
                             double streamline, RoughPoints *out);

/* Constant-width outline (`getConstantWidthFreedrawOutline').  */
void excali_freehand_constant(const double *xy, size_t n, double stroke_width,
                             double streamline, RoughPoints *out);

/* Truncate X to two decimals the way Excalidraw's `getSvgPathFromStroke'
   does to the number's JS string form.  */
double excali_freehand_truncate(double x);

#endif /* EXCALI_FREEHAND_H */
