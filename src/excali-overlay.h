/* excali-overlay.h --- Editor overlays: selection UI  -*- c-file-style: "linux" -*- */
/* Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef EXCALI_OVERLAY_H
#define EXCALI_OVERLAY_H

#include <cairo.h>
#include <stdbool.h>

#include "excali-render.h"

/* Return true if TYPE is an editor overlay rather than a scene element.  */
bool excali_overlay_p(ExcaliType type);

/* Draw overlay E.  Geometry is in scene units; `stroke_width' and dash
   lengths are in screen pixels, so overlays look the same at any ZOOM.  */
void excali_draw_overlay(cairo_t *cr, const ExcaliElement *e, double zoom);

/* Draw grid overlay E, which covers the visible scene; it goes below the
   elements.  PIXEL_SCALE is device pixels per screen pixel.  */
void excali_draw_grid(cairo_t *cr, const ExcaliElement *e, double zoom,
                     double pixel_scale, bool dark);

#endif /* EXCALI_OVERLAY_H */
