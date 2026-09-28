/* excal-overlay.h --- Editor overlays: selection UI  -*- c-file-style: "linux" -*- */

#ifndef EXCAL_OVERLAY_H
#define EXCAL_OVERLAY_H

#include <cairo.h>
#include <stdbool.h>

#include "excal-render.h"

/* Return true if TYPE is an editor overlay rather than a scene element.  */
bool excal_overlay_p(ExcalType type);

/* Draw overlay E.  Geometry is in scene units; `stroke_width' and dash
   lengths are in screen pixels, so overlays look the same at any ZOOM.  */
void excal_draw_overlay(cairo_t *cr, const ExcalElement *e, double zoom);

/* Draw grid overlay E, which covers the visible scene; it goes below the
   elements.  PIXEL_SCALE is device pixels per screen pixel.  */
void excal_draw_grid(cairo_t *cr, const ExcalElement *e, double zoom,
                     double pixel_scale);

#endif /* EXCAL_OVERLAY_H */
