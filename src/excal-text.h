/* excal-text.h --- Text layout, measurement and drawing  -*- c-file-style: "linux" -*- */

#ifndef EXCAL_TEXT_H
#define EXCAL_TEXT_H

#include <cairo.h>

#include "excal-render.h"

/* Draw text element E in the given color, in scene coordinates.  */
void excal_draw_text(cairo_t *cr, const ExcalElement *e, double red,
                     double green, double blue, double alpha);

#endif /* EXCAL_TEXT_H */
