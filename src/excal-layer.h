/* excal-layer.h --- CoreAnimation overlay for excal.el (macOS)  -*- c-file-style: "linux" -*- */

#ifndef EXCAL_LAYER_H
#define EXCAL_LAYER_H

#include <stdbool.h>
#include <stdint.h>

/* The Emacs view (an unretained NSView *) whose screen rectangle (top-left
   origin, points) best matches LEFT TOP WIDTH HEIGHT, or NULL.  */
void *excal_find_emacs_view(double left, double top, double width,
                            double height);

/* Attach an overlay to the Emacs view whose screen rectangle (top-left
   origin, points) best matches LEFT TOP WIDTH HEIGHT.  */
void *excal_layer_create(double left, double top, double width,
                         double height);

/* Place LAYER at X Y WIDTH HEIGHT in view points, top-left origin.  */
void excal_layer_set_geometry(void *layer, double x, double y, double width,
                              double height, double scale, bool visible);

/* Show WIDTH by HEIGHT premultiplied ARGB32 PIXELS in LAYER.  */
bool excal_layer_present(void *layer, const uint32_t *pixels, int width,
                         int height);

void excal_layer_flush(void);

void excal_layer_destroy(void *layer);

#endif /* EXCAL_LAYER_H */
