/* excal-preview.h --- Approximate zoom previews from rendered pixels  -*- c-file-style: "linux" -*- */

#ifndef EXCAL_PREVIEW_H
#define EXCAL_PREVIEW_H

#include <stdbool.h>
#include <stdint.h>

/* Fill the WIDTH by HEIGHT ARGB32 buffer DST with SRC (same size) scaled
   by SCALE and then translated by TX, TY device pixels, with bilinear
   filtering.  Pixels SRC does not cover become white.  DST may equal
   SRC.  Return false on allocation failure.  */
bool excal_zoom_preview(uint32_t *dst, const uint32_t *src, int width,
                        int height, double scale, double tx, double ty);

/* Return the mean absolute difference of the colour channels of the
   WIDTH by HEIGHT buffers A and B, in levels 0..255.  */
double excal_mean_diff(const uint32_t *a, const uint32_t *b, int width,
                       int height);

#endif /* EXCAL_PREVIEW_H */
