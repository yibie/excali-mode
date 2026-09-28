/* excali-image.h --- Image elements: decoding, cache and drawing  -*- c-file-style: "linux" -*- */
/* Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef EXCALI_IMAGE_H
#define EXCALI_IMAGE_H

#include <cairo.h>
#include <stdbool.h>
#include <stddef.h>

#include "excali-render.h"

/* Decode the data URL URL (LEN bytes) and cache the image under ID,
   replacing any image cached under ID.  Return false, and set *ERROR to
   a static message, when it cannot be decoded.  */
bool excali_image_register(const char *id, const char *url, size_t len,
                          const char **error);

/* Drop the image cached under ID; return false if there is none.  */
bool excali_image_forget(const char *id);

/* Natural size and MIME type of the image cached under ID.  */
bool excali_image_info(const char *id, double *width, double *height,
                      const char **mime);

/* Number of cached images.  */
size_t excali_image_count(void);

/* Return a malloc'ed PNG of the image cached under ID, scaled down to
   fit MAX_SIZE pixels in both dimensions; set *LEN.  NULL on failure or
   for vector images.  */
unsigned char *excali_image_png(const char *id, int max_size, size_t *len);

/* Decode the data URL URL of LEN bytes into malloc'ed bytes; set *LEN_OUT
   and *MIME (malloc'ed, may be empty).  Base64 and percent-encoded
   payloads are accepted.  */
unsigned char *excali_data_url_decode(const char *url, size_t len,
                                     char **mime, size_t *len_out);

/* Draw image element E like upstream `drawElementOnCanvas' (flip, crop,
   rounded corners), or its placeholder when no image is cached.  */
void excali_draw_image(cairo_t *cr, const ExcaliElement *e);

/* Free the strings in M.  */
void excali_media_free(ExcaliMedia *m);

/* Build the path of SVG path data D (commands MmLlHhVvCcSsQqZz).  */
void excali_svg_path(cairo_t *cr, const char *d);

#endif /* EXCALI_IMAGE_H */
