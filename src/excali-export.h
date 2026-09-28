/* excali-export.h --- PNG and SVG export, embedded scenes  -*- c-file-style: "linux" -*- */
/* Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef EXCALI_EXPORT_H
#define EXCALI_EXPORT_H

#include <stdbool.h>
#include <stddef.h>

#include "excali-render.h"

typedef struct {
	double x, y;          /* Scene point at the output's top-left.  */
	double width, height; /* Output size in scene units.  */
	double scale;         /* Output pixels per scene unit (PNG).  */
	const char *background; /* "#rrggbb[aa]" or NULL: transparent.  */
	bool clip, outline;   /* frameRendering.clip and .outline.  */
} ExcaliExport;

/* Render ELEMENTS as upstream `exportToCanvas' does and write a PNG to
   PATH.  When TEXT is non-NULL, add a tEXt chunk with KEYWORD and the
   TEXT_LEN Latin-1 bytes TEXT just before IEND, like upstream
   `encodePngMetadata'.  */
bool excali_export_png(const ExcaliElement *elements, size_t count,
                      const ExcaliExport *opts, const char *keyword,
                      const unsigned char *text, size_t text_len,
                      const char *path);

/* Render ELEMENTS into a Cairo SVG document of WIDTH by HEIGHT user
   units; return it malloc'ed and NUL-terminated, set *LEN.  NULL when
   Cairo lacks the SVG surface.  */
char *excali_export_svg(const ExcaliElement *elements, size_t count,
                       const ExcaliExport *opts, size_t *len);

/* zlib (RFC 1950) compression and decompression; malloc'ed results.  */
unsigned char *excali_zlib_compress(const unsigned char *data, size_t len,
                                   size_t *out_len);
unsigned char *excali_zlib_decompress(const unsigned char *data, size_t len,
                                     size_t *out_len);

#endif /* EXCALI_EXPORT_H */
