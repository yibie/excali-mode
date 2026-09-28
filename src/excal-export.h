/* excal-export.h --- PNG and SVG export, embedded scenes  -*- c-file-style: "linux" -*- */

#ifndef EXCAL_EXPORT_H
#define EXCAL_EXPORT_H

#include <stdbool.h>
#include <stddef.h>

#include "excal-render.h"

typedef struct {
	double x, y;          /* Scene point at the output's top-left.  */
	double width, height; /* Output size in scene units.  */
	double scale;         /* Output pixels per scene unit (PNG).  */
	const char *background; /* "#rrggbb[aa]" or NULL: transparent.  */
	bool clip, outline;   /* frameRendering.clip and .outline.  */
} ExcalExport;

/* Render ELEMENTS as upstream `exportToCanvas' does and write a PNG to
   PATH.  When TEXT is non-NULL, add a tEXt chunk with KEYWORD and the
   TEXT_LEN Latin-1 bytes TEXT just before IEND, like upstream
   `encodePngMetadata'.  */
bool excal_export_png(const ExcalElement *elements, size_t count,
                      const ExcalExport *opts, const char *keyword,
                      const unsigned char *text, size_t text_len,
                      const char *path);

/* Render ELEMENTS into a Cairo SVG document of WIDTH by HEIGHT user
   units; return it malloc'ed and NUL-terminated, set *LEN.  NULL when
   Cairo lacks the SVG surface.  */
char *excal_export_svg(const ExcalElement *elements, size_t count,
                       const ExcalExport *opts, size_t *len);

/* zlib (RFC 1950) compression and decompression; malloc'ed results.  */
unsigned char *excal_zlib_compress(const unsigned char *data, size_t len,
                                   size_t *out_len);
unsigned char *excal_zlib_decompress(const unsigned char *data, size_t len,
                                     size_t *out_len);

#endif /* EXCAL_EXPORT_H */
