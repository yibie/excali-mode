/* excal-text.h --- Text layout, measurement and drawing  -*- c-file-style: "linux" -*- */
/* Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef EXCAL_TEXT_H
#define EXCAL_TEXT_H

#include <cairo.h>
#include <stdbool.h>

#include "excal-render.h"

/* Font family ids (FONT_FAMILY and FONT_FAMILY_FALLBACKS upstream).  */
enum {
	EXCAL_FONT_VIRGIL = 1,
	EXCAL_FONT_HELVETICA = 2,
	EXCAL_FONT_CASCADIA = 3,
	EXCAL_FONT_EXCALIFONT = 5,
	EXCAL_FONT_NUNITO = 6,
	EXCAL_FONT_LILITA_ONE = 7,
	EXCAL_FONT_COMIC_SHANNS = 8,
	EXCAL_FONT_LIBERATION_SANS = 9,
	EXCAL_FONT_ASSISTANT = 10,
	EXCAL_FONT_XIAOLAI = 100,
	EXCAL_FONT_SANS_SERIF = 998,
	EXCAL_FONT_MONOSPACE = 999,
	EXCAL_FONT_SEGOE_UI_EMOJI = 1000,
};

/* Register the font file PATH, or every font file in directory PATH,
   with the font backend Pango uses.  Return the number of files
   registered, or -1 if PATH cannot be read.  */
int excal_text_add_fonts(const char *path);

/* Set the comma-separated Pango family list used for font id ID.
   FAMILIES NULL restores the built-in list.  */
void excal_text_set_family(int id, const char *families);

/* Return the Pango family list used for font id ID.  */
const char *excal_text_family(int id);

/* Advance width of the single line LINE in scene units; newlines are
   not interpreted.  */
double excal_text_line_width(const char *line, double font_size,
                             int font_family);

/* Return a malloc'ed, comma-separated list of the font families Pango
   picks to show TEXT in font id FAMILY, in order of first use.  */
char *excal_text_resolve(const char *text, int font_family);

/* Name of the Pango font map type, e.g. "PangoCairoFcFontMap".  */
const char *excal_text_backend(void);

/* Draw text element E line by line in the given color, in scene
   coordinates, placing baselines as Excalidraw does.  */
void excal_draw_text(cairo_t *cr, const ExcalElement *e, double red,
                     double green, double blue, double alpha);

/* If E (an arrow) has a bound label, clip out the label's hole with an
   even-odd clip so the stroke leaves a gap behind the label.  Call
   before stroking the arrow, inside a cairo_save/cairo_restore pair.  */
void excal_text_clip_label_hole(cairo_t *cr, const ExcalElement *e);

#endif /* EXCAL_TEXT_H */
