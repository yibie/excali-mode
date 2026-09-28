/* excali-frame.h --- Frame outlines, names and clipping  -*- c-file-style: "linux" -*- */
/* Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef EXCALI_FRAME_H
#define EXCALI_FRAME_H

#include <cairo.h>
#include <stdbool.h>
#include <stddef.h>

#include "excali-render.h"

/* Upstream `appState.frameRendering' plus the zoom the pass uses.  */
typedef struct {
	double zoom;
	bool outline; /* Draw frame borders.  */
	bool clip;    /* Clip children to their frame.  */
	bool names;   /* Draw frame names (the live canvas; exports add
	                 them as text elements instead).  */
} ExcaliFrameConfig;

/* Start a render pass over ELEMENTS: remember the frames among them and
   CONFIG.  Must be paired with `excali_frame_end_pass'.  */
void excali_frame_begin_pass(const ExcaliElement *elements, size_t count,
                            const ExcaliFrameConfig *config);
void excali_frame_end_pass(void);

/* Save CR and, if E belongs to a frame, clip to that frame as upstream
   `frameClip' does.  Return the element to draw: E, or SCRATCH holding a
   copy of E whose opacity includes its frame's.  Pair with
   `excali_frame_clip_end'.  */
const ExcaliElement *excali_frame_clip_begin(cairo_t *cr, const ExcaliElement *e,
                                           ExcaliElement *scratch);
void excali_frame_clip_end(cairo_t *cr);

/* Draw frame (or magicframe) E: its outline and, on the live canvas, its
   name above it.  */
void excali_draw_frame(cairo_t *cr, const ExcaliElement *e);

/* The name as the live canvas shows it: TITLE cut with an ellipsis to fit
   MAX_WIDTH screen pixels at 14px Assistant.  Return a malloc'ed string
   and set *WIDTH to its width in screen pixels.  */
char *excali_frame_label_text(const char *title, double max_width,
                             double *width);

#endif /* EXCALI_FRAME_H */
