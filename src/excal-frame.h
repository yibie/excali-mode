/* excal-frame.h --- Frame outlines, names and clipping  -*- c-file-style: "linux" -*- */

#ifndef EXCAL_FRAME_H
#define EXCAL_FRAME_H

#include <cairo.h>
#include <stdbool.h>
#include <stddef.h>

#include "excal-render.h"

/* Upstream `appState.frameRendering' plus the zoom the pass uses.  */
typedef struct {
	double zoom;
	bool outline; /* Draw frame borders.  */
	bool clip;    /* Clip children to their frame.  */
	bool names;   /* Draw frame names (the live canvas; exports add
	                 them as text elements instead).  */
} ExcalFrameConfig;

/* Start a render pass over ELEMENTS: remember the frames among them and
   CONFIG.  Must be paired with `excal_frame_end_pass'.  */
void excal_frame_begin_pass(const ExcalElement *elements, size_t count,
                            const ExcalFrameConfig *config);
void excal_frame_end_pass(void);

/* Save CR and, if E belongs to a frame, clip to that frame as upstream
   `frameClip' does.  Return the element to draw: E, or SCRATCH holding a
   copy of E whose opacity includes its frame's.  Pair with
   `excal_frame_clip_end'.  */
const ExcalElement *excal_frame_clip_begin(cairo_t *cr, const ExcalElement *e,
                                           ExcalElement *scratch);
void excal_frame_clip_end(cairo_t *cr);

/* Draw frame (or magicframe) E: its outline and, on the live canvas, its
   name above it.  */
void excal_draw_frame(cairo_t *cr, const ExcalElement *e);

/* The name as the live canvas shows it: TITLE cut with an ellipsis to fit
   MAX_WIDTH screen pixels at 14px Assistant.  Return a malloc'ed string
   and set *WIDTH to its width in screen pixels.  */
char *excal_frame_label_text(const char *title, double max_width,
                             double *width);

#endif /* EXCAL_FRAME_H */
