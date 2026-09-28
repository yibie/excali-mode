/* excal-render.h --- Excalidraw scene rasterizer for Emacs Canvas  -*- c-file-style: "linux" -*- */
/* Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef EXCAL_RENDER_H
#define EXCAL_RENDER_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

/* strdup is POSIX, not C11: glibc hides it under -std=c11.  */
static inline char *excal_strdup(const char *s)
{
	size_t n = strlen(s) + 1;
	char *copy = malloc(n);
	return copy ? memcpy(copy, s, n) : NULL;
}

typedef enum {
	EXCAL_RECTANGLE,
	EXCAL_ELLIPSE,
	EXCAL_DIAMOND,
	EXCAL_LINE,
	EXCAL_ARROW,
	EXCAL_FREEDRAW,
	EXCAL_TEXT,
	EXCAL_STICKYNOTE,
	EXCAL_IMAGE,      /* See excal-image.c.  */
	EXCAL_FRAME,      /* Frame and magicframe; see excal-frame.c.  */
	/* Editor overlays (selection UI), drawn above all elements; see
	   excal-overlay.c.  */
	EXCAL_OV_RECT,   /* Rotated rectangle outline, optionally filled.  */
	EXCAL_OV_HANDLE, /* Transform handle: rounded square.  */
	EXCAL_OV_CIRCLE, /* Rotation handle or linear point.  */
	EXCAL_OV_ELLIPSE, /* Rotated ellipse outline (binding highlight).  */
	EXCAL_OV_DIAMOND, /* Rotated diamond outline (binding highlight).  */
	EXCAL_OV_POLY,    /* Polyline through the element's points (snap lines).  */
	EXCAL_OV_GRID,    /* Background grid over the element's box, below all.  */
	EXCAL_UNKNOWN,
} ExcalType;

/* Image and frame fields from `excal--native-media-extras'.  */
typedef struct {
	char *id;       /* Frame-like elements: their own id.  */
	char *frame_id; /* Containing frame's id, or NULL.  */
	bool grouped;   /* The element belongs to a group.  */
	bool magic;     /* A magicframe.  */
	char *name;     /* Frame title (name or default).  */
	char *file_id;  /* Image file id, or NULL.  */
	bool error;     /* Image status "error".  */
	double scale[2]; /* Image flip factors, [1 1] by default.  */
	bool has_crop;
	double crop[6]; /* x y width height naturalWidth naturalHeight.  */
	double radius;  /* Image corner radius, 0 when sharp.  */
} ExcalMedia;

typedef struct {
	ExcalType type;
	double x, y, width, height, angle;
	char *stroke_color;
	char *background_color;
	char *fill_style;
	char *stroke_style;
	double stroke_width;
	double roughness;
	int32_t seed;
	double *points; /* Flat x0 y0 x1 y1 ..., relative to x, y.  */
	size_t point_count;
	char *text;
	double font_size;
	int font_family;
	char *text_align;
	char *sticky_footer;   /* Sticky note date label, or NULL.  */
	char *start_arrowhead; /* NULL for none.  */
	char *end_arrowhead;
	double line_height;
	double opacity; /* 0..100 */
	bool rounded;
	/* Text layout from `excal--native-text-extras'.  */
	bool has_text_offset;
	double text_offset; /* Baseline of the first line below y.  */
	bool has_label_hole; /* Arrow with a bound label.  */
	double label_hole[4]; /* Hole x, y, width, height in scene units.  */
	/* Shape extras, see `excal--native-shape-extras'.  */
	int roundness_type;      /* 1 legacy, 2 proportional, 3 adaptive; 0 unknown.  */
	double roundness_value;  /* NAN when absent.  */
	bool elbowed;
	double *pressures;       /* Freedraw pressures, or NULL.  */
	size_t pressure_count;
	int simulate_pressure;   /* 1 true, 0 false, -1 absent.  */
	bool constant_width;     /* strokeOptions.variability == "constant".  */
	double streamline;       /* strokeOptions.streamline, default 0.5.  */
	ExcalMedia media; /* Image and frame data, see excal-image.h.  */
} ExcalElement;

#define EXCAL_MAX_CLIPS 8

typedef struct {
	int x, y, width, height;
} ExcalRect;

typedef struct {
	int width, height;   /* Canvas size in device pixels.  */
	double pixel_scale;  /* Device pixels per logical pixel.  */
	double zoom;
	double scroll_x, scroll_y;
	/* Optional damage rectangles in device pixels; when CLIP_COUNT is
	   positive only pixels inside them are repainted and elements outside
	   all of them are skipped.  */
	int clip_count;
	ExcalRect clips[EXCAL_MAX_CLIPS];
	/* Canvas background colour for outline arrowheads ("#rrggbb"), or
	   NULL for white.  */
	const char *background_color;
	bool dark; /* Draw colors through the dark theme filter.  */
} ExcalView;

/* Upstream applyDarkModeFilter: CSS invert(93%) then hue-rotate(180deg),
   applied in place to RGB components in [0, 1].  */
void excal_dark_filter(double rgb[3]);

/* Render ELEMENTS into the ARGB32 PIXELS buffer.  Return the number of
   elements actually drawn after culling.  */
size_t excal_render(uint32_t *pixels, const ExcalView *view,
                    const ExcalElement *elements, size_t count);

/* Draw scene element E with CR in scene coordinates (the element pass of
   `excal_render', without culling or frame clipping).  */
struct _cairo;
void excal_draw_element(struct _cairo *cr, const ExcalElement *e);

/* Set the theme for drawing outside excal_render (exports).  */
void excal_render_prepare(bool dark);

/* Return true while drawing in the dark theme.  */
bool excal_render_dark(void);

/* Measure TEXT in scene units like Excalidraw's measureText: the
   width of the widest line, and lines * FONT_SIZE * LINE_HEIGHT.  */
void excal_measure_text(const char *text, double font_size, int font_family,
                        double line_height, double *width, double *height);

/* Shift the WIDTH by HEIGHT PIXELS by DX, DY device pixels.  Pixels
   shifted in from outside keep stale contents and must be repainted.  */
void excal_scroll(uint32_t *pixels, int width, int height, int dx, int dy);

bool excal_write_png(uint32_t *pixels, int width, int height,
                     const char *path);

#endif /* EXCAL_RENDER_H */
