/* excal-render.h --- Excalidraw scene rasterizer for Emacs Canvas  -*- c-file-style: "linux" -*- */

#ifndef EXCAL_RENDER_H
#define EXCAL_RENDER_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

typedef enum {
	EXCAL_RECTANGLE,
	EXCAL_ELLIPSE,
	EXCAL_DIAMOND,
	EXCAL_LINE,
	EXCAL_ARROW,
	EXCAL_FREEDRAW,
	EXCAL_TEXT,
	/* Editor overlays, drawn only in the selection pass.  */
	EXCAL_SELECTION, /* Box with resize handles around a multi-selection.  */
	EXCAL_MARQUEE,   /* Box-selection rectangle.  */
	EXCAL_UNKNOWN,
} ExcalType;

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
	char *start_arrowhead; /* NULL for none.  */
	char *end_arrowhead;
	double line_height;
	double opacity; /* 0..100 */
	/* 0: not selected, 1: selection box, 2: box with resize handles.  */
	int selection;
	bool rounded;
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
} ExcalView;

/* Render ELEMENTS into the ARGB32 PIXELS buffer.  Return the number of
   elements actually drawn after culling.  */
size_t excal_render(uint32_t *pixels, const ExcalView *view,
                    const ExcalElement *elements, size_t count);

/* Measure TEXT in scene units.  */
void excal_measure_text(const char *text, double font_size, int font_family,
                        double line_height, double *width, double *height);

/* Shift the WIDTH by HEIGHT PIXELS by DX, DY device pixels.  Pixels
   shifted in from outside keep stale contents and must be repainted.  */
void excal_scroll(uint32_t *pixels, int width, int height, int dx, int dy);

bool excal_write_png(uint32_t *pixels, int width, int height,
                     const char *path);

#endif /* EXCAL_RENDER_H */
