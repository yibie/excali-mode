/* excal-shape.h --- Excalidraw element shapes on top of roughjs  -*- c-file-style: "linux" -*-
 *
 * Port of packages/element/src/shape.ts (`generateRoughOptions',
 * `_generateElementShape', arrowheads, freedraw outline) and the bits of
 * bounds.ts / utils.ts it needs.  All geometry is in element-local
 * coordinates: the element's (x, y) is the origin, as in Excalidraw.
 */

#ifndef EXCAL_SHAPE_H
#define EXCAL_SHAPE_H

#include <stdbool.h>

#include "excal-render.h"
#include "excal-rough.h"

typedef enum {
	EXCAL_FILL_ELEMENT, /* The element's backgroundColor.  */
	EXCAL_FILL_STROKE,  /* The element's strokeColor (solid heads).  */
	EXCAL_FILL_CANVAS,  /* The canvas background (outline heads).  */
} ExcalFillSource;

typedef struct {
	RoughDrawable rough;
	ExcalFillSource fill_source;
	int dash_count; /* strokeLineDash for "path" sets; 0 for solid.  */
	double dash[2];
} ExcalDrawable;

#define EXCAL_SHAPE_MAX_DRAWABLES 8

typedef struct {
	ExcalDrawable items[EXCAL_SHAPE_MAX_DRAWABLES];
	int count;
	/* Freedraw: the stroke outline polygon (already truncated to two
	   decimals like `getSvgPathFromStroke'), filled with strokeColor.  */
	RoughPoints outline;
	/* getElementAbsoluteCoords, relative to the element's x, y.  */
	double x1, y1, x2, y2;
	bool butt_caps; /* Freedraw keeps the canvas' default caps.  */
} ExcalShape;

void excal_shape_generate(const ExcalElement *e, ExcalShape *shape);
void excal_shape_free(ExcalShape *shape);

/* `getCornerRadius' for roundness TYPE and VALUE (NAN when absent).  */
double excal_corner_radius(double x, int type, double value);

/* `getArrowheadPoints' for arrowhead KIND at the START or end of E,
   given the element's curve SHAPE0.  Writes up to 8 numbers to OUT and
   returns how many (0 when there is no head).  */
int excal_arrowhead_points(const ExcalElement *e, const RoughDrawable *shape0,
                           bool start, const char *kind,
                           double offset_multiplier, double *out);

/* `getArrowheadSize' and `getArrowheadAngle' (degrees).  */
double excal_arrowhead_size(const char *kind);
double excal_arrowhead_angle(const char *kind);

/* Conservative padding around the element's box or points that its
   drawn geometry (jitter, curve overshoot, arrowheads, stroke) stays
   within, in scene units.  */
double excal_shape_padding(const ExcalElement *e);

#endif /* EXCAL_SHAPE_H */
