/* excali-shape.h --- Excalidraw element shapes on top of roughjs  -*- c-file-style: "linux" -*-
 * Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Port of packages/element/src/shape.ts (`generateRoughOptions',
 * `_generateElementShape', arrowheads, freedraw outline) and the bits of
 * bounds.ts / utils.ts it needs.  All geometry is in element-local
 * coordinates: the element's (x, y) is the origin, as in Excalidraw.
 */

#ifndef EXCALI_SHAPE_H
#define EXCALI_SHAPE_H

#include <stdbool.h>

#include "excali-render.h"
#include "excali-rough.h"

typedef enum {
	EXCALI_FILL_ELEMENT, /* The element's backgroundColor.  */
	EXCALI_FILL_STROKE,  /* The element's strokeColor (solid heads).  */
	EXCALI_FILL_CANVAS,  /* The canvas background (outline heads).  */
} ExcaliFillSource;

typedef struct {
	RoughDrawable rough;
	ExcaliFillSource fill_source;
	int dash_count; /* strokeLineDash for "path" sets; 0 for solid.  */
	double dash[2];
} ExcaliDrawable;

#define EXCALI_SHAPE_MAX_DRAWABLES 8

typedef struct {
	ExcaliDrawable items[EXCALI_SHAPE_MAX_DRAWABLES];
	int count;
	/* Freedraw: the stroke outline polygon (already truncated to two
	   decimals like `getSvgPathFromStroke'), filled with strokeColor.  */
	RoughPoints outline;
	/* getElementAbsoluteCoords, relative to the element's x, y.  */
	double x1, y1, x2, y2;
	bool butt_caps; /* Freedraw keeps the canvas' default caps.  */
} ExcaliShape;

void excali_shape_generate(const ExcaliElement *e, ExcaliShape *shape);
void excali_shape_free(ExcaliShape *shape);

/* `getCornerRadius' for roundness TYPE and VALUE (NAN when absent).  */
double excali_corner_radius(double x, int type, double value);

/* `getArrowheadPoints' for arrowhead KIND at the START or end of E,
   given the element's curve SHAPE0.  Writes up to 8 numbers to OUT and
   returns how many (0 when there is no head).  */
int excali_arrowhead_points(const ExcaliElement *e, const RoughDrawable *shape0,
                           bool start, const char *kind,
                           double offset_multiplier, double *out);

/* `getArrowheadSize' and `getArrowheadAngle' (degrees).  */
double excali_arrowhead_size(const char *kind);
double excali_arrowhead_angle(const char *kind);

/* Conservative padding around the element's box or points that its
   drawn geometry (jitter, curve overshoot, arrowheads, stroke) stays
   within, in scene units.  */
double excali_shape_padding(const ExcaliElement *e);

#endif /* EXCALI_SHAPE_H */
