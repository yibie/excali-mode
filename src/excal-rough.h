/* excal-rough.h --- Port of roughjs 4.6.4 shape generation  -*- c-file-style: "linux" -*-
 *
 * A faithful port of the parts of roughjs (renderer.ts, generator.ts,
 * fillers/, and its helpers hachure-fill 0.5.2, points-on-curve 0.2.0,
 * points-on-path 0.2.1, path-data-parser 0.1.0) that Excalidraw uses.
 * Random numbers are consumed in the same order as roughjs, so a shape
 * generated here from an element's seed has the same geometry as on
 * excalidraw.com, up to libm rounding differences.
 */

#ifndef EXCAL_ROUGH_H
#define EXCAL_ROUGH_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

/* roughjs `Random': Park-Miller style LCG on a 32-bit state.  */
typedef struct {
	uint32_t seed;
} RoughRandom;

void rough_random_init(RoughRandom *r, double seed);
double rough_random_next(RoughRandom *r);

/* Convert a JS number to its int32 bit pattern (ToInt32 / ToUint32).  */
uint32_t rough_to_uint32(double x);

typedef enum {
	ROUGH_FILL_HACHURE,
	ROUGH_FILL_SOLID,
	ROUGH_FILL_ZIGZAG,
	ROUGH_FILL_CROSS_HATCH,
} RoughFillStyle;

RoughFillStyle rough_fill_style(const char *name);

/* roughjs `ResolvedOptions', restricted to what Excalidraw uses.  */
typedef struct RoughOptions {
	double max_randomness_offset;
	double roughness;
	double bowing;
	double stroke_width;
	double curve_tightness;
	double curve_fitting;
	double curve_step_count;
	RoughFillStyle fill_style;
	double fill_weight;
	double hachure_angle;
	double hachure_gap;
	double fill_shape_roughness_gain;
	double seed; /* A JS number; 0 means unseeded.  */
	bool disable_multi_stroke;
	bool disable_multi_stroke_fill;
	bool preserve_vertices;
	bool fill;   /* `o.fill' is set.  */
	bool stroke; /* `o.stroke !== "none"'.  */
	/* The lazily created `o.randomizer'.  Copying the struct shares the
	   randomizer, as copying a JS options object does.  */
	RoughRandom *randomizer;
	RoughRandom randomizer_storage;
} RoughOptions;

/* roughjs `defaultOptions'.  */
RoughOptions rough_default_options(void);

typedef enum { ROUGH_MOVE, ROUGH_LINE_TO, ROUGH_BCURVE_TO } RoughOpKind;

typedef struct {
	RoughOpKind op;
	double data[6];
} RoughOp;

typedef struct {
	RoughOp *ops;
	size_t count, capacity;
} RoughOps;

typedef enum {
	ROUGH_SET_PATH,
	ROUGH_SET_FILL_PATH,
	ROUGH_SET_FILL_SKETCH,
} RoughSetType;

typedef struct {
	RoughSetType type;
	RoughOps ops;
} RoughSet;

typedef enum {
	ROUGH_SHAPE_LINE,
	ROUGH_SHAPE_RECTANGLE,
	ROUGH_SHAPE_ELLIPSE,
	ROUGH_SHAPE_CIRCLE,
	ROUGH_SHAPE_LINEAR_PATH,
	ROUGH_SHAPE_CURVE,
	ROUGH_SHAPE_POLYGON,
	ROUGH_SHAPE_PATH,
} RoughShape;

#define ROUGH_MAX_SETS 3

/* roughjs `Drawable'.  */
typedef struct {
	RoughShape shape;
	RoughSet sets[ROUGH_MAX_SETS];
	int set_count;
	RoughOptions options;
} RoughDrawable;

void rough_drawable_free(RoughDrawable *d);

/* Canvas fill rule roughjs uses for a fillPath set of D.  */
bool rough_fill_evenodd(const RoughDrawable *d);

/* The ops of the first "path" set, else of the first set, or NULL:
   Excalidraw's `getCurvePathOps'.  */
const RoughOps *rough_curve_path_ops(const RoughDrawable *d);

/* Flat point list x0 y0 x1 y1 ...  */
typedef struct {
	double *xy;
	size_t count, capacity;
} RoughPoints;

void rough_points_push(RoughPoints *p, double x, double y);
void rough_points_free(RoughPoints *p);

/* A normalized SVG path segment: M, L and C with absolute coordinates,
   and Z.  */
typedef struct {
	char key;
	double data[6];
} RoughSegment;

typedef struct {
	RoughSegment *segments;
	size_t count, capacity;
	double cx, cy, subx, suby; /* Current point and subpath start.  */
} RoughPath;

void rough_path_move(RoughPath *p, double x, double y);
void rough_path_line(RoughPath *p, double x, double y);
void rough_path_cubic(RoughPath *p, double x1, double y1, double x2,
                      double y2, double x, double y);
/* Quadratic, converted to cubic as path-data-parser `normalize' does.  */
void rough_path_quad(RoughPath *p, double x1, double y1, double x, double y);
void rough_path_close(RoughPath *p);
void rough_path_free(RoughPath *p);

/* Generator (generator.ts).  Each call copies OPTIONS like `_o' does,
   starting from a fresh randomizer.  */
void rough_gen_line(RoughDrawable *d, double x1, double y1, double x2,
                    double y2, const RoughOptions *options);
void rough_gen_rectangle(RoughDrawable *d, double x, double y, double width,
                         double height, const RoughOptions *options);
void rough_gen_ellipse(RoughDrawable *d, double x, double y, double width,
                       double height, const RoughOptions *options);
void rough_gen_circle(RoughDrawable *d, double x, double y, double diameter,
                      const RoughOptions *options);
void rough_gen_linear_path(RoughDrawable *d, const double *xy, size_t n,
                           const RoughOptions *options);
void rough_gen_polygon(RoughDrawable *d, const double *xy, size_t n,
                       const RoughOptions *options);
void rough_gen_curve(RoughDrawable *d, const double *xy, size_t n,
                     const RoughOptions *options);
void rough_gen_path(RoughDrawable *d, const RoughPath *path,
                    const RoughOptions *options);

/* points-on-curve helpers, exported for freedraw and tests.  */
void rough_simplify(const double *xy, size_t n, double distance,
                    RoughPoints *out);

/* hachure-fill `hachureLines' on one or more polygons, exported for
   tests.  LINES receives x1 y1 x2 y2 per line.  */
void rough_hachure_lines(const RoughPoints *polygons, size_t count,
                         double gap, double angle, double step_offset,
                         RoughPoints *lines);

#endif /* EXCAL_ROUGH_H */
