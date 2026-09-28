/* excal-module.c --- Emacs module glue for excal.el  -*- c-file-style: "linux" -*- */

#include <emacs-module.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "excal-preview.h"
#include "excal-render.h"
#include "excal-rough.h"
#include "excal-shape.h"
#ifdef EXCAL_HAVE_LAYER
#include "excal-layer.h"
#endif

int plugin_is_GPL_compatible;

/* Element vector slots, kept in sync with `excal--native-element'.  */
enum {
	SLOT_TYPE,
	SLOT_X,
	SLOT_Y,
	SLOT_WIDTH,
	SLOT_HEIGHT,
	SLOT_ANGLE,
	SLOT_STROKE_COLOR,
	SLOT_BACKGROUND_COLOR,
	SLOT_FILL_STYLE,
	SLOT_STROKE_WIDTH,
	SLOT_ROUGHNESS,
	SLOT_SEED,
	SLOT_POINTS,
	SLOT_TEXT,
	SLOT_FONT_SIZE,
	SLOT_OPACITY,
	SLOT_SELECTED,
	SLOT_STROKE_STYLE,
	SLOT_FONT_FAMILY,
	SLOT_TEXT_ALIGN,
	SLOT_LINE_HEIGHT,
	SLOT_ROUNDED,
	SLOT_START_ARROWHEAD,
	SLOT_END_ARROWHEAD,
	SLOT_SHAPE_EXTRAS, /* [KEY VALUE ...], see `excal--native-shape-extras'.  */
	SLOT_TEXT_EXTRAS,  /* [KEY VALUE ...], see `excal--native-text-extras'.  */
	SLOT_COUNT,
};

static emacs_value Qnil, Qt, Qinteger, Qfloat, Qstring, Qvector, Quser_ptr;

static bool type_is(emacs_env *env, emacs_value value, emacs_value type)
{
	return env->eq(env, env->type_of(env, value), type);
}

static double get_number(emacs_env *env, emacs_value value, double fallback)
{
	if (type_is(env, value, Qinteger))
		return (double)env->extract_integer(env, value);
	if (type_is(env, value, Qfloat))
		return env->extract_float(env, value);
	return fallback;
}

static char *get_string(emacs_env *env, emacs_value value)
{
	if (!type_is(env, value, Qstring))
		return NULL;
	ptrdiff_t size = 0;
	env->copy_string_contents(env, value, NULL, &size);
	char *buffer = malloc(size);
	if (buffer && !env->copy_string_contents(env, value, buffer, &size)) {
		free(buffer);
		return NULL;
	}
	return buffer;
}

/* Return the value stored under KEY in EXTRAS, a vector [KEY VALUE ...]
   with string keys, or nil.  */
static emacs_value get_extra(emacs_env *env, emacs_value extras,
                             const char *key)
{
	if (!type_is(env, extras, Qvector))
		return Qnil;
	ptrdiff_t n = env->vec_size(env, extras);
	for (ptrdiff_t i = 0; i + 1 < n; i += 2) {
		char *name = get_string(env, env->vec_get(env, extras, i));
		bool match = name && strcmp(name, key) == 0;
		free(name);
		if (match)
			return env->vec_get(env, extras, i + 1);
	}
	return Qnil;
}

static double get_extra_number(emacs_env *env, emacs_value extras,
                                      const char *key, double fallback)
{
	return get_number(env, get_extra(env, extras, key), fallback);
}

static char *get_extra_string(emacs_env *env, emacs_value extras,
                                     const char *key)
{
	return get_string(env, get_extra(env, extras, key));
}

static ExcalType parse_type(const char *name)
{
	static const struct {
		const char *name;
		ExcalType type;
	} table[] = {
	        {"rectangle", EXCAL_RECTANGLE}, {"ellipse", EXCAL_ELLIPSE},
	        {"diamond", EXCAL_DIAMOND},     {"line", EXCAL_LINE},
	        {"arrow", EXCAL_ARROW},         {"freedraw", EXCAL_FREEDRAW},
	        {"text", EXCAL_TEXT},           {"selection", EXCAL_SELECTION},
	        {"marquee", EXCAL_MARQUEE},
	};
	if (name)
		for (size_t i = 0; i < sizeof table / sizeof table[0]; ++i)
			if (strcmp(name, table[i].name) == 0)
				return table[i].type;
	return EXCAL_UNKNOWN;
}

static void free_element(ExcalElement *e)
{
	free(e->stroke_color);
	free(e->background_color);
	free(e->fill_style);
	free(e->stroke_style);
	free(e->points);
	free(e->text);
	free(e->text_align);
	free(e->start_arrowhead);
	free(e->end_arrowhead);
	free(e->pressures);
}

/* Shape extras from `excal--native-shape-extras'.  */
static void read_shape_extras(emacs_env *env, emacs_value extras,
                              ExcalElement *e)
{
	e->roundness_type = (int)get_extra_number(env, extras, "roundnessType", 0);
	e->roundness_value =
	        get_extra_number(env, extras, "roundnessValue", NAN);
	e->elbowed = env->is_not_nil(env, get_extra(env, extras, "elbowed"));
	e->simulate_pressure =
	        (int)get_extra_number(env, extras, "simulatePressure", -1);
	char *variability = get_extra_string(env, extras, "strokeVariability");
	e->constant_width = variability && strcmp(variability, "constant") == 0;
	free(variability);
	e->streamline = get_extra_number(env, extras, "streamline", 0.5);
	emacs_value pressures = get_extra(env, extras, "pressures");
	if (type_is(env, pressures, Qvector)) {
		ptrdiff_t n = env->vec_size(env, pressures);
		e->pressures = malloc(sizeof(double) * (n ? n : 1));
		if (e->pressures) {
			e->pressure_count = (size_t)n;
			for (ptrdiff_t i = 0; i < n; ++i)
				e->pressures[i] = get_number(
				        env, env->vec_get(env, pressures, i), NAN);
		}
	}
}

static bool read_element(emacs_env *env, emacs_value vec, ExcalElement *e)
{
	memset(e, 0, sizeof *e);
	if (!type_is(env, vec, Qvector) || env->vec_size(env, vec) < SLOT_COUNT)
		return false;
#define SLOT(i) env->vec_get(env, vec, (i))
	char *type = get_string(env, SLOT(SLOT_TYPE));
	e->type = parse_type(type);
	free(type);
	e->x = get_number(env, SLOT(SLOT_X), 0);
	e->y = get_number(env, SLOT(SLOT_Y), 0);
	e->width = get_number(env, SLOT(SLOT_WIDTH), 0);
	e->height = get_number(env, SLOT(SLOT_HEIGHT), 0);
	e->angle = get_number(env, SLOT(SLOT_ANGLE), 0);
	e->stroke_color = get_string(env, SLOT(SLOT_STROKE_COLOR));
	e->background_color = get_string(env, SLOT(SLOT_BACKGROUND_COLOR));
	e->fill_style = get_string(env, SLOT(SLOT_FILL_STYLE));
	e->stroke_width = get_number(env, SLOT(SLOT_STROKE_WIDTH), 2);
	e->roughness = get_number(env, SLOT(SLOT_ROUGHNESS), 1);
	/* JS ToInt32, as roughjs' Math.imul sees the seed.  */
	e->seed = (int32_t)rough_to_uint32(get_number(env, SLOT(SLOT_SEED), 1));
	emacs_value points = SLOT(SLOT_POINTS);
	if (type_is(env, points, Qvector)) {
		ptrdiff_t n = env->vec_size(env, points);
		e->point_count = (size_t)n / 2;
		e->points = malloc(sizeof(double) * (n ? n : 1));
		for (ptrdiff_t i = 0; i < n && e->points; ++i)
			e->points[i] =
			        get_number(env, env->vec_get(env, points, i), 0);
	}
	e->text = get_string(env, SLOT(SLOT_TEXT));
	e->font_size = get_number(env, SLOT(SLOT_FONT_SIZE), 20);
	e->opacity = get_number(env, SLOT(SLOT_OPACITY), 100);
	e->selection = (int)get_number(env, SLOT(SLOT_SELECTED), 0);
	e->stroke_style = get_string(env, SLOT(SLOT_STROKE_STYLE));
	e->font_family = (int)get_number(env, SLOT(SLOT_FONT_FAMILY), 5);
	e->text_align = get_string(env, SLOT(SLOT_TEXT_ALIGN));
	e->line_height = get_number(env, SLOT(SLOT_LINE_HEIGHT), 1.25);
	e->rounded = env->is_not_nil(env, SLOT(SLOT_ROUNDED));
	e->start_arrowhead = get_string(env, SLOT(SLOT_START_ARROWHEAD));
	e->end_arrowhead = get_string(env, SLOT(SLOT_END_ARROWHEAD));
	read_shape_extras(env, SLOT(SLOT_SHAPE_EXTRAS), e);
#undef SLOT
	return env->non_local_exit_check(env) == emacs_funcall_exit_return;
}

/* Read the element vector VEC into a malloc'ed array; set *COUNT.  */
static ExcalElement *read_elements(emacs_env *env, emacs_value vec,
                                   size_t *count)
{
	ptrdiff_t n = env->vec_size(env, vec);
	ExcalElement *elements = calloc(n ? n : 1, sizeof *elements);
	size_t used = 0;
	for (ptrdiff_t i = 0; i < n && elements; ++i) {
		if (read_element(env, env->vec_get(env, vec, i),
		                 &elements[used]))
			++used;
		else
			free_element(&elements[used]);
		if (env->non_local_exit_check(env) != emacs_funcall_exit_return)
			break;
	}
	*count = used;
	return elements;
}

static void free_elements(ExcalElement *elements, size_t count)
{
	for (size_t i = 0; i < count; ++i)
		free_element(&elements[i]);
	free(elements);
}

/* Render the element vector ARGS[ELEMENTS] into PIXELS with VIEW.  */
static emacs_value render_into(emacs_env *env, uint32_t *pixels,
                               ExcalView *view, emacs_value vec)
{
	size_t count;
	ExcalElement *elements = read_elements(env, vec, &count);
	size_t drawn = 0;
	if (elements &&
	    env->non_local_exit_check(env) == emacs_funcall_exit_return)
		drawn = excal_render(pixels, view, elements, count);
	free_elements(elements, count);
	return env->make_integer(env, (intmax_t)drawn);
}

/* (excal-native-render CANVAS WIDTH HEIGHT PIXEL-SCALE ZOOM SCROLL-X
   SCROLL-Y ELEMENTS) */
static emacs_value Fexcal_native_render(emacs_env *env, ptrdiff_t nargs,
                                        emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	ExcalView view = {
	        .width = (int)env->extract_integer(env, args[1]),
	        .height = (int)env->extract_integer(env, args[2]),
	        .pixel_scale = get_number(env, args[3], 1),
	        .zoom = get_number(env, args[4], 1),
	        .scroll_x = get_number(env, args[5], 0),
	        .scroll_y = get_number(env, args[6], 0),
	};
	if (env->non_local_exit_check(env) != emacs_funcall_exit_return ||
	    view.width <= 0 || view.height <= 0)
		return Qnil;
	uint32_t *pixels = env->canvas_data(env, args[0]);
	if (!pixels ||
	    env->non_local_exit_check(env) != emacs_funcall_exit_return)
		return Qnil;
	return render_into(env, pixels, &view, args[7]);
}

/* Module-owned offscreen framebuffer.  */

typedef struct {
	int width, height;
	uint32_t *pixels;
} Framebuffer;

static void framebuffer_free(void *ptr)
{
	Framebuffer *fb = ptr;
	free(fb->pixels);
	free(fb);
}

static Framebuffer *get_framebuffer(emacs_env *env, emacs_value value)
{
	if (!type_is(env, value, Quser_ptr) ||
	    env->get_user_finalizer(env, value) != framebuffer_free)
		return NULL;
	return env->get_user_ptr(env, value);
}

/* (excal-native-fb-create WIDTH HEIGHT) */
static emacs_value Fexcal_native_fb_create(emacs_env *env, ptrdiff_t nargs,
                                           emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	int width = (int)env->extract_integer(env, args[0]);
	int height = (int)env->extract_integer(env, args[1]);
	if (width <= 0 || height <= 0)
		return Qnil;
	Framebuffer *fb = malloc(sizeof *fb);
	fb->width = width;
	fb->height = height;
	fb->pixels = calloc((size_t)width * height, 4);
	return env->make_user_ptr(env, framebuffer_free, fb);
}

/* (excal-native-fb-render FB PIXEL-SCALE ZOOM SCROLL-X SCROLL-Y ELEMENTS
   DAMAGE) */
static emacs_value Fexcal_native_fb_render(emacs_env *env, ptrdiff_t nargs,
                                           emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	Framebuffer *fb = get_framebuffer(env, args[0]);
	if (!fb)
		return Qnil;
	ExcalView view = {
	        .width = fb->width,
	        .height = fb->height,
	        .pixel_scale = get_number(env, args[1], 1),
	        .zoom = get_number(env, args[2], 1),
	        .scroll_x = get_number(env, args[3], 0),
	        .scroll_y = get_number(env, args[4], 0),
	};
	/* DAMAGE is nil, [X Y W H], or a vector of such rectangles.  */
	emacs_value damage = args[6];
	if (type_is(env, damage, Qvector) && env->vec_size(env, damage) > 0) {
		ptrdiff_t n = env->vec_size(env, damage);
		bool nested = type_is(env, env->vec_get(env, damage, 0), Qvector);
		bool overflow = false;
		for (ptrdiff_t i = 0; i < (nested ? n : 1) && !overflow; ++i) {
			emacs_value r = nested ? env->vec_get(env, damage, i)
			                       : damage;
			if (!type_is(env, r, Qvector) || env->vec_size(env, r) != 4)
				continue;
			int x = (int)get_number(env, env->vec_get(env, r, 0), 0);
			int y = (int)get_number(env, env->vec_get(env, r, 1), 0);
			int x2 = x + (int)get_number(env, env->vec_get(env, r, 2), 0);
			int y2 = y + (int)get_number(env, env->vec_get(env, r, 3), 0);
			x = x < 0 ? 0 : x;
			y = y < 0 ? 0 : y;
			x2 = x2 > fb->width ? fb->width : x2;
			y2 = y2 > fb->height ? fb->height : y2;
			if (x2 <= x || y2 <= y)
				continue;
			if (view.clip_count == EXCAL_MAX_CLIPS)
				overflow = true;
			else
				view.clips[view.clip_count++] =
				        (ExcalRect){x, y, x2 - x, y2 - y};
		}
		if (overflow)
			view.clip_count = 0; /* Too many pieces: repaint all.  */
		else if (view.clip_count == 0)
			return env->make_integer(env, 0); /* All offscreen.  */
	}
	return render_into(env, fb->pixels, &view, args[5]);
}

/* (excal-native-fb-scroll FB DX DY)
   Shift FB's pixels by DX, DY device pixels.  */
static emacs_value Fexcal_native_fb_scroll(emacs_env *env, ptrdiff_t nargs,
                                           emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	Framebuffer *fb = get_framebuffer(env, args[0]);
	if (!fb)
		return Qnil;
	excal_scroll(fb->pixels, fb->width, fb->height,
	             (int)env->extract_integer(env, args[1]),
	             (int)env->extract_integer(env, args[2]));
	return Qt;
}

/* (excal-native-fb-present-canvas FB CANVAS)
   Copy the whole framebuffer into a canvas of the same size.  */
static emacs_value Fexcal_native_fb_present_canvas(emacs_env *env,
                                                   ptrdiff_t nargs,
                                                   emacs_value *args,
                                                   void *data)
{
	(void)nargs;
	(void)data;
	Framebuffer *fb = get_framebuffer(env, args[0]);
	uint32_t *pixels = fb ? env->canvas_data(env, args[1]) : NULL;
	if (!pixels)
		return Qnil;
	memcpy(pixels, fb->pixels, (size_t)fb->width * fb->height * 4);
	return Qt;
}

/* (excal-native-fb-present-tiles FB TILES)
   TILES is a vector of [CANVAS X Y WIDTH HEIGHT] in device pixels.  Copy
   each tile whose pixels differ from the framebuffer and return the list
   of their indices, so only those canvases need `canvas-refresh'.  */
static emacs_value Fexcal_native_fb_present_tiles(emacs_env *env,
                                                  ptrdiff_t nargs,
                                                  emacs_value *args,
                                                  void *data)
{
	(void)nargs;
	(void)data;
	Framebuffer *fb = get_framebuffer(env, args[0]);
	if (!fb)
		return Qnil;
	emacs_value result = Qnil;
	emacs_value cons = env->intern(env, "cons");
	ptrdiff_t n = env->vec_size(env, args[1]);
	for (ptrdiff_t i = n - 1; i >= 0; --i) {
		emacs_value tile = env->vec_get(env, args[1], i);
		int x = (int)env->extract_integer(env, env->vec_get(env, tile, 1));
		int y = (int)env->extract_integer(env, env->vec_get(env, tile, 2));
		int w = (int)env->extract_integer(env, env->vec_get(env, tile, 3));
		int h = (int)env->extract_integer(env, env->vec_get(env, tile, 4));
		if (x < 0 || y < 0 || x + w > fb->width || y + h > fb->height)
			continue;
		uint32_t *dst = env->canvas_data(env, env->vec_get(env, tile, 0));
		if (!dst)
			continue;
		bool changed = false;
		for (int row = 0; row < h; ++row) {
			const uint32_t *src =
			        fb->pixels + (size_t)(y + row) * fb->width + x;
			uint32_t *out = dst + (size_t)row * w;
			if (changed || memcmp(out, src, (size_t)w * 4) != 0) {
				memcpy(out, src, (size_t)w * 4);
				changed = true;
			}
		}
		if (changed)
			result = env->funcall(env, cons, 2,
			                      (emacs_value[]){
			                              env->make_integer(env, i),
			                              result});
	}
	return result;
}

/* (excal-native-fb-diff A B)
   Return the largest per-channel difference between framebuffers A and B,
   or nil when their sizes differ.  */
static emacs_value Fexcal_native_fb_diff(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	Framebuffer *a = get_framebuffer(env, args[0]);
	Framebuffer *b = get_framebuffer(env, args[1]);
	if (!a || !b || a->width != b->width || a->height != b->height)
		return Qnil;
	int worst = 0;
	for (size_t i = 0; i < (size_t)a->width * a->height; ++i)
		for (int shift = 0; shift < 32; shift += 8) {
			int d = (int)((a->pixels[i] >> shift) & 0xff) -
			        (int)((b->pixels[i] >> shift) & 0xff);
			if (abs(d) > worst)
				worst = abs(d);
		}
	return env->make_integer(env, worst);
}

/* (excal-native-fb-copy SRC DST)
   Copy SRC's pixels into DST; return nil when their sizes differ.  */
static emacs_value Fexcal_native_fb_copy(emacs_env *env, ptrdiff_t nargs,
                                         emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	Framebuffer *src = get_framebuffer(env, args[0]);
	Framebuffer *dst = get_framebuffer(env, args[1]);
	if (!src || !dst || src->width != dst->width ||
	    src->height != dst->height)
		return Qnil;
	if (src != dst)
		memcpy(dst->pixels, src->pixels,
		       (size_t)src->width * src->height * 4);
	return Qt;
}

/* (excal-native-fb-zoom-preview DST SRC SCALE TX TY)
   Fill DST with SRC scaled by SCALE, then shifted by TX, TY.  */
static emacs_value Fexcal_native_fb_zoom_preview(emacs_env *env,
                                                 ptrdiff_t nargs,
                                                 emacs_value *args,
                                                 void *data)
{
	(void)nargs;
	(void)data;
	Framebuffer *dst = get_framebuffer(env, args[0]);
	Framebuffer *src = get_framebuffer(env, args[1]);
	double scale = get_number(env, args[2], 1);
	double tx = get_number(env, args[3], 0);
	double ty = get_number(env, args[4], 0);
	if (!dst || !src || dst->width != src->width ||
	    dst->height != src->height || !(scale > 0))
		return Qnil;
	return excal_zoom_preview(dst->pixels, src->pixels, dst->width,
	                          dst->height, scale, tx, ty)
	               ? Qt
	               : Qnil;
}

/* (excal-native-fb-mean-diff A B)
   Return the mean colour channel difference of A and B as a float, or
   nil when their sizes differ.  */
static emacs_value Fexcal_native_fb_mean_diff(emacs_env *env, ptrdiff_t nargs,
                                              emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	Framebuffer *a = get_framebuffer(env, args[0]);
	Framebuffer *b = get_framebuffer(env, args[1]);
	if (!a || !b || a->width != b->width || a->height != b->height)
		return Qnil;
	return env->make_float(
	        env, excal_mean_diff(a->pixels, b->pixels, a->width, a->height));
}

/* (excal-native-fb-write-png FB FILE) */
static emacs_value Fexcal_native_fb_write_png(emacs_env *env, ptrdiff_t nargs,
                                              emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	Framebuffer *fb = get_framebuffer(env, args[0]);
	char *path = get_string(env, args[1]);
	bool ok = fb && path &&
	          excal_write_png(fb->pixels, fb->width, fb->height, path);
	free(path);
	return ok ? Qt : Qnil;
}

#ifdef EXCAL_HAVE_LAYER
/* CoreAnimation overlay (macOS only).  */

static Framebuffer *layer_fb_arg(emacs_env *env, emacs_value value)
{
	return get_framebuffer(env, value);
}

static void *get_layer(emacs_env *env, emacs_value value)
{
	if (!type_is(env, value, Quser_ptr) ||
	    env->get_user_finalizer(env, value) != excal_layer_destroy)
		return NULL;
	return env->get_user_ptr(env, value);
}

/* (excal-native-layer-create LEFT TOP WIDTH HEIGHT) */
static emacs_value Fexcal_native_layer_create(emacs_env *env, ptrdiff_t nargs,
                                              emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	void *layer = excal_layer_create(
	        get_number(env, args[0], 0), get_number(env, args[1], 0),
	        get_number(env, args[2], 0), get_number(env, args[3], 0));
	return layer ? env->make_user_ptr(env, excal_layer_destroy, layer)
	             : Qnil;
}

/* (excal-native-layer-set-geometry LAYER X Y WIDTH HEIGHT SCALE VISIBLE) */
static emacs_value Fexcal_native_layer_set_geometry(emacs_env *env,
                                                    ptrdiff_t nargs,
                                                    emacs_value *args,
                                                    void *data)
{
	(void)nargs;
	(void)data;
	void *layer = get_layer(env, args[0]);
	if (!layer)
		return Qnil;
	excal_layer_set_geometry(layer, get_number(env, args[1], 0),
	                         get_number(env, args[2], 0),
	                         get_number(env, args[3], 0),
	                         get_number(env, args[4], 0),
	                         get_number(env, args[5], 1),
	                         env->is_not_nil(env, args[6]));
	return Qt;
}

/* (excal-native-layer-present LAYER FB) */
static emacs_value Fexcal_native_layer_present(emacs_env *env, ptrdiff_t nargs,
                                               emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	void *layer = get_layer(env, args[0]);
	Framebuffer *fb = layer_fb_arg(env, args[1]);
	if (!layer || !fb)
		return Qnil;
	return excal_layer_present(layer, fb->pixels, fb->width, fb->height)
	               ? Qt
	               : Qnil;
}

/* (excal-native-layer-flush) */
static emacs_value Fexcal_native_layer_flush(emacs_env *env, ptrdiff_t nargs,
                                             emacs_value *args, void *data)
{
	(void)nargs;
	(void)args;
	(void)data;
	excal_layer_flush();
	return Qt;
}
#endif

/* Shape introspection, for tests and other Elisp code.  */

static emacs_value make_vector(emacs_env *env, ptrdiff_t n,
                               const emacs_value *items)
{
	return env->funcall(env, env->intern(env, "vector"), n,
	                    (emacs_value *)items);
}

static emacs_value floats_vector(emacs_env *env, const double *xs, size_t n)
{
	emacs_value *items = malloc(sizeof *items * (n ? n : 1));
	if (!items)
		return Qnil;
	for (size_t i = 0; i < n; ++i)
		items[i] = env->make_float(env, xs[i]);
	emacs_value v = make_vector(env, (ptrdiff_t)n, items);
	free(items);
	return v;
}

static emacs_value ops_vector(emacs_env *env, const RoughOps *ops)
{
	emacs_value *items = malloc(sizeof *items * (ops->count ? ops->count : 1));
	if (!items)
		return Qnil;
	static const char *names[] = {"move", "lineTo", "bcurveTo"};
	for (size_t i = 0; i < ops->count; ++i) {
		const RoughOp *op = &ops->ops[i];
		int n = op->op == ROUGH_BCURVE_TO ? 6 : 2;
		emacs_value parts[7];
		parts[0] = env->make_string(env, names[op->op],
		                            (ptrdiff_t)strlen(names[op->op]));
		for (int k = 0; k < n; ++k)
			parts[k + 1] = env->make_float(env, op->data[k]);
		items[i] = make_vector(env, n + 1, parts);
	}
	emacs_value v = make_vector(env, (ptrdiff_t)ops->count, items);
	free(items);
	return v;
}

static emacs_value make_str(emacs_env *env, const char *s)
{
	return env->make_string(env, s, (ptrdiff_t)strlen(s));
}

/* (excal-native-element-shape ELEMENT)
   Return [DRAWABLES OUTLINE COORDS] for the native element vector
   ELEMENT, as generated for rendering.  DRAWABLES is a vector of
   [SHAPE SETS FILL] where SHAPE names the roughjs generator, FILL is
   "element", "stroke" or "canvas", and SETS is a vector of [TYPE OPS]
   with TYPE "path", "fillPath" or "fillSketch" and OPS a vector of
   ["move" X Y], ["lineTo" X Y] or ["bcurveTo" X1 Y1 X2 Y2 X Y].
   OUTLINE is the freedraw outline as [QX QY EX EY ...] (quadratic
   control and end points), and COORDS is getElementAbsoluteCoords
   [X1 Y1 X2 Y2 CX CY] in scene coordinates.  All ops are relative to
   the element's x, y.  */
static emacs_value Fexcal_native_element_shape(emacs_env *env, ptrdiff_t nargs,
                                               emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	ExcalElement e;
	if (!read_element(env, args[0], &e)) {
		free_element(&e);
		return Qnil;
	}
	ExcalShape shape;
	excal_shape_generate(&e, &shape);
	static const char *shapes[] = {"line",       "rectangle", "ellipse",
	                               "circle",     "linearPath", "curve",
	                               "polygon",    "path"};
	static const char *sets[] = {"path", "fillPath", "fillSketch"};
	static const char *fills[] = {"element", "stroke", "canvas"};
	emacs_value drawables[EXCAL_SHAPE_MAX_DRAWABLES];
	for (int i = 0; i < shape.count; ++i) {
		const ExcalDrawable *d = &shape.items[i];
		emacs_value set_values[ROUGH_MAX_SETS];
		for (int k = 0; k < d->rough.set_count; ++k) {
			emacs_value pair[2] = {
			        make_str(env, sets[d->rough.sets[k].type]),
			        ops_vector(env, &d->rough.sets[k].ops)};
			set_values[k] = make_vector(env, 2, pair);
		}
		emacs_value parts[3] = {
		        make_str(env, shapes[d->rough.shape]),
		        make_vector(env, d->rough.set_count, set_values),
		        make_str(env, fills[d->fill_source])};
		drawables[i] = make_vector(env, 3, parts);
	}
	double coords[6] = {
	        e.x + shape.x1, e.y + shape.y1, e.x + shape.x2, e.y + shape.y2,
	        e.x + (shape.x1 + shape.x2) / 2, e.y + (shape.y1 + shape.y2) / 2};
	emacs_value result[3] = {
	        make_vector(env, shape.count, drawables),
	        floats_vector(env, shape.outline.xy, shape.outline.count * 2),
	        floats_vector(env, coords, 6)};
	excal_shape_free(&shape);
	free_element(&e);
	return make_vector(env, 3, result);
}

/* (excal-native-rough-random SEED COUNT)
   Return the first COUNT numbers of roughjs' Random seeded with SEED.  */
static emacs_value Fexcal_native_rough_random(emacs_env *env, ptrdiff_t nargs,
                                              emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	RoughRandom r;
	rough_random_init(&r, get_number(env, args[0], 0));
	intmax_t n = env->extract_integer(env, args[1]);
	if (n < 0 || n > 100000)
		return Qnil;
	double *xs = malloc(sizeof *xs * (n ? n : 1));
	if (!xs)
		return Qnil;
	for (intmax_t i = 0; i < n; ++i)
		xs[i] = rough_random_next(&r);
	emacs_value v = floats_vector(env, xs, (size_t)n);
	free(xs);
	return v;
}

/* (excal-native-measure-text TEXT FONT-SIZE FONT-FAMILY LINE-HEIGHT) */
static emacs_value Fexcal_native_measure_text(emacs_env *env, ptrdiff_t nargs,
                                              emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	char *text = get_string(env, args[0]);
	if (!text)
		return Qnil;
	double width, height;
	excal_measure_text(text, get_number(env, args[1], 20),
	                   (int)get_number(env, args[2], 5),
	                   get_number(env, args[3], 1.25), &width, &height);
	free(text);
	emacs_value cons = env->intern(env, "cons");
	return env->funcall(env, cons, 2,
	                    (emacs_value[]){env->make_float(env, width),
	                                    env->make_float(env, height)});
}

/* (excal-native-write-png CANVAS WIDTH HEIGHT FILE) */
static emacs_value Fexcal_native_write_png(emacs_env *env, ptrdiff_t nargs,
                                           emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	int width = (int)env->extract_integer(env, args[1]);
	int height = (int)env->extract_integer(env, args[2]);
	char *path = get_string(env, args[3]);
	uint32_t *pixels = env->canvas_data(env, args[0]);
	bool ok = path && pixels && width > 0 && height > 0 &&
	          excal_write_png(pixels, width, height, path);
	free(path);
	return ok ? Qt : Qnil;
}

static void bind(emacs_env *env, const char *name,
                 emacs_value (*fn)(emacs_env *, ptrdiff_t, emacs_value *,
                                   void *),
                 ptrdiff_t arity, const char *doc)
{
	emacs_value function =
	        env->make_function(env, arity, arity, fn, doc, NULL);
	env->funcall(env, env->intern(env, "defalias"), 2,
	             (emacs_value[]){env->intern(env, name), function});
}

int emacs_module_init(struct emacs_runtime *runtime)
{
	if (runtime->size < (ptrdiff_t)sizeof *runtime)
		return 1;
	emacs_env *env = runtime->get_environment(runtime);
	if (env->size < (ptrdiff_t)sizeof *env)
		return 2;
#define GLOBAL(name) env->make_global_ref(env, env->intern(env, name))
	Qnil = GLOBAL("nil");
	Qt = GLOBAL("t");
	Qinteger = GLOBAL("integer");
	Qfloat = GLOBAL("float");
	Qstring = GLOBAL("string");
	Qvector = GLOBAL("vector");
	Quser_ptr = GLOBAL("user-ptr");
#undef GLOBAL
	bind(env, "excal-native-render", Fexcal_native_render, 8,
	     "Render ELEMENTS into CANVAS.\n\n"
	     "(fn CANVAS WIDTH HEIGHT PIXEL-SCALE ZOOM SCROLL-X SCROLL-Y "
	     "ELEMENTS)");
	bind(env, "excal-native-element-shape", Fexcal_native_element_shape, 1,
	     "Return the generated shape of the native element vector ELEMENT.\n\n"
	     "The result is [DRAWABLES OUTLINE COORDS]; see excal-module.c.\n\n"
	     "(fn ELEMENT)");
	bind(env, "excal-native-rough-random", Fexcal_native_rough_random, 2,
	     "Return the first COUNT numbers of roughjs' Random for SEED.\n\n"
	     "(fn SEED COUNT)");
	bind(env, "excal-native-measure-text", Fexcal_native_measure_text, 4,
	     "Return (WIDTH . HEIGHT) of TEXT in scene units.\n\n"
	     "(fn TEXT FONT-SIZE FONT-FAMILY LINE-HEIGHT)");
	bind(env, "excal-native-write-png", Fexcal_native_write_png, 4,
	     "Write CANVAS pixels to FILE as PNG.\n\n"
	     "(fn CANVAS WIDTH HEIGHT FILE)");
	bind(env, "excal-native-fb-create", Fexcal_native_fb_create, 2,
	     "Return a WIDTH by HEIGHT offscreen framebuffer.\n\n"
	     "(fn WIDTH HEIGHT)");
	bind(env, "excal-native-fb-render", Fexcal_native_fb_render, 7,
	     "Render ELEMENTS into FB, repainting only DAMAGE if non-nil.\n\n"
	     "DAMAGE is a vector [X Y WIDTH HEIGHT] in device pixels, or a\n"
	     "vector of such vectors.\n"
	     "Return the number of elements drawn.\n\n"
	     "(fn FB PIXEL-SCALE ZOOM SCROLL-X SCROLL-Y ELEMENTS DAMAGE)");
	bind(env, "excal-native-fb-scroll", Fexcal_native_fb_scroll, 3,
	     "Shift FB's pixels by DX, DY device pixels.\n\n"
	     "Pixels shifted in keep stale contents and must be repainted.\n\n"
	     "(fn FB DX DY)");
	bind(env, "excal-native-fb-present-canvas",
	     Fexcal_native_fb_present_canvas, 2,
	     "Copy FB into CANVAS of the same size.\n\n(fn FB CANVAS)");
	bind(env, "excal-native-fb-present-tiles",
	     Fexcal_native_fb_present_tiles, 2,
	     "Copy changed tiles of FB into their canvases.\n\n"
	     "TILES is a vector of [CANVAS X Y WIDTH HEIGHT].  Return the\n"
	     "list of indices of tiles whose pixels changed.\n\n"
	     "(fn FB TILES)");
	bind(env, "excal-native-fb-diff", Fexcal_native_fb_diff, 2,
	     "Return the largest channel difference between A and B.\n\n"
	     "(fn A B)");
	bind(env, "excal-native-fb-copy", Fexcal_native_fb_copy, 2,
	     "Copy SRC's pixels into DST of the same size.\n\n"
	     "Return nil when the sizes differ.\n\n(fn SRC DST)");
	bind(env, "excal-native-fb-zoom-preview",
	     Fexcal_native_fb_zoom_preview, 5,
	     "Fill DST with SRC scaled by SCALE, then shifted by TX, TY.\n\n"
	     "A device pixel P of SRC lands at SCALE * P + (TX, TY) in DST,\n"
	     "with bilinear filtering; uncovered pixels become white.  DST\n"
	     "may be SRC.  Return nil when the sizes differ.\n\n"
	     "(fn DST SRC SCALE TX TY)");
	bind(env, "excal-native-fb-mean-diff", Fexcal_native_fb_mean_diff, 2,
	     "Return the mean colour channel difference of A and B.\n\n"
	     "The result is a float in levels 0..255, or nil when the sizes\n"
	     "differ.\n\n(fn A B)");
	bind(env, "excal-native-fb-write-png", Fexcal_native_fb_write_png, 2,
	     "Write FB to FILE as PNG.\n\n(fn FB FILE)");
#ifdef EXCAL_HAVE_LAYER
	bind(env, "excal-native-layer-create", Fexcal_native_layer_create, 4,
	     "Attach an overlay layer to the Emacs view at screen rectangle.\n\n"
	     "LEFT, TOP, WIDTH and HEIGHT are the frame's native edges in\n"
	     "screen pixels.  Return nil when no view matches.\n\n"
	     "(fn LEFT TOP WIDTH HEIGHT)");
	bind(env, "excal-native-layer-set-geometry",
	     Fexcal_native_layer_set_geometry, 7,
	     "Place LAYER at view rectangle X Y WIDTH HEIGHT.\n\n"
	     "(fn LAYER X Y WIDTH HEIGHT SCALE VISIBLE)");
	bind(env, "excal-native-layer-present", Fexcal_native_layer_present, 2,
	     "Show FB's pixels in LAYER.\n\n(fn LAYER FB)");
	bind(env, "excal-native-layer-flush", Fexcal_native_layer_flush, 0,
	     "Commit pending CoreAnimation changes.\n\n(fn)");
#endif
	env->funcall(env, env->intern(env, "provide"), 1,
	             (emacs_value[]){env->intern(env, "excal-module")});
	return 0;
}
