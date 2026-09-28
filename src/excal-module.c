/* excal-module.c --- Emacs module glue for excal.el  -*- c-file-style: "linux" -*- */

#include <emacs-module.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>

#include "excal-export.h"
#include "excal-frame.h"
#include "excal-image.h"
#include "excal-preview.h"
#include "excal-render.h"
#include "excal-text.h"
#include "excal-rough.h"
#include "excal-shape.h"
#include "excal-sticky.h"
#ifdef EXCAL_HAVE_LAYER
#include "excal-layer.h"
#include "excal-cursor.h"
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
	/* Optional: vectors may stop before it.  */
	SLOT_MEDIA_EXTRAS = SLOT_COUNT, /* See `excal--native-media-extras'.  */
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

/* Read the text layout extras, see `excal--native-text-extras'.  */
static void read_text_extras(emacs_env *env, emacs_value extras,
                             ExcalElement *e)
{
	emacs_value offset = get_extra(env, extras, "vertical-offset");
	if (type_is(env, offset, Qinteger) || type_is(env, offset, Qfloat)) {
		e->has_text_offset = true;
		e->text_offset = get_number(env, offset, 0);
	}
	emacs_value hole = get_extra(env, extras, "label-hole");
	if (type_is(env, hole, Qvector) && env->vec_size(env, hole) == 4) {
		e->has_label_hole = true;
		for (int i = 0; i < 4; ++i)
			e->label_hole[i] =
			        get_number(env, env->vec_get(env, hole, i), 0);
	}
}

/* Read the image and frame extras, see `excal--native-media-extras'.  */
static void read_media_extras(emacs_env *env, emacs_value extras,
                              ExcalElement *e)
{
	ExcalMedia *m = &e->media;
	m->scale[0] = m->scale[1] = 1;
	if (!type_is(env, extras, Qvector))
		return;
	m->id = get_extra_string(env, extras, "id");
	m->frame_id = get_extra_string(env, extras, "frame-id");
	m->grouped = env->is_not_nil(env, get_extra(env, extras, "grouped"));
	m->magic = env->is_not_nil(env, get_extra(env, extras, "magic"));
	m->name = get_extra_string(env, extras, "name");
	m->file_id = get_extra_string(env, extras, "file-id");
	m->error = env->is_not_nil(env, get_extra(env, extras, "error"));
	m->radius = get_extra_number(env, extras, "radius", 0);
	emacs_value scale = get_extra(env, extras, "scale");
	if (type_is(env, scale, Qvector) && env->vec_size(env, scale) == 2)
		for (int i = 0; i < 2; ++i)
			m->scale[i] = get_number(
			        env, env->vec_get(env, scale, i), 1);
	emacs_value crop = get_extra(env, extras, "crop");
	if (type_is(env, crop, Qvector) && env->vec_size(env, crop) == 6) {
		m->has_crop = true;
		for (int i = 0; i < 6; ++i)
			m->crop[i] =
			        get_number(env, env->vec_get(env, crop, i), 0);
	}
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
	        {"text", EXCAL_TEXT},           {"stickynote", EXCAL_STICKYNOTE},
	        {"ov-rect", EXCAL_OV_RECT},
	        {"ov-handle", EXCAL_OV_HANDLE}, {"ov-circle", EXCAL_OV_CIRCLE},
	        {"ov-ellipse", EXCAL_OV_ELLIPSE}, {"ov-diamond", EXCAL_OV_DIAMOND},
	        {"ov-poly", EXCAL_OV_POLY},       {"ov-grid", EXCAL_OV_GRID},
	        {"image", EXCAL_IMAGE},           {"frame", EXCAL_FRAME},
	        {"magicframe", EXCAL_FRAME},
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
	free(e->sticky_footer);
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
	excal_media_free(&e->media);
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
	e->stroke_style = get_string(env, SLOT(SLOT_STROKE_STYLE));
	e->font_family = (int)get_number(env, SLOT(SLOT_FONT_FAMILY), 5);
	e->text_align = get_string(env, SLOT(SLOT_TEXT_ALIGN));
	e->line_height = get_number(env, SLOT(SLOT_LINE_HEIGHT), 1.25);
	e->rounded = env->is_not_nil(env, SLOT(SLOT_ROUNDED));
	e->start_arrowhead = get_string(env, SLOT(SLOT_START_ARROWHEAD));
	e->end_arrowhead = get_string(env, SLOT(SLOT_END_ARROWHEAD));
	read_text_extras(env, SLOT(SLOT_TEXT_EXTRAS), e);
	read_shape_extras(env, SLOT(SLOT_SHAPE_EXTRAS), e);
	e->sticky_footer = get_extra_string(env, SLOT(SLOT_SHAPE_EXTRAS), "stickyFooter");
	read_media_extras(env,
	                  env->vec_size(env, vec) > SLOT_MEDIA_EXTRAS
	                          ? SLOT(SLOT_MEDIA_EXTRAS)
	                          : Qnil,
	                  e);
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
	(void)data;
	Framebuffer *fb = get_framebuffer(env, args[0]);
	if (!fb)
		return Qnil;
	/* The optional BACKGROUND lives until the render is done.  */
	char *background = nargs > 7 ? get_string(env, args[7]) : NULL;
	ExcalView view = {
	        .width = fb->width,
	        .height = fb->height,
	        .pixel_scale = get_number(env, args[1], 1),
	        .zoom = get_number(env, args[2], 1),
	        .scroll_x = get_number(env, args[3], 0),
	        .scroll_y = get_number(env, args[4], 0),
	        .background_color = background,
	        .dark = nargs > 8 && env->is_not_nil(env, args[8]),
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
		else if (view.clip_count == 0) {
			free(background);
			return env->make_integer(env, 0); /* All offscreen.  */
		}
	}
	emacs_value drawn = render_into(env, fb->pixels, &view, args[5]);
	free(background);
	return drawn;
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

/* (excal-native-seeded-random SEED COUNT)
   Return COUNT numbers from upstream seededRandom (mulberry32).  */
static emacs_value Fexcal_native_seeded_random(emacs_env *env, ptrdiff_t nargs,
                                               emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	ExcalMulberry m;
	excal_mulberry_init(&m, get_number(env, args[0], 0));
	intmax_t count = env->extract_integer(env, args[1]);
	if (count < 0 || count > 1000)
		return Qnil;
	emacs_value vector = env->funcall(
	        env, env->intern(env, "make-vector"), 2,
	        (emacs_value[]){env->make_integer(env, count), Qnil});
	for (intmax_t i = 0; i < count; ++i)
		env->vec_set(env, vector, i,
		             env->make_float(env, excal_mulberry_next(&m)));
	return vector;
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

/* Pointer shapes over the canvas (macOS only).  */

static void *get_cursor_view(emacs_env *env, emacs_value value)
{
	if (!type_is(env, value, Quser_ptr) ||
	    env->get_user_finalizer(env, value) != excal_cursor_view_destroy)
		return NULL;
	return env->get_user_ptr(env, value);
}

/* (excal-native-cursor-view-create LEFT TOP WIDTH HEIGHT) */
static emacs_value Fexcal_native_cursor_view_create(emacs_env *env,
                                                    ptrdiff_t nargs,
                                                    emacs_value *args,
                                                    void *data)
{
	(void)nargs;
	(void)data;
	void *view = excal_cursor_view_create(
	        get_number(env, args[0], 0), get_number(env, args[1], 0),
	        get_number(env, args[2], 0), get_number(env, args[3], 0));
	return view ? env->make_user_ptr(env, excal_cursor_view_destroy, view)
	            : Qnil;
}

/* (excal-native-cursor-view-set-geometry VIEW X Y WIDTH HEIGHT VISIBLE) */
static emacs_value Fexcal_native_cursor_view_set_geometry(emacs_env *env,
                                                          ptrdiff_t nargs,
                                                          emacs_value *args,
                                                          void *data)
{
	(void)nargs;
	(void)data;
	void *view = get_cursor_view(env, args[0]);
	if (!view)
		return Qnil;
	excal_cursor_view_set_geometry(view, get_number(env, args[1], 0),
	                               get_number(env, args[2], 0),
	                               get_number(env, args[3], 0),
	                               get_number(env, args[4], 0),
	                               env->is_not_nil(env, args[5]));
	return Qt;
}

/* (excal-native-cursor-set VIEW NAME) */
static emacs_value Fexcal_native_cursor_set(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	void *view = get_cursor_view(env, args[0]);
	char *name = get_string(env, args[1]);
	bool ok = view && name && excal_cursor_set(view, name);
	free(name);
	return ok ? Qt : Qnil;
}

/* (excal-native-cursor-known-p NAME) */
static emacs_value Fexcal_native_cursor_known_p(emacs_env *env,
                                                ptrdiff_t nargs,
                                                emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	char *name = get_string(env, args[0]);
	bool ok = name && excal_cursor_known(name);
	free(name);
	return ok ? Qt : Qnil;
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
   Return [DRAWABLES OUTLINE COORDS PADDING] for the native element vector
   ELEMENT, as generated for rendering.  DRAWABLES is a vector of
   [SHAPE SETS FILL] where SHAPE names the roughjs generator, FILL is
   "element", "stroke" or "canvas", and SETS is a vector of [TYPE OPS]
   with TYPE "path", "fillPath" or "fillSketch" and OPS a vector of
   ["move" X Y], ["lineTo" X Y] or ["bcurveTo" X1 Y1 X2 Y2 X Y].
   OUTLINE is the freedraw outline as [QX QY EX EY ...] (quadratic
   control and end points), COORDS is getElementAbsoluteCoords
   [X1 Y1 X2 Y2 CX CY] in scene coordinates, and PADDING is how far
   past its box or points the renderer assumes the element may draw
   (for culling).  All ops are relative to the element's x, y.  */
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
	emacs_value result[4] = {
	        make_vector(env, shape.count, drawables),
	        floats_vector(env, shape.outline.xy, shape.outline.count * 2),
	        floats_vector(env, coords, 6),
	        env->make_float(env, excal_shape_padding(&e))};
	excal_shape_free(&shape);
	free_element(&e);
	return make_vector(env, 4, result);
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

/* (excal-native-text-width LINE FONT-SIZE FONT-FAMILY) */
static emacs_value Fexcal_native_text_width(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	char *text = get_string(env, args[0]);
	if (!text)
		return Qnil;
	double width = excal_text_line_width(text, get_number(env, args[1], 20),
	                                     (int)get_number(env, args[2], 5));
	free(text);
	return env->make_float(env, width);
}

/* (excal-native-add-fonts PATH) */
static emacs_value Fexcal_native_add_fonts(emacs_env *env, ptrdiff_t nargs,
                                           emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	char *path = get_string(env, args[0]);
	if (!path)
		return Qnil;
	int count = excal_text_add_fonts(path);
	free(path);
	return count < 0 ? Qnil : env->make_integer(env, count);
}

/* (excal-native-set-font-family ID FAMILIES) */
static emacs_value Fexcal_native_set_font_family(emacs_env *env,
                                                 ptrdiff_t nargs,
                                                 emacs_value *args,
                                                 void *data)
{
	(void)nargs;
	(void)data;
	char *families = get_string(env, args[1]);
	excal_text_set_family((int)get_number(env, args[0], 0), families);
	free(families);
	return Qt;
}

/* (excal-native-font-family ID) */
static emacs_value Fexcal_native_font_family(emacs_env *env, ptrdiff_t nargs,
                                             emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	const char *families =
	        excal_text_family((int)get_number(env, args[0], 0));
	return env->make_string(env, families, (ptrdiff_t)strlen(families));
}

/* (excal-native-font-resolve TEXT FONT-FAMILY) */
static emacs_value Fexcal_native_font_resolve(emacs_env *env,
                                              ptrdiff_t nargs,
                                              emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	char *text = get_string(env, args[0]);
	if (!text)
		return Qnil;
	char *names = excal_text_resolve(text, (int)get_number(env, args[1], 5));
	free(text);
	emacs_value result =
	        env->make_string(env, names, (ptrdiff_t)strlen(names));
	free(names);
	return result;
}

/* (excal-native-font-backend) */
static emacs_value Fexcal_native_font_backend(emacs_env *env, ptrdiff_t nargs,
                                              emacs_value *args, void *data)
{
	(void)nargs;
	(void)args;
	(void)data;
	const char *name = excal_text_backend();
	return env->make_string(env, name, (ptrdiff_t)strlen(name));
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

static void bind_range(emacs_env *env, const char *name,
                       emacs_value (*fn)(emacs_env *, ptrdiff_t, emacs_value *,
                                         void *),
                       ptrdiff_t min_arity, ptrdiff_t max_arity, const char *doc)
{
	emacs_value function =
	        env->make_function(env, min_arity, max_arity, fn, doc, NULL);
	env->funcall(env, env->intern(env, "defalias"), 2,
	             (emacs_value[]){env->intern(env, name), function});
}

/* Images, frames and export; see excal-image.c, excal-frame.c and
   excal-export.c.  */

/* Return the bytes of string VALUE (unibyte strings keep raw bytes);
   set *LEN, which excludes the terminating NUL.  */
static char *get_bytes(emacs_env *env, emacs_value value, size_t *len)
{
	if (!type_is(env, value, Qstring))
		return NULL;
	ptrdiff_t size = 0;
	env->copy_string_contents(env, value, NULL, &size);
	char *buffer = malloc(size > 0 ? size : 1);
	if (buffer && !env->copy_string_contents(env, value, buffer, &size)) {
		free(buffer);
		return NULL;
	}
	*len = size > 0 ? (size_t)size - 1 : 0;
	return buffer;
}

static emacs_value image_info_vector(emacs_env *env, const char *id)
{
	double w, h;
	const char *mime;
	if (!excal_image_info(id, &w, &h, &mime))
		return Qnil;
	return env->funcall(
	        env, env->intern(env, "vector"), 3,
	        (emacs_value[]){env->make_float(env, w),
	                        env->make_float(env, h),
	                        env->make_string(env, mime,
	                                         (ptrdiff_t)strlen(mime))});
}

/* (excal-native-image-register FILE-ID DATA-URL) */
static emacs_value Fexcal_native_image_register(emacs_env *env,
                                                ptrdiff_t nargs,
                                                emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	char *id = get_string(env, args[0]);
	size_t len = 0;
	char *url = get_bytes(env, args[1], &len);
	const char *error = NULL;
	bool ok = id && url && excal_image_register(id, url, len, &error);
	emacs_value result = ok ? image_info_vector(env, id) : Qnil;
	free(id);
	free(url);
	return result;
}

/* (excal-native-image-forget FILE-ID) */
static emacs_value Fexcal_native_image_forget(emacs_env *env, ptrdiff_t nargs,
                                              emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	char *id = get_string(env, args[0]);
	bool ok = id && excal_image_forget(id);
	free(id);
	return ok ? Qt : Qnil;
}

/* (excal-native-image-info FILE-ID) */
static emacs_value Fexcal_native_image_info(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	char *id = get_string(env, args[0]);
	emacs_value result = id ? image_info_vector(env, id) : Qnil;
	free(id);
	return result;
}

/* (excal-native-image-count) */
static emacs_value Fexcal_native_image_count(emacs_env *env, ptrdiff_t nargs,
                                             emacs_value *args, void *data)
{
	(void)nargs;
	(void)args;
	(void)data;
	return env->make_integer(env, (intmax_t)excal_image_count());
}

/* (excal-native-image-png FILE-ID MAX-SIZE) */
static emacs_value Fexcal_native_image_png(emacs_env *env, ptrdiff_t nargs,
                                           emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	char *id = get_string(env, args[0]);
	size_t len = 0;
	unsigned char *png =
	        id ? excal_image_png(id, (int)get_number(env, args[1], 0), &len)
	           : NULL;
	free(id);
	if (!png)
		return Qnil;
	emacs_value result =
	        env->make_unibyte_string(env, (const char *)png, (ptrdiff_t)len);
	free(png);
	return result;
}

static ExcalExport export_options(emacs_env *env, emacs_value *args,
                                  char **background)
{
	*background = get_string(env, args[4]);
	return (ExcalExport){
	        .x = get_number(env, args[0], 0),
	        .y = get_number(env, args[1], 0),
	        .width = get_number(env, args[2], 0),
	        .height = get_number(env, args[3], 0),
	        .background = *background,
	        .clip = env->is_not_nil(env, args[5]),
	        .outline = env->is_not_nil(env, args[6]),
	};
}

/* (excal-native-export-png FILE ELEMENTS X Y WIDTH HEIGHT BACKGROUND CLIP
   OUTLINE SCALE TEXT) */
static emacs_value Fexcal_native_export_png(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	char *path = get_string(env, args[0]);
	char *background;
	ExcalExport opts = export_options(env, args + 2, &background);
	opts.scale = get_number(env, args[9], 1);
	size_t text_len = 0;
	char *text = get_bytes(env, args[10], &text_len);
	size_t count = 0;
	ExcalElement *elements = read_elements(env, args[1], &count);
	bool ok = path && elements &&
	          env->non_local_exit_check(env) == emacs_funcall_exit_return &&
	          excal_export_png(elements, count, &opts,
	                           "application/vnd.excalidraw+json",
	                           (const unsigned char *)text, text_len, path);
	if (elements)
		free_elements(elements, count);
	free(path);
	free(background);
	free(text);
	return ok ? Qt : Qnil;
}

/* (excal-native-export-svg ELEMENTS X Y WIDTH HEIGHT BACKGROUND CLIP
   OUTLINE) */
static emacs_value Fexcal_native_export_svg(emacs_env *env, ptrdiff_t nargs,
                                            emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	char *background;
	ExcalExport opts = export_options(env, args + 1, &background);
	opts.scale = 1;
	size_t count = 0;
	ExcalElement *elements = read_elements(env, args[0], &count);
	size_t len = 0;
	char *svg = elements && env->non_local_exit_check(env) ==
	                                emacs_funcall_exit_return
	                    ? excal_export_svg(elements, count, &opts, &len)
	                    : NULL;
	if (elements)
		free_elements(elements, count);
	free(background);
	if (!svg)
		return Qnil;
	emacs_value result = env->make_string(env, svg, (ptrdiff_t)len);
	free(svg);
	return result;
}

/* zlib for embedded scenes.  */
static emacs_value zlib_call(emacs_env *env, emacs_value value, bool inflate)
{
	size_t len = 0, out_len = 0;
	char *bytes = get_bytes(env, value, &len);
	if (!bytes)
		return Qnil;
	unsigned char *out =
	        inflate ? excal_zlib_decompress((unsigned char *)bytes, len,
	                                        &out_len)
	                : excal_zlib_compress((unsigned char *)bytes, len,
	                                      &out_len);
	free(bytes);
	if (!out)
		return Qnil;
	emacs_value result = env->make_unibyte_string(env, (const char *)out,
	                                              (ptrdiff_t)out_len);
	free(out);
	return result;
}

/* (excal-native-zlib-compress STRING) */
static emacs_value Fexcal_native_zlib_compress(emacs_env *env,
                                               ptrdiff_t nargs,
                                               emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	return zlib_call(env, args[0], false);
}

/* (excal-native-zlib-decompress STRING) */
static emacs_value Fexcal_native_zlib_decompress(emacs_env *env,
                                                 ptrdiff_t nargs,
                                                 emacs_value *args,
                                                 void *data)
{
	(void)nargs;
	(void)data;
	return zlib_call(env, args[0], true);
}

/* (excal-native-frame-label TITLE MAX-WIDTH) */
static emacs_value Fexcal_native_frame_label(emacs_env *env, ptrdiff_t nargs,
                                             emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	char *title = get_string(env, args[0]);
	if (!title)
		return Qnil;
	double width = 0;
	char *text = excal_frame_label_text(title, get_number(env, args[1], 0),
	                                    &width);
	free(title);
	emacs_value result = env->funcall(
	        env, env->intern(env, "cons"), 2,
	        (emacs_value[]){env->make_string(env, text,
	                                         (ptrdiff_t)strlen(text)),
	                        env->make_float(env, width)});
	free(text);
	return result;
}

/* (excal-native-fb-pixel FB X Y) */
static emacs_value Fexcal_native_fb_pixel(emacs_env *env, ptrdiff_t nargs,
                                          emacs_value *args, void *data)
{
	(void)nargs;
	(void)data;
	Framebuffer *fb = get_framebuffer(env, args[0]);
	int x = (int)get_number(env, args[1], -1);
	int y = (int)get_number(env, args[2], -1);
	if (!fb || x < 0 || y < 0 || x >= fb->width || y >= fb->height)
		return Qnil;
	return env->make_integer(env, fb->pixels[(size_t)y * fb->width + x]);
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
	     "The result is [DRAWABLES OUTLINE COORDS PADDING]; see\n"
	     "excal-module.c.\n\n"
	     "(fn ELEMENT)");
	bind(env, "excal-native-rough-random", Fexcal_native_rough_random, 2,
	     "Return the first COUNT numbers of roughjs' Random for SEED.\n\n"
	     "(fn SEED COUNT)");
	bind(env, "excal-native-measure-text", Fexcal_native_measure_text, 4,
	     "Return (WIDTH . HEIGHT) of TEXT in scene units.\n\n"
	     "(fn TEXT FONT-SIZE FONT-FAMILY LINE-HEIGHT)");
	bind(env, "excal-native-text-width", Fexcal_native_text_width, 3,
	     "Return the advance width of the single line LINE.\n\n"
	     "Newlines are not interpreted.\n\n"
	     "(fn LINE FONT-SIZE FONT-FAMILY)");
	bind(env, "excal-native-add-fonts", Fexcal_native_add_fonts, 1,
	     "Register the font file PATH, or the font files under directory\n"
	     "PATH, with the font backend.  Return the number registered, or\n"
	     "nil if PATH cannot be read.\n\n(fn PATH)");
	bind(env, "excal-native-set-font-family", Fexcal_native_set_font_family,
	     2,
	     "Use the Pango family list FAMILIES for font id ID.\n\n"
	     "FAMILIES nil restores the built-in list.\n\n(fn ID FAMILIES)");
	bind(env, "excal-native-font-family", Fexcal_native_font_family, 1,
	     "Return the Pango family list used for font id ID.\n\n(fn ID)");
	bind(env, "excal-native-font-resolve", Fexcal_native_font_resolve, 2,
	     "Return the comma-separated font families that show TEXT.\n\n"
	     "(fn TEXT FONT-FAMILY)");
	bind(env, "excal-native-font-backend", Fexcal_native_font_backend, 0,
	     "Return the type name of Pango's font map.\n\n(fn)");
	bind(env, "excal-native-write-png", Fexcal_native_write_png, 4,
	     "Write CANVAS pixels to FILE as PNG.\n\n"
	     "(fn CANVAS WIDTH HEIGHT FILE)");
	bind(env, "excal-native-fb-create", Fexcal_native_fb_create, 2,
	     "Return a WIDTH by HEIGHT offscreen framebuffer.\n\n"
	     "(fn WIDTH HEIGHT)");
	bind_range(env, "excal-native-fb-render", Fexcal_native_fb_render, 7, 9,
	     "Render ELEMENTS into FB, repainting only DAMAGE if non-nil.\n\n"
	     "BACKGROUND, a \"#rrggbb\" string, is the canvas color (white);\n"
	     "DARK draws every color through the dark theme filter.\n"
	     "DAMAGE is a vector [X Y WIDTH HEIGHT] in device pixels, or a\n"
	     "vector of such vectors.\n"
	     "Return the number of elements drawn.\n\n"
	     "(fn FB PIXEL-SCALE ZOOM SCROLL-X SCROLL-Y ELEMENTS DAMAGE &optional BACKGROUND DARK)");
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
	bind(env, "excal-native-seeded-random", Fexcal_native_seeded_random, 2,
	     "Return COUNT numbers from upstream seededRandom for SEED.\n\n"
	     "(fn SEED COUNT)");
	bind(env, "excal-native-fb-write-png", Fexcal_native_fb_write_png, 2,
	     "Write FB to FILE as PNG.\n\n(fn FB FILE)");
	bind(env, "excal-native-fb-pixel", Fexcal_native_fb_pixel, 3,
	     "Return FB's pixel at X, Y as an ARGB integer, or nil.\n\n"
	     "(fn FB X Y)");
	bind(env, "excal-native-image-register", Fexcal_native_image_register,
	     2,
	     "Decode DATA-URL and cache the image under FILE-ID.\n\n"
	     "Return [WIDTH HEIGHT MIME] (natural size), or nil if it cannot\n"
	     "be decoded.\n\n(fn FILE-ID DATA-URL)");
	bind(env, "excal-native-image-forget", Fexcal_native_image_forget, 1,
	     "Free the image cached under FILE-ID.\n\n(fn FILE-ID)");
	bind(env, "excal-native-image-info", Fexcal_native_image_info, 1,
	     "Return [WIDTH HEIGHT MIME] of the image cached under FILE-ID.\n\n"
	     "(fn FILE-ID)");
	bind(env, "excal-native-image-count", Fexcal_native_image_count, 0,
	     "Return the number of cached images.\n\n(fn)");
	bind(env, "excal-native-image-png", Fexcal_native_image_png, 2,
	     "Return raster image FILE-ID as PNG bytes fitting MAX-SIZE.\n\n"
	     "(fn FILE-ID MAX-SIZE)");
	bind(env, "excal-native-export-png", Fexcal_native_export_png, 11,
	     "Render ELEMENTS to the PNG FILE.\n\n"
	     "X, Y is the scene point at the top-left, WIDTH and HEIGHT the\n"
	     "size in scene units, BACKGROUND a hex color or nil, CLIP and\n"
	     "OUTLINE the frame rendering flags, SCALE pixels per scene unit,\n"
	     "TEXT nil or the bytes of an embedded-scene tEXt chunk.\n\n"
	     "(fn FILE ELEMENTS X Y WIDTH HEIGHT BACKGROUND CLIP OUTLINE SCALE "
	     "TEXT)");
	bind(env, "excal-native-export-svg", Fexcal_native_export_svg, 8,
	     "Render ELEMENTS as an SVG document string.\n\n"
	     "Arguments as for `excal-native-export-png'.\n\n"
	     "(fn ELEMENTS X Y WIDTH HEIGHT BACKGROUND CLIP OUTLINE)");
	bind(env, "excal-native-zlib-compress", Fexcal_native_zlib_compress, 1,
	     "Return STRING's bytes zlib-compressed, as a unibyte string.\n\n"
	     "(fn STRING)");
	bind(env, "excal-native-zlib-decompress",
	     Fexcal_native_zlib_decompress, 1,
	     "Return STRING's zlib data inflated, or nil if invalid.\n\n"
	     "(fn STRING)");
	bind(env, "excal-native-frame-label", Fexcal_native_frame_label, 2,
	     "Return (TEXT . WIDTH): TITLE as a frame label within MAX-WIDTH.\n\n"
	     "Sizes are screen pixels at the 14px label font.\n\n"
	     "(fn TITLE MAX-WIDTH)");
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
	bind(env, "excal-native-cursor-view-create",
	     Fexcal_native_cursor_view_create, 4,
	     "Add a hidden cursor view to the Emacs view at screen rectangle.\n\n"
	     "Arguments are as for `excal-native-layer-create'.\n\n"
	     "(fn LEFT TOP WIDTH HEIGHT)");
	bind(env, "excal-native-cursor-view-set-geometry",
	     Fexcal_native_cursor_view_set_geometry, 6,
	     "Place cursor VIEW at view rectangle X Y WIDTH HEIGHT.\n\n"
	     "(fn VIEW X Y WIDTH HEIGHT VISIBLE)");
	bind(env, "excal-native-cursor-set", Fexcal_native_cursor_set, 2,
	     "Show the CSS cursor NAME (a string) over VIEW.\n\n"
	     "Return nil if NAME is unknown.\n\n(fn VIEW NAME)");
	bind(env, "excal-native-cursor-known-p", Fexcal_native_cursor_known_p,
	     1, "Return t if NAME is a cursor the module can show.\n\n(fn NAME)");
#endif
	env->funcall(env, env->intern(env, "provide"), 1,
	             (emacs_value[]){env->intern(env, "excal-module")});
	return 0;
}
