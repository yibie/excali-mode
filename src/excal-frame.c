/* excal-frame.c --- Frame outlines, names and clipping  -*- c-file-style: "linux" -*-
 *
 * Ports the frame parts of upstream renderElement.ts (the "frame" and
 * "magicframe" case), staticScene.ts (`frameClip') and App.tsx
 * (`renderFrameNames', a DOM label there, drawn on the canvas here).
 */

#include "excal-frame.h"
#include "excal-text.h"

#include <math.h>
#include <stdlib.h>
#include <string.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

/* FRAME_STYLE (common/src/constants.ts).  */
#define FRAME_STROKE_WIDTH 2.0
#define FRAME_RADIUS 8.0
#define FRAME_NAME_OFFSET_Y 3.0
#define FRAME_NAME_FONT_SIZE 14.0
#define FRAME_NAME_LINE_HEIGHT 1.25

static ExcalFrameConfig config = {1.0, true, true, true};
static const ExcalElement **frames;
static size_t frame_count;

void excal_frame_begin_pass(const ExcalElement *elements, size_t count,
                            const ExcalFrameConfig *cfg)
{
	config = *cfg;
	if (!(config.zoom > 0))
		config.zoom = 1;
	free(frames);
	frames = NULL;
	frame_count = 0;
	for (size_t i = 0; i < count; ++i)
		if (elements[i].type == EXCAL_FRAME && elements[i].media.id)
			++frame_count;
	if (frame_count == 0)
		return;
	frames = malloc(frame_count * sizeof *frames);
	size_t n = 0;
	for (size_t i = 0; i < count && frames; ++i)
		if (elements[i].type == EXCAL_FRAME && elements[i].media.id)
			frames[n++] = &elements[i];
	frame_count = frames ? n : 0;
}

void excal_frame_end_pass(void)
{
	free(frames);
	frames = NULL;
	frame_count = 0;
	config = (ExcalFrameConfig){1.0, true, true, true};
}

static const ExcalElement *find_frame(const char *id)
{
	if (!id)
		return NULL;
	for (size_t i = 0; i < frame_count; ++i)
		if (strcmp(frames[i]->media.id, id) == 0)
			return frames[i];
	return NULL;
}

/* Axis-aligned scene bounds of E including its rotation.  */
static void rotated_bounds(const ExcalElement *e, double b[4])
{
	double x1, y1, x2, y2;
	if (e->point_count > 0) {
		x1 = x2 = e->x + e->points[0];
		y1 = y2 = e->y + e->points[1];
		for (size_t i = 1; i < e->point_count; ++i) {
			x1 = fmin(x1, e->x + e->points[2 * i]);
			y1 = fmin(y1, e->y + e->points[2 * i + 1]);
			x2 = fmax(x2, e->x + e->points[2 * i]);
			y2 = fmax(y2, e->y + e->points[2 * i + 1]);
		}
	} else {
		x1 = fmin(e->x, e->x + e->width);
		y1 = fmin(e->y, e->y + e->height);
		x2 = fmax(e->x, e->x + e->width);
		y2 = fmax(e->y, e->y + e->height);
	}
	if (e->angle == 0) {
		b[0] = x1, b[1] = y1, b[2] = x2, b[3] = y2;
		return;
	}
	/* `draw_element' rotates about the centre of the box.  */
	double cx = e->x + e->width / 2, cy = e->y + e->height / 2;
	double c = cos(e->angle), s = sin(e->angle);
	double xs[4] = {x1, x2, x2, x1}, ys[4] = {y1, y1, y2, y2};
	b[0] = b[1] = INFINITY;
	b[2] = b[3] = -INFINITY;
	for (int i = 0; i < 4; ++i) {
		double dx = xs[i] - cx, dy = ys[i] - cy;
		double rx = cx + dx * c - dy * s, ry = cy + dx * s + dy * c;
		b[0] = fmin(b[0], rx), b[1] = fmin(b[1], ry);
		b[2] = fmax(b[2], rx), b[3] = fmax(b[3], ry);
	}
}

/* Canvas roundRect(X, Y, W, H, R) as a closed path.  */
static void round_rect(cairo_t *cr, double x, double y, double w, double h,
                       double r)
{
	if (w < 0)
		x += w, w = -w;
	if (h < 0)
		y += h, h = -h;
	r = fmax(0, fmin(r, fmin(w, h) / 2));
	cairo_new_sub_path(cr);
	cairo_arc(cr, x + w - r, y + r, r, -M_PI / 2, 0);
	cairo_arc(cr, x + w - r, y + h - r, r, 0, M_PI / 2);
	cairo_arc(cr, x + r, y + h - r, r, M_PI / 2, M_PI);
	cairo_arc(cr, x + r, y + r, r, M_PI, 3 * M_PI / 2);
	cairo_close_path(cr);
}

const ExcalElement *excal_frame_clip_begin(cairo_t *cr, const ExcalElement *e,
                                           ExcalElement *scratch)
{
	cairo_save(cr);
	const ExcalElement *frame = find_frame(e->media.frame_id);
	if (!frame || frame == e)
		return e;
	if (config.clip) {
		/* `shouldApplyFrameClip': clip unless the element lies
		   wholly outside the frame and is not grouped (a group
		   member outside stays clipped with its group).  Clipping an
		   element wholly inside changes nothing.  */
		double b[4];
		rotated_bounds(e, b);
		double fx1 = fmin(frame->x, frame->x + frame->width);
		double fy1 = fmin(frame->y, frame->y + frame->height);
		double fx2 = fmax(frame->x, frame->x + frame->width);
		double fy2 = fmax(frame->y, frame->y + frame->height);
		bool outside = b[2] < fx1 || b[0] > fx2 || b[3] < fy1 ||
		               b[1] > fy2;
		if (!outside || e->media.grouped) {
			cairo_new_path(cr);
			round_rect(cr, frame->x, frame->y, frame->width,
			           frame->height, FRAME_RADIUS / config.zoom);
			cairo_clip(cr);
		}
	}
	/* `resolveElementRenderState': frame and element opacity multiply.  */
	double frame_opacity = fmax(0, fmin(frame->opacity, 100));
	if (frame_opacity < 100) {
		*scratch = *e;
		scratch->opacity =
		        frame_opacity * fmax(0, fmin(e->opacity, 100)) / 100;
		return scratch;
	}
	return e;
}

void excal_frame_clip_end(cairo_t *cr)
{
	cairo_restore(cr);
}

/* Length of the UTF-8 character starting with byte C.  */
static size_t utf8_length(unsigned char c)
{
	return c < 0x80 ? 1 : c < 0xe0 ? 2 : c < 0xf0 ? 3 : 4;
}

char *excal_frame_label_text(const char *title, double max_width,
                             double *width)
{
	size_t len = strlen(title);
	double full = excal_text_line_width(title, FRAME_NAME_FONT_SIZE,
	                                    EXCAL_FONT_ASSISTANT);
	if (full <= max_width) {
		*width = full;
		return excal_strdup(title);
	}
	/* CSS text-overflow: ellipsis keeps the longest prefix that fits
	   together with "…".  */
	size_t *cuts = malloc((len + 1) * sizeof *cuts);
	size_t n = 0;
	for (size_t i = 0; i < len; i += utf8_length((unsigned char)title[i]))
		cuts[n++] = i;
	char *buf = malloc(len + 4);
	*width = 0;
	for (size_t k = n; k-- > 0;) {
		memcpy(buf, title, cuts[k]);
		memcpy(buf + cuts[k], "\xe2\x80\xa6", 4);
		double w = excal_text_line_width(buf, FRAME_NAME_FONT_SIZE,
		                                 EXCAL_FONT_ASSISTANT);
		if (w <= max_width || k == 0) {
			*width = fmin(w, max_width);
			break;
		}
	}
	free(cuts);
	return buf;
}

void excal_draw_frame(cairo_t *cr, const ExcalElement *e)
{
	double zoom = config.zoom;
	if (config.outline) {
		cairo_save(cr);
		cairo_new_path(cr);
		cairo_set_dash(cr, NULL, 0, 0);
		cairo_set_line_width(cr, FRAME_STROKE_WIDTH / zoom);
		double rgb[3] = {0xbb / 255.0, 0xbb / 255.0, 0xbb / 255.0};
		if (e->media.magic) {
			rgb[0] = 0x7a / 255.0;
			rgb[1] = 0xff / 255.0;
			rgb[2] = 0xd7 / 255.0;
		}
		if (excal_render_dark())
			excal_dark_filter(rgb);
		cairo_set_source_rgb(cr, rgb[0], rgb[1], rgb[2]);
		round_rect(cr, e->x, e->y, e->width, e->height,
		           FRAME_RADIUS / zoom);
		cairo_stroke(cr);
		cairo_restore(cr);
	}
	if (!config.names || !e->media.name || !*e->media.name)
		return;
	/* The DOM label: 14px Assistant, line height 1.25, its bottom
	   3px above the frame, as wide as the frame at most.  All sizes
	   are screen pixels.  */
	double label_width;
	char *text = excal_frame_label_text(
	        e->media.name, fabs(e->width) * zoom, &label_width);
	double font_px = FRAME_NAME_FONT_SIZE / zoom;
	double line_px = font_px * FRAME_NAME_LINE_HEIGHT;
	/* getVerticalOffset with Assistant's metrics (unitsPerEm 2048,
	   ascender 1021, descender -287).  */
	double em = font_px / 2048;
	ExcalElement label = {
	        .type = EXCAL_TEXT,
	        .x = fmin(e->x, e->x + e->width),
	        .y = fmin(e->y, e->y + e->height) -
	             (line_px + FRAME_NAME_OFFSET_Y / zoom),
	        .width = label_width / zoom,
	        .height = line_px,
	        .text = text,
	        .font_size = font_px,
	        .font_family = EXCAL_FONT_ASSISTANT,
	        .line_height = FRAME_NAME_LINE_HEIGHT,
	        .opacity = 100,
	        .has_text_offset = true,
	        .text_offset = em * 1021 + (line_px - em * 1021 + em * -287) / 2,
	};
	cairo_save(cr);
	cairo_rectangle(cr, label.x, label.y, fabs(e->width), line_px);
	cairo_clip(cr);
	/* nameColorLightTheme / nameColorDarkTheme, not filtered.  */
	double name = (excal_render_dark() ? 0x7a : 0x99) / 255.0;
	excal_draw_text(cr, &label, name, name, name, 1);
	cairo_restore(cr);
	free(text);
}
