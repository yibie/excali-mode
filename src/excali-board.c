/* SPDX-License-Identifier: GPL-3.0-or-later */
#include "excali-board.h"
#include "excali-text.h"
#include "excali-image.h"
#include <pango/pangocairo.h>
#include <math.h>
#include <string.h>

static void foreground(cairo_t *cr, double r, double g, double b)
{
        double rgb[] = {r, g, b};
        if (excali_render_dark()) excali_dark_filter(rgb);
        cairo_set_source_rgb(cr, rgb[0], rgb[1], rgb[2]);
}

static PangoLayout *layout(cairo_t *cr, const char *markup, double width,
                           bool nowrap)
{
        PangoLayout *l = pango_cairo_create_layout(cr);
        PangoFontDescription *font = pango_font_description_new();
        pango_font_description_set_family(font, excali_text_family(2));
        pango_font_description_set_absolute_size(font, 18 * PANGO_SCALE);
        pango_layout_set_font_description(l, font);
        pango_font_description_free(font);
        pango_layout_set_width(l, nowrap ? -1 : (int)(fmax(1, fmin(width, 100000)) * PANGO_SCALE));
        pango_layout_set_wrap(l, PANGO_WRAP_WORD_CHAR);
        pango_layout_set_spacing(l, 3 * PANGO_SCALE);
        /* Only our escaped Org-to-Pango serializer supplies markup.
           Invalid saved markup falls back to literal text, not execution. */
        PangoAttrList *attrs = NULL;
        char *text = NULL;
        GError *error = NULL;
        if (pango_parse_markup(markup ? markup : "", -1, 0, &attrs, &text, NULL, &error)) {
                pango_layout_set_text(l, text, -1);
                pango_layout_set_attributes(l, attrs);
                pango_attr_list_unref(attrs);
                g_free(text);
        } else {
                pango_layout_set_text(l, markup ? markup : "", -1);
                g_clear_error(&error);
        }
        return l;
}

static bool image_size(const ExcaliElement *e, size_t i, double *w, double *h)
{
        double iw, ih;
        const char *mime;
        const char *id = e->board_blocks[i].image_id;
        if (!id || !excali_image_info(id, &iw, &ih, &mime) || iw <= 0 || ih <= 0)
                return false;
        double scale = fmin(1, fmin(fmax(1, e->width - 40) / iw, 320 / ih));
        *w = iw * scale; *h = ih * scale;
        return true;
}

bool excali_board_hit(const ExcaliElement *e, double x, double y, int *block, int *index)
{
        if (!isfinite(x) || !isfinite(y) || x < 0 || y < 0 ||
            x > 1000000 || y > 1000000) return false;
        cairo_surface_t *s = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 1, 1);
        cairo_t *cr = cairo_create(s);
        double top = 0;
        bool hit = false;
        for (size_t i = 0; i < e->board_count; ++i) {
                double w, h;
                if (image_size(e, i, &w, &h)) {
                        if (y >= top && y < top + h && x < w) {
                                *block = (int)i; *index = -1; hit = true; break;
                        }
                } else {
                        PangoLayout *l = layout(cr, e->board_blocks[i].markup,
                                                e->width - 40, e->board_blocks[i].nowrap);
                        int pw, ph;
                        pango_layout_get_size(l, &pw, &ph);
                        h = (double)ph / PANGO_SCALE;
                        if (y >= top && y < top + h && x < (double)pw / PANGO_SCALE) {
                                int trailing;
                                hit = pango_layout_xy_to_index(l, (int)(x * PANGO_SCALE),
                                          (int)((y - top) * PANGO_SCALE), index, &trailing);
                                if (hit) *block = (int)i;
                                g_object_unref(l);
                                break;
                        }
                        g_object_unref(l);
                }
                top += h + 12;
        }
        cairo_destroy(cr); cairo_surface_destroy(s);
        return hit;
}

static void content(cairo_t *cr, const ExcaliElement *e, bool draw,
                    double *width, double *height)
{
        double y = 0, widest = 0;
        for (size_t i = 0; i < e->board_count; ++i) {
                double iw, ih;
                if (image_size(e, i, &iw, &ih)) {
                        widest = fmax(widest, iw);
                        if (draw && y + ih >= e->board_scroll_y &&
                            y <= e->board_scroll_y + e->height - 64) {
                                ExcaliElement image = {0};
                                image.width = iw; image.height = ih;
                                image.media.file_id = e->board_blocks[i].image_id;
                                image.media.scale[0] = image.media.scale[1] = 1;
                                cairo_save(cr);
                                cairo_translate(cr, 20 - e->board_scroll_x, 44 + y - e->board_scroll_y);
                                excali_draw_image(cr, &image);
                                cairo_restore(cr);
                        }
                        y += ih + 12;
                        continue;
                }
                PangoLayout *l = layout(cr, e->board_blocks[i].markup,
                                        e->width - 40, e->board_blocks[i].nowrap);
                int w, h;
                pango_layout_get_size(l, &w, &h);
                double bw = (double)w / PANGO_SCALE;
                double bh = (double)h / PANGO_SCALE;
                widest = fmax(widest, bw);
                if (draw && y + bh >= e->board_scroll_y &&
                    y <= e->board_scroll_y + e->height - 64) {
                        cairo_move_to(cr, 20 - e->board_scroll_x, 44 + y - e->board_scroll_y);
                        pango_cairo_show_layout(cr, l);
                }
                y += bh + 12;
                g_object_unref(l);
        }
        *width = widest;
        *height = fmax(0, y - 12);
}

void excali_board_measure(const ExcaliElement *e, double *width, double *height)
{
        cairo_surface_t *s = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 1, 1);
        cairo_t *cr = cairo_create(s);
        content(cr, e, false, width, height);
        cairo_destroy(cr);
        cairo_surface_destroy(s);
}

void excali_board_draw(cairo_t *cr, const ExcaliElement *e)
{
        if (!e->board_count || e->width < 42 || e->height < 66) return;
        cairo_save(cr);
        /* The shape renderer has already translated/rotated into card space. */
        cairo_rectangle(cr, 2, 2, e->width - 4, e->height - 4);
        cairo_clip(cr);
        foreground(cr, .46, .48, .52);
        PangoLayout *title = layout(cr, "", e->width - 40, false);
        pango_layout_set_text(title, e->board_title ? e->board_title : "Org", -1);
        PangoFontDescription *font = pango_font_description_from_string("Sans");
        pango_font_description_set_absolute_size(font, 12 * PANGO_SCALE);
        pango_layout_set_font_description(title, font);
        pango_font_description_free(font);
        pango_layout_set_height(title, -1);
        pango_layout_set_ellipsize(title, PANGO_ELLIPSIZE_END);
        cairo_move_to(cr, 20, 14);
        pango_cairo_show_layout(cr, title);
        g_object_unref(title);
        cairo_set_line_width(cr, 1);
        cairo_move_to(cr, 16, 36);
        cairo_line_to(cr, e->width - 16, 36);
        cairo_stroke(cr);
        cairo_save(cr);
        cairo_rectangle(cr, 18, 42, e->width - 36, e->height - 62);
        cairo_clip(cr);
        foreground(cr, .14, .15, .17);
        double w, h;
        content(cr, e, true, &w, &h);
        cairo_restore(cr);
        double vh = e->height - 64, vw = e->width - 40;
        cairo_set_source_rgba(cr, .45, .47, .52, .75);
        if (h > vh) {
                double thumb = fmax(18, vh * vh / h);
                double y = 44 + fmin(1, e->board_scroll_y / (h - vh)) * (vh - thumb);
                cairo_rectangle(cr, e->width - 8, y, 4, thumb);
                cairo_fill(cr);
        }
        if (w > vw) {
                double thumb = fmax(18, vw * vw / w);
                double x = 20 + fmin(1, e->board_scroll_x / (w - vw)) * (vw - thumb);
                cairo_rectangle(cr, x, e->height - 8, thumb, 4);
                cairo_fill(cr);
        }
        cairo_restore(cr);
}
