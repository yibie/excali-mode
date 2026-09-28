/* excal-text.c --- Text layout, measurement and drawing  -*- c-file-style: "linux" -*- */

#include "excal-text.h"

#include <pango/pangocairo.h>
#include <string.h>


static const char *font_family_name(int family)
{
	switch (family) {
	case 1:
		return "Virgil, Xiaolai, PingFang SC";
	case 2:
		return "Helvetica, PingFang SC";
	case 3:
		return "Cascadia Code, Menlo, PingFang SC";
	case 6:
		return "Nunito, PingFang SC";
	case 7:
		return "Lilita One, PingFang SC";
	case 8:
		return "Comic Shanns, PingFang SC";
	default:
		return "Excalifont, Xiaolai, Virgil, PingFang SC";
	}
}

static PangoLayout *text_layout(cairo_t *cr, const char *text,
                                double font_size, int family,
                                double line_height)
{
	PangoLayout *layout = pango_cairo_create_layout(cr);
	/* Layout must not depend on zoom, so disable metric hinting.  */
	cairo_font_options_t *options = cairo_font_options_create();
	cairo_font_options_set_hint_metrics(options, CAIRO_HINT_METRICS_OFF);
	cairo_font_options_set_hint_style(options, CAIRO_HINT_STYLE_NONE);
	pango_cairo_context_set_font_options(pango_layout_get_context(layout),
	                                     options);
	cairo_font_options_destroy(options);
	pango_layout_context_changed(layout);

	PangoFontDescription *desc =
	        pango_font_description_from_string(font_family_name(family));
	pango_font_description_set_absolute_size(desc, font_size * PANGO_SCALE);
	pango_layout_set_font_description(layout, desc);
	pango_font_description_free(desc);

	PangoAttrList *attrs = pango_attr_list_new();
	pango_attr_list_insert(attrs, pango_attr_line_height_new_absolute((
	                                      int)(font_size * line_height *
	                                           PANGO_SCALE)));
	pango_layout_set_attributes(layout, attrs);
	pango_attr_list_unref(attrs);
	pango_layout_set_text(layout, text, -1);
	return layout;
}

void excal_measure_text(const char *text, double font_size, int font_family,
                        double line_height, double *width, double *height)
{
	cairo_surface_t *surface =
	        cairo_image_surface_create(CAIRO_FORMAT_ARGB32, 1, 1);
	cairo_t *cr = cairo_create(surface);
	PangoLayout *layout =
	        text_layout(cr, text, font_size, font_family, line_height);
	PangoRectangle logical;
	pango_layout_get_extents(layout, NULL, &logical);
	*width = (double)logical.width / PANGO_SCALE;
	*height = (double)logical.height / PANGO_SCALE;
	g_object_unref(layout);
	cairo_destroy(cr);
	cairo_surface_destroy(surface);
}

void excal_draw_text(cairo_t *cr, const ExcalElement *e, double red,
                     double green, double blue, double alpha)
{
	if (!e->text || !*e->text)
		return;
	PangoLayout *layout = text_layout(cr, e->text, e->font_size,
	                                  e->font_family, e->line_height);
	if (e->text_align && strcmp(e->text_align, "left") != 0) {
		pango_layout_set_width(layout, (int)(e->width * PANGO_SCALE));
		pango_layout_set_alignment(layout,
		                           strcmp(e->text_align, "center") == 0
		                                   ? PANGO_ALIGN_CENTER
		                                   : PANGO_ALIGN_RIGHT);
	}
	cairo_set_source_rgba(cr, red, green, blue, alpha);
	cairo_move_to(cr, e->x, e->y);
	pango_cairo_show_layout(cr, layout);
	g_object_unref(layout);
}
