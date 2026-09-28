/* excal-text.c --- Text layout, measurement and drawing  -*- c-file-style: "linux" -*- */

/* Layout decisions (wrapping, line positions) follow Excalidraw and are
   made in Elisp (excal-text.el); Pango/HarfBuzz only shape and measure
   single lines, and draw them at the baselines Excalidraw uses.  */

#include "excal-text.h"

#include <dirent.h>
#include <math.h>
#include <pango/pangocairo.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/stat.h>

#ifdef EXCAL_HAVE_FONTCONFIG
#include <fontconfig/fontconfig.h>
#include <pango/pangofc-fontmap.h>
#endif
#ifdef EXCAL_HAVE_CORETEXT
#include <CoreText/CoreText.h>
#endif
#ifdef _WIN32
#include <windows.h>
#endif

/* Upstream's getFontFamilyString gives "<family>, <fallbacks>", with
   Xiaolai only after Excalifont, then the generic family and Segoe UI
   Emoji.  Alternate names of the same fonts follow each family so that
   system installs are found too.  The generic families are spelled out
   as concrete fonts: Pango's CoreText backend does not resolve
   "sans-serif", and would take digits and spaces from the first family
   it finds, the emoji font.  CJK system fonts come before emoji ones.  */
#define SANS                                                              \
	"Helvetica, Arial, Liberation Sans, DejaVu Sans, Noto Sans, "     \
	"sans-serif, "
#define MONO                                                              \
	"Menlo, Consolas, DejaVu Sans Mono, Noto Sans Mono, Liberation Mono, " \
	"monospace, "
#define CJK                                                               \
	"PingFang SC, Hiragino Sans GB, Noto Sans CJK SC, Source Han Sans SC, " \
	"Microsoft YaHei, "
#define EMOJI "Segoe UI Emoji, Apple Color Emoji, Noto Color Emoji"
static const struct {
	int id;
	const char *families;
} default_families[] = {
        {EXCAL_FONT_VIRGIL, "Virgil, Virgil 3 YOFF, " SANS CJK EMOJI},
        {EXCAL_FONT_HELVETICA, SANS CJK EMOJI},
        {EXCAL_FONT_CASCADIA, "Cascadia Code, Cascadia, " MONO CJK EMOJI},
        {EXCAL_FONT_EXCALIFONT,
         "Excalifont, Xiaolai, Xiaolai SC, " SANS CJK EMOJI},
        {EXCAL_FONT_NUNITO, "Nunito, " SANS CJK EMOJI},
        {EXCAL_FONT_LILITA_ONE, "Lilita One, " SANS CJK EMOJI},
        {EXCAL_FONT_COMIC_SHANNS,
         "Comic Shanns, Comic Shanns Mono, " MONO CJK EMOJI},
        {EXCAL_FONT_LIBERATION_SANS,
         "Liberation Sans, Arial, Helvetica, " SANS CJK EMOJI},
        {EXCAL_FONT_ASSISTANT, "Assistant, " SANS CJK EMOJI},
        {EXCAL_FONT_XIAOLAI, "Xiaolai, Xiaolai SC, " SANS CJK EMOJI},
        {EXCAL_FONT_SANS_SERIF, SANS CJK EMOJI},
        {EXCAL_FONT_MONOSPACE, MONO CJK EMOJI},
        {EXCAL_FONT_SEGOE_UI_EMOJI, EMOJI},
};
#define FAMILY_COUNT (sizeof default_families / sizeof default_families[0])

/* Unknown ids render as just "Segoe UI Emoji" upstream (utils.ts
   getFontFamilyString), with Excalifont metrics.  */
static const char *unknown_family = EMOJI ", " SANS CJK "sans-serif";

static char *family_overrides[FAMILY_COUNT];

static int family_index(int id)
{
	for (size_t i = 0; i < FAMILY_COUNT; ++i)
		if (default_families[i].id == id)
			return (int)i;
	return -1;
}

void excal_text_set_family(int id, const char *families)
{
	int i = family_index(id);
	if (i < 0)
		return;
	free(family_overrides[i]);
	family_overrides[i] = families ? excal_strdup(families) : NULL;
}

const char *excal_text_family(int id)
{
	int i = family_index(id);
	if (i < 0)
		return unknown_family;
	return family_overrides[i] ? family_overrides[i]
	                           : default_families[i].families;
}

/* Font registration.  */

static bool font_file_p(const char *name)
{
	const char *dot = strrchr(name, '.');
	if (!dot)
		return false;
	static const char *exts[] = {".ttf", ".otf", ".ttc", ".otc",
	                             ".woff", ".woff2"};
	for (size_t i = 0; i < sizeof exts / sizeof exts[0]; ++i)
		if (strcasecmp(dot, exts[i]) == 0)
			return true;
	return false;
}

/* Register PATH with every backend available.  Return whether the
   backend Pango actually uses accepted it: fontconfig when the font map
   is a fontconfig one, else CoreText or GDI.  */
static bool register_font_file(const char *path)
{
	bool fc = false, native = false;
#ifdef EXCAL_HAVE_FONTCONFIG
	PangoFontMap *map = pango_cairo_font_map_get_default();
	bool uses_fc = PANGO_IS_FC_FONT_MAP(map);
	if (uses_fc) {
		FcConfig *config =
		        pango_fc_font_map_get_config(PANGO_FC_FONT_MAP(map));
		if (!config)
			config = FcConfigGetCurrent();
		fc = FcConfigAppFontAddFile(config, (const FcChar8 *)path);
	}
#else
	bool uses_fc = false;
#endif
#ifdef EXCAL_HAVE_CORETEXT
	/* Also makes the font available to the rest of the process.  */
	CFURLRef url = CFURLCreateFromFileSystemRepresentation(
	        NULL, (const UInt8 *)path, (CFIndex)strlen(path), false);
	if (url) {
		CFErrorRef error = NULL;
		if (CTFontManagerRegisterFontsForURL(
		            url, kCTFontManagerScopeProcess, &error))
			native = true;
		else if (error)
			CFRelease(error);
		CFRelease(url);
	}
#endif
#ifdef _WIN32
	native = AddFontResourceExA(path, FR_PRIVATE, 0) > 0;
#endif
	return uses_fc ? fc : native;
}

static int add_fonts_in(const char *path, int depth)
{
	struct stat st;
	if (stat(path, &st) != 0)
		return -1;
	if (!S_ISDIR(st.st_mode))
		return font_file_p(path) && register_font_file(path) ? 1 : 0;
	if (depth > 4)
		return 0;
	DIR *dir = opendir(path);
	if (!dir)
		return -1;
	int count = 0;
	struct dirent *entry;
	while ((entry = readdir(dir))) {
		if (entry->d_name[0] == '.')
			continue;
		size_t len = strlen(path) + strlen(entry->d_name) + 2;
		char *child = malloc(len);
		if (!child)
			break;
		snprintf(child, len, "%s/%s", path, entry->d_name);
		int n = add_fonts_in(child, depth + 1);
		if (n > 0)
			count += n;
		free(child);
	}
	closedir(dir);
	return count;
}

static PangoContext *measure_context_cache;

int excal_text_add_fonts(const char *path)
{
	int count = add_fonts_in(path, 0);
	if (count > 0) {
		/* Font maps cache the font list (CoreText) or the config
		   (fontconfig): start over with a fresh default map.  */
		pango_cairo_font_map_set_default(NULL);
		if (measure_context_cache) {
			g_object_unref(measure_context_cache);
			measure_context_cache = NULL;
		}
	}
	return count;
}

const char *excal_text_backend(void)
{
	return G_OBJECT_TYPE_NAME(pango_cairo_font_map_get_default());
}

/* Layouts.  */

/* Metric hinting and glyph position rounding off: advances then scale
   linearly with zoom, and measurements match the rendered text.  */
static void configure_context(PangoContext *context)
{
	cairo_font_options_t *options = cairo_font_options_create();
	cairo_font_options_set_hint_metrics(options, CAIRO_HINT_METRICS_OFF);
	cairo_font_options_set_hint_style(options, CAIRO_HINT_STYLE_NONE);
	pango_cairo_context_set_font_options(context, options);
	cairo_font_options_destroy(options);
	pango_context_set_round_glyph_positions(context, FALSE);
}

static void set_font(PangoLayout *layout, double font_size, int family)
{
	PangoFontDescription *desc =
	        pango_font_description_from_string(excal_text_family(family));
	pango_font_description_set_absolute_size(desc, font_size * PANGO_SCALE);
	pango_layout_set_font_description(layout, desc);
	pango_font_description_free(desc);
}

static PangoContext *measure_context(void)
{
	if (!measure_context_cache) {
		measure_context_cache = pango_font_map_create_context(
		        pango_cairo_font_map_get_default());
		configure_context(measure_context_cache);
	}
	return measure_context_cache;
}

static PangoLayout *measure_layout(const char *text, int length,
                                   double font_size, int family)
{
	PangoLayout *layout = pango_layout_new(measure_context());
	set_font(layout, font_size, family);
	pango_layout_set_single_paragraph_mode(layout, TRUE);
	pango_layout_set_text(layout, text, length);
	return layout;
}

static double layout_width(PangoLayout *layout)
{
	PangoRectangle logical;
	pango_layout_get_extents(layout, NULL, &logical);
	return (double)logical.width / PANGO_SCALE;
}

static double line_width(const char *line, int length, double font_size,
                         int family)
{
	if (length == 0)
		return 0;
	PangoLayout *layout = measure_layout(line, length, font_size, family);
	double width = layout_width(layout);
	g_object_unref(layout);
	return width;
}

double excal_text_line_width(const char *line, double font_size,
                             int font_family)
{
	return line_width(line, (int)strlen(line), font_size, font_family);
}

void excal_measure_text(const char *text, double font_size, int font_family,
                        double line_height, double *width, double *height)
{
	/* Empty lines count as " " upstream; that only matters for the
	   empty text, whose width is that of a space.  */
	int lines = 0;
	double widest = 0;
	const char *p = text;
	for (;;) {
		const char *end = strchr(p, '\n');
		int length = end ? (int)(end - p) : (int)strlen(p);
		double w = length ? line_width(p, length, font_size, font_family)
		                  : line_width(" ", 1, font_size, font_family);
		if (w > widest)
			widest = w;
		++lines;
		if (!end)
			break;
		p = end + 1;
	}
	*width = widest;
	*height = lines * font_size * line_height;
}

char *excal_text_resolve(const char *text, int font_family)
{
	PangoLayout *layout =
	        measure_layout(text, (int)strlen(text), 20, font_family);
	GString *names = g_string_new(NULL);
	PangoLayoutIter *iter = pango_layout_get_iter(layout);
	do {
		PangoLayoutRun *run = pango_layout_iter_get_run_readonly(iter);
		if (!run)
			continue;
		PangoFontDescription *desc =
		        pango_font_describe(run->item->analysis.font);
		const char *family = pango_font_description_get_family(desc);
		if (family) {
			/* Skip families already listed.  */
			bool seen = false;
			const char *s = names->str;
			size_t n = strlen(family);
			while (s && *s && !seen) {
				const char *comma = strchr(s, ',');
				size_t len = comma ? (size_t)(comma - s)
				                   : strlen(s);
				seen = len == n && strncmp(s, family, n) == 0;
				s = comma ? comma + 1 : NULL;
			}
			if (!seen) {
				if (names->len)
					g_string_append_c(names, ',');
				g_string_append(names, family);
			}
		}
		pango_font_description_free(desc);
	} while (pango_layout_iter_next_run(iter));
	pango_layout_iter_free(iter);
	g_object_unref(layout);
	char *result = excal_strdup(names->str);
	g_string_free(names, TRUE);
	return result;
}

/* Drawing.  */

/* getVerticalOffset with Excalifont metrics, for elements that carry no
   offset from Elisp.  */
static double default_vertical_offset(double font_size, double line_px)
{
	double em = font_size / 1000.0;
	return em * 886 + (line_px - em * 886 + em * -374) / 2;
}

void excal_draw_text(cairo_t *cr, const ExcalElement *e, double red,
                     double green, double blue, double alpha)
{
	if (!e->text || !*e->text)
		return;
	double line_px = e->font_size * e->line_height;
	double offset = e->has_text_offset
	                        ? e->text_offset
	                        : default_vertical_offset(e->font_size, line_px);
	int align = 0; /* left */
	if (e->text_align && strcmp(e->text_align, "center") == 0)
		align = 1;
	else if (e->text_align && strcmp(e->text_align, "right") == 0)
		align = 2;

	PangoLayout *layout = pango_cairo_create_layout(cr);
	configure_context(pango_layout_get_context(layout));
	pango_layout_context_changed(layout);
	set_font(layout, e->font_size, e->font_family);
	pango_layout_set_single_paragraph_mode(layout, TRUE);
	cairo_set_source_rgba(cr, red, green, blue, alpha);

	const char *p = e->text;
	for (int i = 0;; ++i) {
		const char *end = strchr(p, '\n');
		int length = end ? (int)(end - p) : (int)strlen(p);
		/* Upstream splits on \r\n and \r too.  */
		if (length > 0 && p[length - 1] == '\r')
			--length;
		if (length > 0) {
			pango_layout_set_text(layout, p, length);
			double w = layout_width(layout);
			double x = e->x + (align == 1   ? e->width / 2 - w / 2
			                   : align == 2 ? e->width - w
			                                : 0);
			cairo_move_to(cr, x, e->y + i * line_px + offset);
			pango_cairo_show_layout_line(
			        cr, pango_layout_get_line_readonly(layout, 0));
		}
		if (!end)
			break;
		p = end + 1;
	}
	g_object_unref(layout);
}

void excal_text_clip_label_hole(cairo_t *cr, const ExcalElement *e)
{
	if (!e->has_label_hole)
		return;
	double x1, y1, x2, y2;
	cairo_clip_extents(cr, &x1, &y1, &x2, &y2);
	const double *h = e->label_hole;
	x1 = fmin(x1, h[0]) - 1;
	y1 = fmin(y1, h[1]) - 1;
	x2 = fmax(x2, h[0] + h[2]) + 1;
	y2 = fmax(y2, h[1] + h[3]) + 1;
	cairo_new_path(cr);
	cairo_rectangle(cr, x1, y1, x2 - x1, y2 - y1);
	cairo_rectangle(cr, h[0], h[1], h[2], h[3]);
	cairo_fill_rule_t rule = cairo_get_fill_rule(cr);
	cairo_set_fill_rule(cr, CAIRO_FILL_RULE_EVEN_ODD);
	cairo_clip(cr);
	cairo_set_fill_rule(cr, rule);
}
