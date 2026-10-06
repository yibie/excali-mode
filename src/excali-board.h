/* Native clipped rich-text panels, used only by excali-board-mode. */
#ifndef EXCALI_BOARD_H
#define EXCALI_BOARD_H
#include <cairo.h>
#include "excali-render.h"
void excali_board_measure(const ExcaliElement *e, double *width, double *height);
bool excali_board_hit(const ExcaliElement *e, double x, double y, int *block, int *index);
void excali_board_draw(cairo_t *cr, const ExcaliElement *e);
#endif
