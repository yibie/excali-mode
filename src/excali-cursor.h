/* excali-cursor.h --- Excalidraw's pointer shapes over the canvas (macOS)  -*- c-file-style: "linux" -*- */
/* Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef EXCALI_CURSOR_H
#define EXCALI_CURSOR_H

#include <stdbool.h>

/* Add a cursor view to the Emacs view found as by
   `excali_find_emacs_view'.  Hidden at first.  */
void *excali_cursor_view_create(double left, double top, double width,
                               double height, const char *title);

/* Place VIEW at X Y WIDTH HEIGHT in Emacs view points, top-left origin.  */
void excali_cursor_view_set_geometry(void *view, double x, double y,
                                    double width, double height, bool visible);

/* Show the CSS-named cursor NAME over VIEW; false if NAME is unknown.  */
bool excali_cursor_set(void *view, const char *name);

/* Whether NAME is a cursor `excali_cursor_set' knows.  */
bool excali_cursor_known(const char *name);

void excali_cursor_view_destroy(void *view);

#endif /* EXCALI_CURSOR_H */
