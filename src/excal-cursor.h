/* excal-cursor.h --- Excalidraw's pointer shapes over the canvas (macOS)  -*- c-file-style: "linux" -*- */
/* Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef EXCAL_CURSOR_H
#define EXCAL_CURSOR_H

#include <stdbool.h>

/* Add a cursor view to the Emacs view whose screen rectangle (top-left
   origin, points) best matches LEFT TOP WIDTH HEIGHT.  Hidden at first.  */
void *excal_cursor_view_create(double left, double top, double width,
                               double height);

/* Place VIEW at X Y WIDTH HEIGHT in Emacs view points, top-left origin.  */
void excal_cursor_view_set_geometry(void *view, double x, double y,
                                    double width, double height, bool visible);

/* Show the CSS-named cursor NAME over VIEW; false if NAME is unknown.  */
bool excal_cursor_set(void *view, const char *name);

/* Whether NAME is a cursor `excal_cursor_set' knows.  */
bool excal_cursor_known(const char *name);

void excal_cursor_view_destroy(void *view);

#endif /* EXCAL_CURSOR_H */
