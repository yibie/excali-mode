/* excal-cursor.m --- Excalidraw's pointer shapes over the canvas (macOS)  -*- c-file-style: "linux" -*-
 * Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Emacs' `pointer' property knows only a handful of shapes, and NS Emacs
 * shows its pointer through a cursor rectangle covering the whole view,
 * rebuilt only when the pointer type changes.  This adds a transparent
 * subview over the canvas that owns the cursor there: its own cursor
 * rectangle and a cursor-update tracking area reassert the shape Elisp
 * chose, and `excal_cursor_set' sets it at once while the mouse is over
 * the canvas.  The view ignores hit testing, so events still reach Emacs.
 *
 * Shapes are named like CSS cursors, as upstream sets them.
 */

#import <AppKit/AppKit.h>
#include <math.h>
#include <string.h>

#include "excal-cursor.h"
#include "excal-layer.h"

/* Drawing helpers for the shapes AppKit lacks.  Arrows are black on a
   white halo, like the system's resize cursors.  */

static void stroke_arrow_path(NSBezierPath *path)
{
	[[NSColor whiteColor] setStroke];
	path.lineWidth = 4;
	path.lineCapStyle = NSLineCapStyleRound;
	path.lineJoinStyle = NSLineJoinStyleRound;
	[path stroke];
	[[NSColor blackColor] setStroke];
	path.lineWidth = 1.5;
	[path stroke];
}

/* A double-headed arrow through the center of a SIZE square at ANGLE
   radians (0 = horizontal), heads LENGTH from the center.  */
static void add_double_arrow(NSBezierPath *path, double size, double angle,
                             double length)
{
	double c = size / 2, dx = cos(angle), dy = sin(angle);
	double head = 3.5;
	for (int sign = -1; sign <= 1; sign += 2) {
		double tx = c + sign * dx * length, ty = c + sign * dy * length;
		[path moveToPoint:NSMakePoint(c, c)];
		[path lineToPoint:NSMakePoint(tx, ty)];
		/* Head: two barbs back from the tip.  */
		for (int side = -1; side <= 1; side += 2) {
			double bx = tx - sign * dx * head - side * dy * head;
			double by = ty - sign * dy * head + side * dx * head;
			[path moveToPoint:NSMakePoint(tx, ty)];
			[path lineToPoint:NSMakePoint(bx, by)];
		}
	}
}

static NSCursor *arrow_cursor(NSArray<NSNumber *> *angles)
{
	const double size = 24;
	NSImage *image = [NSImage
	        imageWithSize:NSMakeSize(size, size)
	              flipped:YES
	       drawingHandler:^BOOL(NSRect rect) {
		       (void)rect;
		       NSBezierPath *path = [NSBezierPath bezierPath];
		       for (NSNumber *angle in angles)
			       add_double_arrow(path, size, angle.doubleValue,
			                        size / 2 - 3);
		       stroke_arrow_path(path);
		       return YES;
	       }];
	return [[NSCursor alloc] initWithImage:image
	                               hotSpot:NSMakePoint(size / 2, size / 2)];
}

/* Upstream `setEraserCursor': a radius-5 circle in a 20px square, white
   with a black outline, or the reverse in the dark theme.  */
static NSCursor *eraser_cursor(bool dark)
{
	const double size = 20;
	NSImage *image = [NSImage
	        imageWithSize:NSMakeSize(size, size)
	              flipped:YES
	       drawingHandler:^BOOL(NSRect rect) {
		       (void)rect;
		       NSBezierPath *circle = [NSBezierPath
		               bezierPathWithOvalInRect:NSMakeRect(size / 2 - 5,
		                                                   size / 2 - 5,
		                                                   10, 10)];
		       [(dark ? [NSColor blackColor] : [NSColor whiteColor])
		               setFill];
		       [circle fill];
		       [(dark ? [NSColor whiteColor] : [NSColor blackColor])
		               setStroke];
		       circle.lineWidth = 1;
		       [circle stroke];
		       return YES;
	       }];
	return [[NSCursor alloc] initWithImage:image
	                               hotSpot:NSMakePoint(size / 2, size / 2)];
}

static NSCursor *resize_cursor(const char *name)
{
#if defined(MAC_OS_VERSION_15_0)
	if (@available(macOS 15.0, *)) {
		NSCursorFrameResizePosition position;
		if (!strcmp(name, "ns-resize"))
			position = NSCursorFrameResizePositionTop;
		else if (!strcmp(name, "ew-resize"))
			position = NSCursorFrameResizePositionRight;
		else if (!strcmp(name, "nwse-resize"))
			position = NSCursorFrameResizePositionTopLeft;
		else
			position = NSCursorFrameResizePositionTopRight;
		return [NSCursor
		        frameResizeCursorFromPosition:position
		                         inDirections:
		                                 NSCursorFrameResizeDirectionsAll];
	}
#endif
	/* Angles in the flipped image: y grows downwards.  */
	double angle = !strcmp(name, "ns-resize")     ? M_PI / 2
	               : !strcmp(name, "ew-resize")   ? 0
	               : !strcmp(name, "nwse-resize") ? M_PI / 4
	                                              : -M_PI / 4;
	return arrow_cursor(@[ @(angle) ]);
}

/* Return the cursor called NAME, or nil.  Custom ones are cached.  */
static NSCursor *cursor_named(const char *name)
{
	static NSMutableDictionary<NSString *, NSCursor *> *cache;
	if (!strcmp(name, "default") || !strcmp(name, "auto"))
		return [NSCursor arrowCursor];
	if (!strcmp(name, "pointer"))
		return [NSCursor pointingHandCursor];
	if (!strcmp(name, "text"))
		return [NSCursor IBeamCursor];
	if (!strcmp(name, "crosshair"))
		return [NSCursor crosshairCursor];
	if (!strcmp(name, "grab"))
		return [NSCursor openHandCursor];
	if (!strcmp(name, "grabbing"))
		return [NSCursor closedHandCursor];
	if (!strcmp(name, "not-allowed"))
		return [NSCursor operationNotAllowedCursor];
	NSString *key = [NSString stringWithUTF8String:name];
	if (!cache)
		cache = [NSMutableDictionary dictionary];
	NSCursor *cursor = cache[key];
	if (cursor)
		return cursor;
	if (!strcmp(name, "move"))
		cursor = arrow_cursor(@[ @0, @(M_PI / 2) ]);
	else if (!strcmp(name, "ns-resize") || !strcmp(name, "ew-resize") ||
	         !strcmp(name, "nwse-resize") || !strcmp(name, "nesw-resize"))
		cursor = resize_cursor(name);
	else if (!strcmp(name, "eraser"))
		cursor = eraser_cursor(false);
	else if (!strcmp(name, "eraser-dark"))
		cursor = eraser_cursor(true);
	if (cursor)
		cache[key] = cursor;
	return cursor;
}

@interface ExcalCursorView : NSView
@property(nonatomic, strong) NSCursor *cursor;
@end

@implementation ExcalCursorView

- (instancetype)initWithFrame:(NSRect)frame
{
	self = [super initWithFrame:frame];
	if (self) {
		self.cursor = [NSCursor arrowCursor];
		[self addTrackingArea:
		              [[NSTrackingArea alloc]
		                      initWithRect:NSZeroRect
		                           options:NSTrackingCursorUpdate |
		                                   NSTrackingMouseEnteredAndExited |
		                                   NSTrackingActiveAlways |
		                                   NSTrackingInVisibleRect
		                             owner:self
		                          userInfo:nil]];
	}
	return self;
}

/* Emacs' view is flipped; match it so frames need no conversion.  */
- (BOOL)isFlipped
{
	return YES;
}

/* Transparent to clicks: Emacs keeps receiving every mouse event.  */
- (NSView *)hitTest:(NSPoint)point
{
	(void)point;
	return nil;
}

- (void)resetCursorRects
{
	if (self.cursor && !NSIsEmptyRect(self.visibleRect))
		[self addCursorRect:self.visibleRect cursor:self.cursor];
}

- (void)cursorUpdate:(NSEvent *)event
{
	(void)event;
	[self.cursor set];
}

- (void)mouseEntered:(NSEvent *)event
{
	(void)event;
	[self.cursor set];
}

/* Leaving the canvas: hand the pointer back to Emacs.  Its own cursor
   rectangle covers the canvas too, so it would not notice.  */
- (void)mouseExited:(NSEvent *)event
{
	(void)event;
	NSView *emacs = self.superview;
	[[NSCursor arrowCursor] set];
	if (emacs)
		[self.window invalidateCursorRectsForView:emacs];
}

- (BOOL)mouseInside
{
	if (self.hidden || !self.window)
		return NO;
	NSPoint p = [self
	        convertPoint:[self.window mouseLocationOutsideOfEventStream]
	            fromView:nil];
	return NSPointInRect(p, self.bounds);
}
@end

void *excal_cursor_view_create(double left, double top, double width,
                               double height)
{
	NSView *emacs = (__bridge NSView *)excal_find_emacs_view(left, top,
	                                                         width, height);
	if (!emacs)
		return NULL;
	ExcalCursorView *view =
	        [[ExcalCursorView alloc] initWithFrame:NSMakeRect(0, 0, 1, 1)];
	view.hidden = YES;
	[emacs addSubview:view];
	return (__bridge_retained void *)view;
}

void excal_cursor_view_set_geometry(void *ptr, double x, double y,
                                    double width, double height, bool visible)
{
	ExcalCursorView *view = (__bridge ExcalCursorView *)ptr;
	NSView *parent = view.superview;
	if (!parent)
		return;
	/* The Emacs view is flipped, but convert in case it ever is not.  */
	double view_y = parent.isFlipped ? y : parent.bounds.size.height - y - height;
	view.frame = NSMakeRect(x, view_y, width, height);
	view.hidden = !visible;
	[view.window invalidateCursorRectsForView:view];
}

bool excal_cursor_set(void *ptr, const char *name)
{
	ExcalCursorView *view = (__bridge ExcalCursorView *)ptr;
	NSCursor *cursor = cursor_named(name);
	if (!cursor)
		return false;
	if (cursor != view.cursor) {
		view.cursor = cursor;
		[view.window invalidateCursorRectsForView:view];
	}
	/* Reassert on every call: Emacs may have set its own since.  */
	if ([view mouseInside] && NSCursor.currentCursor != cursor)
		[cursor set];
	return true;
}

bool excal_cursor_known(const char *name)
{
	/* By name: AppKit may have no cursors to hand out in batch mode.  */
	static const char *const names[] = {
	        "default",     "auto",        "pointer",     "text",
	        "crosshair",   "grab",        "grabbing",    "not-allowed",
	        "move",        "ns-resize",   "ew-resize",   "nwse-resize",
	        "nesw-resize", "eraser",      "eraser-dark",
	};
	for (size_t i = 0; i < sizeof names / sizeof *names; ++i)
		if (!strcmp(name, names[i]))
			return true;
	return false;
}

void excal_cursor_view_destroy(void *ptr)
{
	ExcalCursorView *view = CFBridgingRelease(ptr);
	[view removeFromSuperview];
}
