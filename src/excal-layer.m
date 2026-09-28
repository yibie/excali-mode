/* excal-layer.m --- CoreAnimation overlay for excal.el (macOS)  -*- c-file-style: "linux" -*-
 * Copyright (C) 2026 yibie
 * SPDX-License-Identifier: GPL-3.0-or-later
 *
 * Emacs' NS canvas refresh copies pixels one Objective-C message at a
 * time.  This overlay bypasses it: a CALayer is added above the Emacs
 * view's backing layer and fed double-buffered IOSurfaces, the same way
 * EmacsLayer presents its own frame.  The layer ignores events, so mouse
 * input still reaches Emacs at unchanged coordinates.
 */

#import <AppKit/AppKit.h>
#import <IOSurface/IOSurface.h>
#import <QuartzCore/QuartzCore.h>
#include <string.h>

#include "excal-layer.h"

@interface ExcalOverlay : NSObject
@property(nonatomic, strong) CALayer *layer;
@property(nonatomic, weak) NSView *view;
@end

@implementation ExcalOverlay {
	IOSurfaceRef surfaces[2];
	int current;
}

- (void)dealloc
{
	[self.layer removeFromSuperlayer];
	for (int i = 0; i < 2; ++i)
		if (surfaces[i])
			CFRelease(surfaces[i]);
}

- (IOSurfaceRef)backSurfaceWidth:(int)width height:(int)height
{
	int back = 1 - current;
	IOSurfaceRef s = surfaces[back];
	if (s && ((int)IOSurfaceGetWidth(s) != width ||
	          (int)IOSurfaceGetHeight(s) != height)) {
		CFRelease(s);
		s = surfaces[back] = NULL;
	}
	if (!s) {
		size_t bpr = IOSurfaceAlignProperty(kIOSurfaceBytesPerRow,
		                                    (size_t)width * 4);
		s = IOSurfaceCreate((__bridge CFDictionaryRef) @{
			(id)kIOSurfaceWidth : @(width),
			(id)kIOSurfaceHeight : @(height),
			(id)kIOSurfaceBytesPerRow : @(bpr),
			(id)kIOSurfaceBytesPerElement : @4,
			(id)kIOSurfacePixelFormat : @((unsigned)'BGRA'),
		});
		surfaces[back] = s;
	}
	return s;
}

- (BOOL)present:(const uint32_t *)pixels width:(int)width height:(int)height
{
	IOSurfaceRef s = [self backSurfaceWidth:width height:height];
	if (!s || IOSurfaceLock(s, 0, NULL) != kIOReturnSuccess)
		return NO;
	uint8_t *base = IOSurfaceGetBaseAddress(s);
	size_t bpr = IOSurfaceGetBytesPerRow(s);
	if (bpr == (size_t)width * 4)
		memcpy(base, pixels, (size_t)width * height * 4);
	else
		for (int y = 0; y < height; ++y)
			memcpy(base + y * bpr, pixels + (size_t)y * width,
			       (size_t)width * 4);
	IOSurfaceUnlock(s, 0, NULL);
	current = 1 - current;
	[CATransaction begin];
	[CATransaction setDisableActions:YES];
	self.layer.contents = (__bridge id)s;
	[CATransaction commit];
	return YES;
}
@end

static NSView *find_emacs_view(NSView *root)
{
	if ([NSStringFromClass([root class]) isEqualToString:@"EmacsView"])
		return root;
	for (NSView *child in root.subviews) {
		NSView *found = find_emacs_view(child);
		if (found)
			return found;
	}
	return nil;
}

void *excal_find_emacs_view(double left, double top, double width,
                            double height)
{
	CGFloat screen_height = NSScreen.screens.firstObject.frame.size.height;
	NSView *best = nil;
	double best_distance = 1e18;
	for (NSWindow *window in NSApp.windows) {
		NSView *view = find_emacs_view(window.contentView);
		if (!view || !view.layer)
			continue;
		NSRect r = [window
		        convertRectToScreen:[view convertRect:view.bounds
		                                       toView:nil]];
		double view_top = screen_height - NSMaxY(r);
		double d = fabs(NSMinX(r) - left) + fabs(view_top - top) +
		           fabs(NSWidth(r) - width) + fabs(NSHeight(r) - height);
		if (d < best_distance) {
			best_distance = d;
			best = view;
		}
	}
	return (__bridge void *)best;
}

void *excal_layer_create(double left, double top, double width,
                         double height)
{
	NSView *best = (__bridge NSView *)excal_find_emacs_view(left, top, width,
	                                                        height);
	if (!best)
		return NULL;
	ExcalOverlay *overlay = [ExcalOverlay new];
	overlay.view = best;
	CALayer *layer = [CALayer layer];
	layer.actions = @{
		@"contents" : [NSNull null],
		@"bounds" : [NSNull null],
		@"position" : [NSNull null],
		@"hidden" : [NSNull null],
	};
	layer.anchorPoint = CGPointZero;
	layer.contentsGravity = kCAGravityResize;
	layer.hidden = YES;
	[best.layer addSublayer:layer];
	overlay.layer = layer;
	return (__bridge_retained void *)overlay;
}

void excal_layer_set_geometry(void *ptr, double x, double y, double width,
                              double height, double scale, bool visible)
{
	ExcalOverlay *overlay = (__bridge ExcalOverlay *)ptr;
	CALayer *parent = overlay.layer.superlayer;
	if (!parent)
		return;
	/* Layer coordinates have a bottom-left origin unless flipped.  */
	double layer_y = parent.geometryFlipped
	                         ? y
	                         : parent.bounds.size.height - y - height;
	[CATransaction begin];
	[CATransaction setDisableActions:YES];
	overlay.layer.frame = CGRectMake(x, layer_y, width, height);
	overlay.layer.contentsScale = scale;
	overlay.layer.hidden = !visible;
	[CATransaction commit];
}

bool excal_layer_present(void *ptr, const uint32_t *pixels, int width,
                         int height)
{
	ExcalOverlay *overlay = (__bridge ExcalOverlay *)ptr;
	return [overlay present:pixels width:width height:height];
}

void excal_layer_flush(void)
{
	[CATransaction flush];
}

void excal_layer_destroy(void *ptr)
{
	CFBridgingRelease(ptr);
}
