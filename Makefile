EMACS ?= emacs
CC ?= cc
PKG_CONFIG ?= pkg-config

# Platform: macos, windows (MSYS2/MinGW) or unix (Linux, BSD).
ifeq ($(OS),Windows_NT)
PLATFORM := windows
else ifeq ($(shell uname -s),Darwin)
PLATFORM := macos
else
PLATFORM := unix
endif

EMACS_BIN := $(shell readlink -f "$$(command -v $(EMACS))" 2>/dev/null)

# Module file name must use Emacs' own suffix (.so, .dylib or .dll).
MODULE_SUFFIX ?= $(shell $(EMACS) -Q --batch --eval "(princ module-file-suffix)" 2>/dev/null)
ifeq ($(MODULE_SUFFIX),)
MODULE_SUFFIX := $(if $(filter windows,$(PLATFORM)),.dll,$(if $(filter macos,$(PLATFORM)),.dylib,.so))
endif
MODULE := excal-module$(MODULE_SUFFIX)

# emacs-module.h of the Emacs being built against (it must have canvas_data):
# installed next to bin/, or beside src/emacs in an uninstalled build tree.
EMACS_MODULE_INCLUDE ?= $(patsubst %/,%,$(dir $(firstword $(wildcard \
	$(dir $(EMACS_BIN))../include/emacs-module.h \
	$(dir $(EMACS_BIN))emacs-module.h \
	/usr/local/include/emacs-module.h \
	/usr/include/emacs-module.h))))
ifeq ($(EMACS_MODULE_INCLUDE),)
$(error emacs-module.h not found; set EMACS_MODULE_INCLUDE=/path/to/include)
endif

SOURCES := src/excal-module.c src/excal-render.c src/excal-text.c src/excal-overlay.c \
	src/excal-preview.c src/excal-rough.c src/excal-shape.c src/excal-freehand.c src/excal-sticky.c \
	src/excal-image.c src/excal-frame.c src/excal-export.c src/excal-fill.c
HEADERS := src/excal-render.h src/excal-text.h src/excal-overlay.h src/excal-layer.h src/excal-cursor.h \
	src/excal-preview.h src/excal-rough.h src/excal-shape.h src/excal-freehand.h src/excal-sticky.h \
	src/excal-image.h src/excal-frame.h src/excal-export.h src/excal-fill.h
OBJECTS := $(patsubst src/%.c,build/%.o,$(SOURCES))
PACKAGES := cairo pangocairo

# zlib (embedded scenes in exports) is required; not every system ships
# a zlib.pc.
ifeq ($(shell $(PKG_CONFIG) --exists zlib && echo yes),yes)
PACKAGES += zlib
else
LDLIBS += -lz
endif

# Optional image decoders; PNG always works through Cairo.  Set e.g.
# EXCAL_WITH_RSVG=no to build without one.
EXCAL_WITH_RSVG ?= yes
EXCAL_WITH_PIXBUF ?= yes
EXCAL_WITH_WEBP ?= yes
ifeq ($(EXCAL_WITH_RSVG)$(shell $(PKG_CONFIG) --exists librsvg-2.0 && echo yes),yesyes)
PACKAGES += librsvg-2.0
CPPFLAGS += -DEXCAL_HAVE_RSVG
endif
ifeq ($(EXCAL_WITH_PIXBUF)$(shell $(PKG_CONFIG) --exists gdk-pixbuf-2.0 && echo yes),yesyes)
PACKAGES += gdk-pixbuf-2.0
CPPFLAGS += -DEXCAL_HAVE_PIXBUF
endif
ifeq ($(EXCAL_WITH_WEBP)$(shell $(PKG_CONFIG) --exists libwebp && echo yes),yesyes)
PACKAGES += libwebp
CPPFLAGS += -DEXCAL_HAVE_WEBP
endif

# Fonts in fonts/ are registered with fontconfig when Pango uses it
# (Linux, BSD, Homebrew on macOS), and with CoreText on macOS.
ifeq ($(shell $(PKG_CONFIG) --exists fontconfig pangofc && echo yes),yes)
PACKAGES += fontconfig pangofc
CPPFLAGS += -DEXCAL_HAVE_FONTCONFIG
endif

CPPFLAGS += -I$(EMACS_MODULE_INCLUDE) $(shell $(PKG_CONFIG) --cflags $(PACKAGES))
CFLAGS ?= -O2 -g
CFLAGS += -std=c11 -Wall -Wextra -Wno-unused-parameter
# The roughjs port must round like JS: no fused multiply-add.
CFLAGS += -ffp-contract=off
LDFLAGS += -shared
LDLIBS += $(shell $(PKG_CONFIG) --libs $(PACKAGES)) -lm

ifneq ($(PLATFORM),windows)
CFLAGS += -fPIC
else
LDLIBS += -lgdi32
endif

# macOS: optional CoreAnimation overlay backend, and the GUI binary lives
# inside Emacs.app so that a frame opens.
ifeq ($(PLATFORM),macos)
OBJECTS += build/excal-layer.o build/excal-cursor.o
CPPFLAGS += -DEXCAL_HAVE_LAYER -DEXCAL_HAVE_CORETEXT
LDLIBS += -framework AppKit -framework QuartzCore -framework IOSurface \
	-framework CoreText -framework CoreFoundation
EMACS_GUI ?= $(firstword $(wildcard $(dir $(EMACS_BIN))../Emacs.app/Contents/MacOS/Emacs) $(EMACS))
else
EMACS_GUI ?= $(EMACS)
endif

# emacs -Q skips the user's init, so point libgccjit at Homebrew gcc's
# runtime (libemutls_w) or trampolines fail: "error invoking gcc driver".
# A no-op where the glob matches nothing.
EMACS_Q_FIX = --eval "(let ((lib (car (file-expand-wildcards \"/opt/homebrew/opt/gcc/lib/gcc/current/gcc/*/*/libemutls_w.a\")))) (when lib (setq native-comp-driver-options (list (concat \"-L\" (file-name-directory lib))))))"

.PHONY: all module test bench try info clean fonts hero

all: module

module: $(MODULE)

$(MODULE): $(OBJECTS)
	$(CC) $(LDFLAGS) -o $@ $^ $(LDLIBS)

build/%.o: src/%.c $(HEADERS) | build
	$(CC) $(CPPFLAGS) $(CFLAGS) -c -o $@ $<

build/%.o: src/%.m $(HEADERS) | build
	$(CC) $(CPPFLAGS) -O2 -g -fPIC -fobjc-arc -Wall -c -o $@ $<

build:
	mkdir -p build

info:
	@echo "platform:  $(PLATFORM)"
	@echo "emacs:     $(EMACS_BIN)"
	@echo "gui emacs: $(EMACS_GUI)"
	@echo "module:    $(MODULE)"
	@echo "include:   $(EMACS_MODULE_INCLUDE)"
	@echo "objects:   $(OBJECTS)"

TESTS := $(wildcard test/*-test.el)

# Test files may (require 'excal-test) for its helpers, so skip files
# whose feature is already loaded instead of loading them twice.
test: module
	$(EMACS) --batch -Q -L . -L test -l ert \
	  --eval '(dolist (f (list $(foreach t,$(TESTS),"$(t)"))) (unless (featurep (intern (file-name-base f))) (load (expand-file-name f) nil t)))' \
	  -f ert-run-tests-batch-and-exit

# Needs a graphical session: opens a frame, benchmarks, writes bench.txt.
bench: module
	rm -f bench.txt
	$(EMACS_GUI) -Q $(EMACS_Q_FIX) -L $(CURDIR) -l $(CURDIR)/test/excal-gui-bench.el
	@cat bench.txt

# The README animation, rendered in batch by excal itself (needs ffmpeg).
hero: module
	rm -rf build/hero && mkdir -p build/hero
	EXCAL_HERO_FRAMES=$(CURDIR)/build/hero $(EMACS) -Q --batch -L $(CURDIR) \
	  -l $(CURDIR)/docs/media/hero.el -f hero-render
	ffmpeg -loglevel error -y -framerate 30 -i build/hero/f%05d.png \
	  -vf "fps=15,scale=800:-1:flags=lanczos,split[a][b];[a]palettegen=max_colors=64:stats_mode=diff[p];[b][p]paletteuse=dither=none:diff_mode=rectangle" \
	  docs/media/hero.gif
	ffmpeg -loglevel error -y -framerate 30 -i build/hero/f%05d.png \
	  -c:v libx264 -pix_fmt yuv420p -crf 18 -movflags +faststart build/hero.mp4

# Download Excalidraw's fonts into fonts/ (needs curl and network access;
# woff2_decompress and pyftmerge are used when installed).  See fonts/README.
fonts:
	$(EMACS) --batch -Q -l fonts/excal-fetch-fonts.el

# Open the sample scene in a clean GUI Emacs for manual testing.
try: module
	$(EMACS_GUI) -Q $(EMACS_Q_FIX) -L $(CURDIR) --eval "(progn (require 'excal) (excal-open \"$(CURDIR)/test/sample.excalidraw\"))"

clean:
	rm -rf build excal-module.so excal-module.dylib excal-module.dll *.o *.elc
