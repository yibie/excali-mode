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
MODULE := excali-module$(MODULE_SUFFIX)

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

SOURCES := src/excali-module.c src/excali-render.c src/excali-text.c src/excali-overlay.c \
	src/excali-preview.c src/excali-rough.c src/excali-shape.c src/excali-freehand.c src/excali-sticky.c \
	src/excali-image.c src/excali-frame.c src/excali-export.c src/excali-fill.c
HEADERS := src/excali-render.h src/excali-text.h src/excali-overlay.h src/excali-layer.h src/excali-cursor.h \
	src/excali-preview.h src/excali-rough.h src/excali-shape.h src/excali-freehand.h src/excali-sticky.h \
	src/excali-image.h src/excali-frame.h src/excali-export.h src/excali-fill.h
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
# EXCALI_WITH_RSVG=no to build without one.
EXCALI_WITH_RSVG ?= yes
EXCALI_WITH_PIXBUF ?= yes
EXCALI_WITH_WEBP ?= yes
ifeq ($(EXCALI_WITH_RSVG)$(shell $(PKG_CONFIG) --exists librsvg-2.0 && echo yes),yesyes)
PACKAGES += librsvg-2.0
CPPFLAGS += -DEXCALI_HAVE_RSVG
endif
ifeq ($(EXCALI_WITH_PIXBUF)$(shell $(PKG_CONFIG) --exists gdk-pixbuf-2.0 && echo yes),yesyes)
PACKAGES += gdk-pixbuf-2.0
CPPFLAGS += -DEXCALI_HAVE_PIXBUF
endif
ifeq ($(EXCALI_WITH_WEBP)$(shell $(PKG_CONFIG) --exists libwebp && echo yes),yesyes)
PACKAGES += libwebp
CPPFLAGS += -DEXCALI_HAVE_WEBP
endif

# Fonts in fonts/ are registered with fontconfig when Pango uses it
# (Linux, BSD, Homebrew on macOS), and with CoreText on macOS.
ifeq ($(shell $(PKG_CONFIG) --exists fontconfig pangofc && echo yes),yes)
PACKAGES += fontconfig pangofc
CPPFLAGS += -DEXCALI_HAVE_FONTCONFIG
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
OBJECTS += build/excali-layer.o build/excali-cursor.o
CPPFLAGS += -DEXCALI_HAVE_LAYER -DEXCALI_HAVE_CORETEXT
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

.PHONY: all module compile test bench try info clean fonts hero

LISP := $(wildcard excali*.el)

all: module compile

# Byte-compile the Lisp (Emacs then native-compiles it in the background
# where it can).  Loaded from source, excali runs several times slower.
# Everything is recompiled when any file changes, since files share macros.
compile: build/elc.stamp

build/elc.stamp: $(LISP) $(MODULE) | build
	$(EMACS) --batch -Q -L . -f batch-byte-compile $(LISP)
	@touch $@

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

# Test files may (require 'excali-test) for its helpers, so skip files
# whose feature is already loaded instead of loading them twice.
test: compile
	$(EMACS) --batch -Q -L . -L test -l ert \
	  --eval '(dolist (f (list $(foreach t,$(TESTS),"$(t)"))) (unless (featurep (intern (file-name-base f))) (load (expand-file-name f) nil t)))' \
	  -f ert-run-tests-batch-and-exit

# Needs a graphical session: opens a frame, benchmarks, writes bench.txt.
bench: compile
	rm -f bench.txt
	$(EMACS_GUI) -Q $(EMACS_Q_FIX) -L $(CURDIR) -l $(CURDIR)/test/excali-gui-bench.el
	@cat bench.txt

# The README animation, rendered in batch by excali itself (needs ffmpeg).
hero: compile
	rm -rf build/hero && mkdir -p build/hero
	EXCALI_HERO_FRAMES=$(CURDIR)/build/hero $(EMACS) -Q --batch -L $(CURDIR) \
	  -l $(CURDIR)/docs/media/hero.el -f hero-render
	ffmpeg -loglevel error -y -framerate 30 -i build/hero/f%05d.png \
	  -vf "fps=15,scale=800:-1:flags=lanczos,split[a][b];[a]palettegen=max_colors=64:stats_mode=diff[p];[b][p]paletteuse=dither=none:diff_mode=rectangle" \
	  docs/media/hero.gif
	ffmpeg -loglevel error -y -framerate 30 -i build/hero/f%05d.png \
	  -c:v libx264 -pix_fmt yuv420p -crf 18 -movflags +faststart build/hero.mp4

# Download Excalidraw's fonts into fonts/ (needs curl and network access;
# woff2_decompress and pyftmerge are used when installed).  See fonts/README.
fonts:
	$(EMACS) --batch -Q -l fonts/excali-fetch-fonts.el

# Open the sample scene in a clean GUI Emacs for manual testing.
try: compile
	$(EMACS_GUI) -Q $(EMACS_Q_FIX) -L $(CURDIR) --eval "(progn (require 'excali) (excali-open \"$(CURDIR)/test/sample.excalidraw\"))"

clean:
	rm -rf build excali-module.so excali-module.dylib excali-module.dll *.o *.elc
