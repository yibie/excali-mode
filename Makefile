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

# emacs-module.h of the Emacs being built against (it must have canvas_data).
EMACS_MODULE_INCLUDE ?= $(patsubst %/,%,$(dir $(firstword $(wildcard \
	$(dir $(EMACS_BIN))../include/emacs-module.h \
	/usr/local/include/emacs-module.h \
	/usr/include/emacs-module.h))))
ifeq ($(EMACS_MODULE_INCLUDE),)
$(error emacs-module.h not found; set EMACS_MODULE_INCLUDE=/path/to/include)
endif

SOURCES := src/excal-module.c src/excal-render.c src/excal-text.c src/excal-overlay.c src/excal-preview.c
HEADERS := src/excal-render.h src/excal-text.h src/excal-overlay.h src/excal-layer.h src/excal-preview.h
OBJECTS := $(patsubst src/%.c,build/%.o,$(SOURCES))
PACKAGES := cairo pangocairo

CPPFLAGS += -I$(EMACS_MODULE_INCLUDE) $(shell $(PKG_CONFIG) --cflags $(PACKAGES))
CFLAGS ?= -O2 -g
CFLAGS += -std=c11 -Wall -Wextra -Wno-unused-parameter
LDFLAGS += -shared
LDLIBS += $(shell $(PKG_CONFIG) --libs $(PACKAGES)) -lm

ifneq ($(PLATFORM),windows)
CFLAGS += -fPIC
endif

# macOS: optional CoreAnimation overlay backend, and the GUI binary lives
# inside Emacs.app so that a frame opens.
ifeq ($(PLATFORM),macos)
OBJECTS += build/excal-layer.o
CPPFLAGS += -DEXCAL_HAVE_LAYER
LDLIBS += -framework AppKit -framework QuartzCore -framework IOSurface
EMACS_GUI ?= $(firstword $(wildcard $(dir $(EMACS_BIN))../Emacs.app/Contents/MacOS/Emacs) $(EMACS))
else
EMACS_GUI ?= $(EMACS)
endif

.PHONY: all module test bench try info clean

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

test: module
	$(EMACS) --batch -Q -L . -L test -l ert $(addprefix -l ,$(TESTS)) -f ert-run-tests-batch-and-exit

# Needs a graphical session: opens a frame, benchmarks, writes bench.txt.
bench: module
	rm -f bench.txt
	$(EMACS_GUI) -Q -L $(CURDIR) -l $(CURDIR)/test/excal-gui-bench.el
	@cat bench.txt

# Open the sample scene in a clean GUI Emacs for manual testing.
try: module
	$(EMACS_GUI) -Q -L $(CURDIR) --eval "(progn (require 'excal) (excal-open \"$(CURDIR)/test/sample.excalidraw\"))"

clean:
	rm -rf build excal-module.so excal-module.dylib excal-module.dll *.o *.elc
