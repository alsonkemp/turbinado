APP_DIR = build/PluginExpressionParser.app
APP_CONTENTS_DIR = $(APP_DIR)/Contents
APP_ARCH_EXEC_DIR = $(APP_CONTENTS_DIR)/MacOS
APP_RESOURCES_DIR = $(APP_CONTENTS_DIR)/Resources
EXECUTABLE = $(APP_ARCH_EXEC_DIR)/PluginExpressionParser

OBJECT_FILES = main.o MyDocument.o PluginEvalAux.o
BUILD_OBJECT_FILES = $(addprefix build/,$(OBJECT_FILES)) \
		     build/PluginEvalAux_stub.o

HOST = $(shell uname)

ifeq ($(HOST),Darwin)
default: app
else
default: no_app
endif

app: $(APP_CONTENTS_DIR) $(APP_RESOURCES_DIR) $(EXECUTABLE)

#

$(EXECUTABLE): $(APP_ARCH_EXEC_DIR) $(BUILD_OBJECT_FILES)
	ghc \
	  -o "$(EXECUTABLE)" \
	  -framework Cocoa \
	  -package-conf ../../../plugins.conf.inplace \
	  -package plugins \
	  -no-hs-main \
	  $(BUILD_OBJECT_FILES)

build/MyDocument.o: MyDocument.m MyDocument.h
	gcc -c -o "$@" -Wall -I`ghc --print-libdir`/include "$<"

build/main.o: main.m
	gcc -c -o "$@" -Wall -I`ghc --print-libdir`/include "$<"

build/PluginEvalAux.o: PluginEvalAux.hs
	ghc --make \
	  -package-conf ../../../plugins.conf.inplace \
	  -package plugins \
	  -odir build/ \
	  -hidir build/ \
	  "$<"

#

$(APP_DIR):
	mkdir -p "$@"

$(APP_ARCH_EXEC_DIR): $(APP_DIR)
	mkdir -p "$@"

$(APP_CONTENTS_DIR): $(APP_DIR) Info.plist
	mkdir -p "$(APP_CONTENTS_DIR)"
	cp Info.plist "$@"
	echo -n 'APPL????' > "$@"/PkgInfo

$(APP_RESOURCES_DIR): $(APP_DIR) English.lproj
	mkdir -p "$(APP_RESOURCES_DIR)"
	cp -R English.lproj "$@"

#

clean:
	-rm -rf build *_stub.?

