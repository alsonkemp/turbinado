include $(TOP)/config.mk
include $(TOP)/testsuite/check.mk

BIN=Main
SRC=Main.hs

BINDIR=		"."
REALBIN=	./$(BIN)

.SUFFIXES : .o .hs .hi .lhs .hc .s

all: $(BIN)

$(BIN): $(SRC) $(OBJS)
	@rm -f $@
	@$(GHC) --make -fglasgow-exts $(GHCFLAGS) $(PKGFLAGS) $(EXTRAFLAGS) $(SRC)

# Standard suffix rules
.o.hi:
	@:
.hs.o:
	@$(GHC) $(INCLUDES) $(PKGFLAGS) $(GHCFLAGS) $(EXTRAFLAGS) -c $<

clean: 
	@rm -rf *.hi *.o *~ $(BIN)
