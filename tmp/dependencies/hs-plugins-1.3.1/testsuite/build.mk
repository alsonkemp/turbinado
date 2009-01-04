# how to build the default projects

include $(TOP)/config.mk
include $(TOP)/testsuite/check.mk

BIN= 		prog/Main
OBJ=		prog/Main.o
SRC=		prog/Main.hs

BINDIR=		prog
REALBIN=	./Main

API_OBJ=	api/API.o

INCLUDES=   	-i$(TOP)/testsuite/$(TEST)/api
GHCFLAGS=   	-Onot -cpp -fglasgow-exts

.SUFFIXES : .o .hs .hi .lhs .hc .s

all: $(BIN)

$(BIN) : $(PRIOR_OBJS) $(API_OBJ) $(SRC) $(EXTRA_OBJS)
	@rm -f $@
	@$(GHC) --make -o $@ $(INCLUDES) $(PKGFLAGS) $(GHCFLAGS) $(EXTRAFLAGS) $(API) $(SRC)

# Standard suffix rules
.o.hi:
	@:
.hs.o:
	@$(GHC) $(INCLUDES) $(PKGFLAGS) $(GHCFLAGS) $(EXTRAFLAGS) -c $<

clean:
	find . -name '*~' -exec rm {} \;
	rm -rf *.{o,hi,dep}
	rm -rf */*.{hi,o,old} */Main
	rm -rf */*core
	rm -rf */*.a
	rm -rf */package.conf
	rm -rf *.a

