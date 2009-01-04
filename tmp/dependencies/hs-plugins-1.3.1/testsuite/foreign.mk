include $(TOP)/config.mk
include $(TOP)/testsuite/check.mk


INCLUDES=   -I$(TOP)

# compile with GHC to save us setting all the necessary include and
# lib flags. use ghc -v to find out what these are if you wish to go
# via gcc.
BIN=./Main
SRC=main.c

BINDIR=		"."
REALBIN=	$(BIN)

all:  $(BIN)

$(BIN): $(SRC)
	$(GHC) -package plugins $(INCLUDES) $(PKGFLAGS) $(SRC)

clean: 
	rm -rf *.hi *.o *~ $(BIN)
