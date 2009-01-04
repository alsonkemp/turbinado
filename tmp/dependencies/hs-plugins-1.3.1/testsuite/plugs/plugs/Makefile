GHCFLAGS=   -O
PKGFLAGS+=  -package plugins -package readline

all: build

build:
	@$(GHC) $(GHCFLAGS) $(PKGFLAGS) $(EXTRAFLAGS) Main.hs -o plugs
check: build
	@(if [ -f "expected" ] ;\
	  then \
		actual_out="/tmp/hs-plugins-actual.out.$$$$"	   ;\
		diff_out="/tmp/hs-plugins.diff.$$$$"		   ;\
		cat test.in | ./plugs > $$actual_out  2>&1 || true ;\
		diff -u expected $$actual_out > $$diff_out || true ;\
		if [ -s "$$diff_out" ] ; then \
			echo "failed with:"	;\
			cat "$$diff_out" | sed '1,3d' ;\
		else \
			echo "ok." 	;\
		fi ;\
		rm $$actual_out 	;\
	 else \
		cat test.in | ./plugs 2>&1 || true ;\
	 fi)
clean:
	rm -rf *.hi *.o *~ *.dep ./plugs

include ../../../config.mk
