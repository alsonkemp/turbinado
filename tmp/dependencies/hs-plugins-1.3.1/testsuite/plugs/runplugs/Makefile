GHCFLAGS=   -Onot $(GHC_EXTRA_OPTS)
PKGFLAGS=   -package posix
PKGFLAGS+=  -package plugins

all: build

build:
	@$(GHC) $(GHCFLAGS) $(PKGFLAGS) $(EXTRAFLAGS) Main.hs -o runplugs
include ../../../config.mk
check: build
	@(if [ -f "expected" ] ;\
	  then \
		actual_out="/tmp/hs-plugins-actual.out.$$$$"	   ;\
		diff_out="/tmp/hs-plugins.diff.$$$$"		   ;\
		cat test.in | ./runplugs > $$actual_out  2>&1 || true ;\
		diff -u expected $$actual_out > $$diff_out || true ;\
		if [ -s "$$diff_out" ] ; then \
			echo "failed with:"	;\
			cat "$$diff_out" | sed '1,3d' ;\
		else \
			echo "ok." 	;\
		fi ;\
		rm $$actual_out 	;\
	 else \
		cat test.in | ./runplugs 2>&1 || true ;\
	 fi)
clean:
	rm -rf *.hi *.o *~ *.dep ./runplugs

include ../../../config.mk
