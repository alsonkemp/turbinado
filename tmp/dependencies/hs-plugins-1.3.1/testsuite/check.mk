include $(TOP)/config.mk

check: $(BIN)
	@(cd $(BINDIR) ;\
	  expected="expected" ;\
          if [ -f "expected" -o -f "expected.$(GLASGOW_HASKELL)" ] ;\
	  then \
		actual_out="/tmp/hs-plugins-actual.out.$$$$"	   ;\
		diff_out="/tmp/hs-plugins.diff.$$$$"		   ;\
		$(REALBIN) > $$actual_out  2>&1         || true ;\
		if [ -f "expected.$(GLASGOW_HASKELL)" ] ; then \
			expected="expected.$(GLASGOW_HASKELL)" ;\
		fi ;\
		diff -u $$expected $$actual_out > $$diff_out || true ;\
		if [ -s "$$diff_out" ] ; then \
			echo "failed with:"	;\
			cat "$$diff_out" | sed '1,3d' ;\
		else \
			echo "ok." 	;\
		fi ;\
		rm $$actual_out $$diff_out	;\
	 else \
		$(REALBIN) 2>&1 || true ;\
	 fi)
