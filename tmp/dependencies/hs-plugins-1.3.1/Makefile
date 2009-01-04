# Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
# LGPL version 2.1 or later (see http://www.gnu.org/copyleft/lesser.html)

#
# regress check. TODO check expected output
# 
check:
	@( d=/tmp/plugins.tmp.$$$$ ; mkdir $$d ; export TMPDIR=$$d ;\
	   for i in `find testsuite ! -name CVS -type d -maxdepth 2 -mindepth 2 | sort` ; do \
		printf "=== testing %-50s ... " "$$i" ;	\
		( cd $$i ; if [ -f dont_test ] ; then \
		 	echo "ignored."	;\
		  else ${MAKE} -sk && ${MAKE} -ksi check |\
			sed '/^Compil/d;/^Load/d;/Read/d;/Expan/d;/Savi/d;/Writ/d' ;\
		       ${MAKE} -sk clean ;\
		  fi ) 2> /dev/null ;\
	   done ; rm -rf $$d )
	
#
# making clean
#

CLEAN_FILES += *.conf.*.old *~

EXTRA_CLEANS+=*.conf.inplace* *.conf.in *.h autom4te.cache \
	      config.h config.mk config.log config.status

clean:
	cd docs && $(MAKE) clean
	runhaskell Setup.lhs clean 2> /dev/null || true
	rm -rf $(CLEAN_FILES)
	find testsuite -name '*.a' -exec rm {} \;
	find testsuite -name '*~' -exec rm {} \;
	find testsuite -name 'a.out' -exec rm {} \;
	find testsuite -name '*.hi' -exec rm {} \;
	find testsuite -name '*.o' -exec rm {} \;
	find testsuite -name '*.core' -exec rm {} \;
	find testsuite -name 'package.conf' -exec rm {} \;
	rm -f testsuite/makewith/io/TestIO.conf
	rm -f testsuite/makewith/unsafeio/Unsafe.conf
	rm -rf testsuite/plugs/plugs/plugs
	rm -rf testsuite/plugs/plugs/runplugs
	rm -rf $(EXTRA_CLEANS)

