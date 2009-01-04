all:
	@echo "test disabled"

#true_api::
#	( cd api ;\
#	$(GHC) -Onot $(EXTRAFLAGS) -c $(API).hs ;\
#	$(RM) -f libHSapi.a ;\
#	$(AR) cq libHSapi.a API.o ;\
#	$(RANLIB) libHSapi.a ;\
#	$(LD) -r $(LD_X) $(WHOLE_ARCHIVE_FLAG) -o HSapi.o libHSapi.a ;\
#	rm API.o ;\
#	echo [] > package.conf ;\
#	env PREFIX=`pwd` $(GHC_PKG) -f package.conf -u < package.conf.in )
#	$(GHC) -package-conf ${TOP}/plugins.conf.inplace -package plugins \
#		-package-conf api/package.conf            -package api \
#		-O $(EXTRAFLAGS) -c Null.hs
