# Copyright (c) 2004 Don Stewart - http://www.cse.unsw.edu.au/~dons
# LGPL version 2.1 or later (see http://www.gnu.org/copyleft/lesser.html)

.PHONY: build clean html

SRC = hs-plugins

build: $(SRC).ps html

$(SRC).ps: $(SRC).dvi
	dvips -f $(SRC).dvi > $@

html: $(SRC).tex
	tex2page $(SRC)
	tex2page $(SRC)
	sed -f munge.sed < $(SRC)/$(SRC).html > tmp.out
	mv tmp.out $(SRC)/$(SRC).html
	cp $(SRC)/$(SRC).html $(SRC)/index.html
	tar czf $(SRC).html.tar.gz $(SRC)
	mv $(SRC).html.tar.gz $(SRC)/

$(SRC).dvi: $(SRC).tex
	latex $(SRC).tex && latex $(SRC).tex

CLEANS= *.{ps,dvi,aux,log} *~ hs-plugins *-Z-* *.toc

clean: 
	rm -rf $(CLEANS)

all: doc

