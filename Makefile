SHELL=/bin/bash
GRAMMAR_FILES=ParLatte.hs,PrintLatte.hs,AbsLatte.hs,LexLatte.hs,ErrM.hs
OUT=latc_x86

all: $(OUT)

$(OUT): src/*.hs src/ParLatte.hs
	cd src && ghc Latc.hs -o $(OUT)
	mv src/$(OUT) .

src/ParLatte.hs: src/bnfc/Latte.cf
	cd src/bnfc && bnfc -m --haskell Latte.cf && $(MAKE)
	cp src/bnfc/{$(GRAMMAR_FILES)} ./src/
	cd src/bnfc && make distclean

tags: src/*.hs
	hasktags .
	rm TAGS
	mv ctags tags

.PHONY: clean
clean:
	-cd src/bnfc && $(MAKE) distclean
	-rm -f src/{$(GRAMMAR_FILES)} 2>/dev/null
	-rm -f src/*.hi 2>/dev/null
	-rm -f src/*.o 2>/dev/null
	-rm -f $(OUT) 2>/dev/null

.PHONY: test
test:
	./test.sh
