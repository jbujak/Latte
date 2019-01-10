SHELL=/bin/bash
GRAMMAR_FILES=ParLatte.hs PrintLatte.hs AbsLatte.hs LexLatte.hs ErrM.hs
OUT=latc_x86_64

all: $(OUT)

$(OUT): src/*.hs src/bnfc/*.hs
	cd src && ghc -o $(OUT) -i $(addprefix bnfc/,$(GRAMMAR_FILES)) -i *.hs
	mv src/$(OUT) .

bnfc: src/bnfc/Latte.cf
	cd src/bnfc && bnfc -m --haskell Latte.cf && $(MAKE)

tags: src/*.hs
	hasktags .
	rm TAGS

.PHONY: clean
clean:
	-rm -f src/*.hi 2>/dev/null
	-rm -f src/*.o 2>/dev/null
	-rm -f $(OUT) 2>/dev/null
	-rm -rf src/bin
	-rm -rf tags
	-rm -f src/bnfc/*.{hi,o,x,y,txt}
	-rm -f src/bnfc/TestLatte

.PHONY: test
test:
	./test.sh
