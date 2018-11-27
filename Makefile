SHELL=/bin/bash
CXX=g++

SRCDIR=src
BINDIR=bin
BNFC_BINDIR=$(BINDIR)/bnfc
OUT=latc_x86

SRCS := $(shell find src/ -name '*.cpp' -printf "%f\n")
OBJS := $(addprefix $(BINDIR)/,$(SRCS:%.cpp=%.o))

latc: $(BINDIR) $(OBJS) $(BNFC_BINDIR)/Absyn.o
	$(CXX) $(BINDIR)/*.o $(BNFC_BINDIR)/*.o -o $(OUT)

$(BINDIR):
	mkdir $(BINDIR)
	mkdir $(BNFC_BINDIR)

$(BNFC_BINDIR)/Absyn.o: src/bnfc/Latte.cf
	cd src/bnfc && bnfc -m --cpp Latte.cf && $(MAKE)
	rm src/bnfc/Test.*
	cp src/bnfc/*.o $(BNFC_BINDIR)/

tags: src/*.c
	ctags -R .
	rm tags

$(BINDIR)/%.o: $(SRCDIR)/%.cpp
	$(CXX) -c $(SRCDIR)/$*.cpp -o $(BINDIR)/$*.o

.PHONY: clean
clean:
	-cd src/bnfc && $(MAKE) distclean
	-rm -f $(OUT) 2>/dev/null
	-rm -rf $(BINDIR) 2>/dev/null

.PHONY: test
test:
	./test.sh
