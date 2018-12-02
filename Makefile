SHELL=/bin/bash
CXX=g++
CXXFLAGS= -Wall -Wextra -std=c++17

SRCDIR=src
BINDIR=bin
BNFC_BINDIR=$(BINDIR)/bnfc
OUT=latc_x86

SRCS := $(shell find src/ -name '*.cpp' -printf "%f\n")
OBJS := $(addprefix $(BINDIR)/,$(SRCS:%.cpp=%.o))

$(OUT): $(BINDIR) $(OBJS) $(BNFC_BINDIR)/Absyn.o
	$(CXX) $(CXXFLAGS) $(BINDIR)/*.o $(BNFC_BINDIR)/*.o -o $(OUT)

$(BINDIR):
	-mkdir $(BINDIR) 2>/dev/null
	-mkdir $(BNFC_BINDIR) 2>/dev/null

$(BNFC_BINDIR)/Absyn.o: src/bnfc/Latte.cf
	cd src/bnfc && bnfc -m --cpp Latte.cf && $(MAKE)
	rm src/bnfc/Test.o
	cp src/bnfc/*.o $(BNFC_BINDIR)/

tags: src/*.cpp
	ctags -R .

.SECONDEXPANSION:
$(BINDIR)/%.o: $(SRCDIR)/%.cpp $$(wildcard $$(SRCDIR)/%.h)
	$(CXX) $(CXXFLAGS) -c $(SRCDIR)/$*.cpp -o $(BINDIR)/$*.o

.PHONY: clean
clean:
	-cd src/bnfc && $(MAKE) distclean
	-rm -f $(OUT) 2>/dev/null
	-rm -rf $(BINDIR) 2>/dev/null
	-rm -rg tags 2>/dev/null

.PHONY: test
test:
	./test.sh
