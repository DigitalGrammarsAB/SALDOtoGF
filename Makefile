.PHONY: build clean

# See if Stack is installed on system
STACK=$(shell if hash stack 2>/dev/null; then echo "1"; else echo "0"; fi)

build: src/*.hs
ifeq ($(STACK),1)
	stack build
else
	cabal build
endif

clean:
	rm -f build/*.pgf generate/*.gf* logs/*.txt
