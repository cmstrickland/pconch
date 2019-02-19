SHELL = /bin/sh
APPDIR = $(DESTDIR)/pconch

.PHONY: clean distclean pconch all install deb

build/pconch/pconch: $(wildcard *lisp)
	./build.sh

pconch: build/pconch/pconch

all: pconch

clean:
	rm -rf build

distclean: clean

deb: pconch
	debuild -uc -us -b

install:
	mkdir -p $(APPDIR)/posts $(APPDIR)/html
	install -D build/pconch/pconch $(APPDIR)/pconch
