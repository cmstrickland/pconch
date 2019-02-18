SHELL = /bin/sh

.PHONY: clean distclean pconch all install deb

build/pconch/pconch: $(wildcard *lisp)
	./build.sh

pconch: build/pconch/pconch

all: pconch

clean:
	rm -rf build

distclean: clean

deb: distclean
	debuild -uc -us -b

install:
	install -D build/pconch/pconch $(DESTDIR)/pconch/pconch
