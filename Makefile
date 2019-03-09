SHELL = /bin/sh
APPDIR = $(DESTDIR)/pconch
override INSTALL = install


.PHONY: clean distclean pconch all install deb

build/pconch/pconch: $(wildcard *lisp)
	./build.sh

pconch: build/pconch/pconch

all: pconch

clean:
	rm -rf build

distclean: clean

deb: pconch
	DEB_BUILD_OPTIONS='nostrip' debuild -uc -us -b

install:
	mkdir -p $(APPDIR)/posts $(APPDIR)/html $(APPDIR)/html/.cache
	install -D build/pconch/pconch $(APPDIR)/pconch
	install -D -d templates $(DESTDIR)/usr/share/pconch/
	install -D -d styles $(DESTDIR)/usr/share/pconch/templates/
	cp -r templates/* $(DESTDIR)/usr/share/pconch/templates/
	cp -r templates/styles/* $(DESTDIR)/usr/share/pconch/templates/styles/
