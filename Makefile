SHELL = /bin/sh
APPDIR = $(DESTDIR)/pconch
override INSTALL = install
unexport CFLAGS
eval = sbcl --non-interactive --load ~/quicklisp/setup.lisp --eval

.PHONY: clean distclean pconch all install deb version release

.deps: *.lisp
	$(eval) "(ql:quickload 'pconch)"
	touch .deps

pconch: .deps
	$(eval) "(asdf:make 'pconch)"

all: pconch

clean:
	rm -rf build manifest.txt pconch .deps

distclean: clean
	@if [ -d .git ]; then git clean -xfd; fi

version:
	dch -r 'version bumped by make version'

release: version deb

deb:
	DEB_BUILD_OPTIONS='nostrip' debuild -uc -us -b

install: pconch
	mkdir -p $(APPDIR)/posts $(APPDIR)/html $(APPDIR)/html/.cache
	install -D pconch $(APPDIR)/pconch
	install -D -d templates $(DESTDIR)/usr/share/pconch/
	install -D -d styles $(DESTDIR)/usr/share/pconch/templates/
	cp -r templates/* $(DESTDIR)/usr/share/pconch/templates/
	cp -r templates/styles/* $(DESTDIR)/usr/share/pconch/templates/styles/
	mv templates/lorem.post $(APPDIR)/posts/
