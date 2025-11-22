SHELL = /bin/sh
APPDIR = $(DESTDIR)/pconch
override INSTALL = install
unexport CFLAGS

# Try to find quicklisp in common locations
QUICKLISP_SETUP := $(shell \
	if [ -f "$$HOME/quicklisp/setup.lisp" ]; then \
		echo "$$HOME/quicklisp/setup.lisp"; \
	elif [ -f /usr/share/cl-quicklisp/quicklisp.lisp ]; then \
		echo /usr/share/cl-quicklisp/quicklisp.lisp; \
	else \
		echo /usr/share/cl-quicklisp/quicklisp.lisp; \
	fi)

.PHONY: clean distclean pconch all install deb version release

.deps: *.lisp
	sbcl --non-interactive --load $(QUICKLISP_SETUP) --eval "(ql:quickload 'pconch)" --quit
	touch .deps

pconch: .deps
	sbcl --non-interactive --load $(QUICKLISP_SETUP) --eval "(asdf:make 'pconch)" --quit

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
	install -D templates/lorem.post $(APPDIR)/posts/lorem.post
