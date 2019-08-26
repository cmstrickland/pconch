SHELL = /bin/sh
APPDIR = $(DESTDIR)/pconch
override INSTALL = install
unexport CFLAGS
eval = ros -Q -e
load-systems = $(shell ./scripts/parse-deps.lisp ./pconch.asd) --load-system pconch

.PHONY: clean distclean pconch all install manifest.txt

pconch: $(wildcard *lisp)  manifest.txt
	buildapp --output $@ --manifest-file manifest.txt --entry 'pconch:main' \
	$(load-systems)

manifest.txt: pconch.asd
	$(eval) '(ql:quickload :pconch) (ql:write-asdf-manifest-file "manifest.txt")'

all: pconch

clean:
	rm -rf build

distclean: clean
	git clean -xfd

install:
	mkdir -p $(APPDIR)/posts $(APPDIR)/html $(APPDIR)/html/.cache
	install -D pconch $(APPDIR)/pconch
	install -D -d templates $(DESTDIR)/usr/share/pconch/
	install -D -d styles $(DESTDIR)/usr/share/pconch/templates/
	cp -r templates/* $(DESTDIR)/usr/share/pconch/templates/
	cp -r templates/styles/* $(DESTDIR)/usr/share/pconch/templates/styles/


deb: distclean
	gbp buildpackage --git-debian-branch=debian/buster --git-upstream-tree=upstream --git-force-create
