SHELL = /bin/sh
APPDIR = $(DESTDIR)/pconch
override INSTALL = install
unexport CFLAGS
eval = ros -Q -e

.PHONY: clean distclean pconch all install manifest.txt

pconch: $(wildcard *lisp)  manifest.txt
	buildapp --output $@ --manifest-file manifest.txt --entry 'pconch:main' \
	--load-system cl-who \
	--load-system hunchentoot \
	--load-system quri \
	--load-system lquery \
	--load-system array-utils \
	--load-system clss \
	--load-system trivial-indent \
	--load-system uiop \
	--load-system myway \
	--load-system cl-ppcre \
	--load-system cl-markdown \
	--load-system clache \
	--load-system local-time \
	--load-system bordeaux-threads \
	--load-system cl-who \
	--load-system pconch

manifest.txt: pconch.asd
	$(eval) '(ql:quickload :pconch) (ql:write-asdf-manifest-file "manifest.txt")'

all: pconch

clean:
	rm -rf build

distclean: clean
	git clean -xfd

install:
	mkdir -p $(APPDIR)/posts $(APPDIR)/html $(APPDIR)/html/.cache
	install -D build/pconch/pconch $(APPDIR)/pconch
	install -D -d templates $(DESTDIR)/usr/share/pconch/
	install -D -d styles $(DESTDIR)/usr/share/pconch/templates/
	cp -r templates/* $(DESTDIR)/usr/share/pconch/templates/
	cp -r templates/styles/* $(DESTDIR)/usr/share/pconch/templates/styles/


deb: distclean
	gbp buildpackage --git-debian-branch=debian/buster --git-upstream-tree=upstream --git-force-create
