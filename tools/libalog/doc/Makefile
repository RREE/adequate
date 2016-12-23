DESTDIR   ?= html
STYLEPATH ?= .

ASCIIDOC_OPTS = \
	--backend=xhtml11 \
	--conf-file=page.conf \
	-a stylesdir=$(STYLEPATH) \
	-a linkcss \
	-a disable-javascript \
	-o -

DST_HTML = \
	$(DESTDIR)/index.html \
	$(DESTDIR)/README.html \
	$(DESTDIR)/CHANGELOG.html

all: $(DESTDIR) $(DESTDIR)/asciidoc.css $(DESTDIR)/alog-arch.png $(DST_HTML)

$(DESTDIR)/alog-arch.png: arch/alog-arch.png
	cp $< $@

$(DESTDIR)/asciidoc.css: asciidoc.css
	cp $< $@

$(DESTDIR)/index.html: index page.conf
	TZ=UTC asciidoc $(ASCIIDOC_OPTS) $< > $@

$(DESTDIR)/README.html: ../README
	TZ=UTC asciidoc $(ASCIIDOC_OPTS) $< > $@

$(DESTDIR)/CHANGELOG.html: ../CHANGELOG
	TZ=UTC asciidoc $(ASCIIDOC_OPTS) $< > $@

$(DESTDIR):
	mkdir $@

clean:
	rm -f $(DESTDIR)/*.html
	rm -f $(DESTDIR)/*.css
	rm -f $(DESTDIR)/alog-arch.png

distclean:
	rm -rf $(DESTDIR)
