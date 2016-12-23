#
#  Copyright (c) 2008-2014,
#  Reto Buerki, Adrian-Ken Rueegsegger
#
#  This file is part of Alog.
#
#  Alog is free software; you can redistribute it and/or modify
#  it under the terms of the GNU Lesser General Public License as published
#  by the Free Software Foundation; either version 2.1 of the License, or
#  (at your option) any later version.
#
#  Alog is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU Lesser General Public License for more details.
#
#  You should have received a copy of the GNU Lesser General Public License
#  along with Alog; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston,
#  MA  02110-1301  USA
#

PREFIX ?= $(HOME)/libraries
INSTALL = install

MAJOR = 0
MINOR = 5
REVISION = 2
VERSION = $(MAJOR).$(MINOR).$(REVISION)
ALOG = libalog-$(VERSION)
TARBALL = $(ALOG).tar.bz2

SO_LIBRARY = libalog.so.$(VERSION)
A_LIBRARY = libalog.a
LIBRARY_KIND = dynamic

SOURCEDIR = src
OBJECTDIR = obj
LIBDIR = lib
COVDIR = cov
ALI_FILES = lib/$(LIBRARY_KIND)/*.ali
GPR_FILE = gnat/alog.gpr

PWD = `pwd`

NUM_CPUS ?= 1

# GNAT_BUILDER_FLAGS, ADAFLAGS, CFLAGS and GNATFLAGS may be overridden in the
# environment or on the command line.
CFLAGS             ?= -W -Wall -Werror -O3
GNAT_BUILDER_FLAGS ?= -R -j$(NUM_CPUS)
GNATFLAGS          ?= ${GNAT_BUILDER_FLAGS} -cargs ${ADAFLAGS}
# GMAKE_OPTS should not be overridden because -p is essential.
GMAKE_OPTS = -p ${GNATFLAGS} -margs

all: build_lib

tests: build_tests
	@$(OBJECTDIR)/test_runner

build_lib:
	@gprbuild $(GMAKE_OPTS) -Palog -XALOG_VERSION="$(VERSION)" \
		-XLIBRARY_KIND="$(LIBRARY_KIND)" -XCFLAGS="$(CFLAGS)" \
		-XLDFLAGS="$(LDFLAGS)" -cargs $(ADAFLAGS)

build_tests:
	@gprbuild $(GMAKE_OPTS) -Palog_tests -XALOG_BUILD="tests"

build_all: build_lib build_tests

clean:
	@rm -f $(TARBALL)
	@rm -rf $(OBJECTDIR)
	@rm -rf $(LIBDIR)
	@rm -rf $(COVDIR)
	$(MAKE) -C doc clean

dist:
	@echo "Creating release tarball $(TARBALL) ... "
	@git archive --format=tar HEAD --prefix $(ALOG)/ | bzip2 > $(TARBALL)

install: install_lib install_$(LIBRARY_KIND)

install_lib: build_lib
	@mkdir -p $(PREFIX)/include/alog
	@mkdir -p $(PREFIX)/lib/alog
	@mkdir -p $(PREFIX)/lib/gnat
	$(INSTALL) -m 644 $(SOURCEDIR)/*.ad[bs] $(PREFIX)/include/alog
	$(INSTALL) -m 444 $(ALI_FILES) $(PREFIX)/lib/alog
	$(INSTALL) -m 644 $(GPR_FILE) $(PREFIX)/lib/gnat

install_static:
	$(INSTALL) -m 444 $(LIBDIR)/$(LIBRARY_KIND)/$(A_LIBRARY) $(PREFIX)/lib

install_dynamic:
	$(INSTALL) -m 444 $(LIBDIR)/$(LIBRARY_KIND)/$(SO_LIBRARY) $(PREFIX)/lib
	@cd $(PREFIX)/lib && ln -sf $(SO_LIBRARY) libalog.so

install_tests: build_tests
	$(INSTALL) -v -d $(PREFIX)/tests
	$(INSTALL) -m 755 $(OBJECTDIR)/test_runner $(PREFIX)/tests/
	@cp -vr data $(PREFIX)/tests

cov:
	@mkdir -p $(COVDIR)
	@rm -f $(OBJECTDIR)/cov/*.gcda
	@gprbuild $(GMAKE_OPTS) -Palog_tests -XALOG_BUILD="coverage"
	@$(OBJECTDIR)/cov/test_runner || true
	@lcov -c -d $(OBJECTDIR)/cov/ -o $(OBJECTDIR)/cov/alog_tmp.info
	@lcov -e $(OBJECTDIR)/cov/alog_tmp.info "$(PWD)/src/*.adb" \
		-o $(OBJECTDIR)/cov/alog.info
	@genhtml --no-branch-coverage $(OBJECTDIR)/cov/alog.info -o $(COVDIR)

prof:
	@rm -f $(OBJECTDIR)/callgrind.*
	gprbuild $(GMAKE_OPTS) -Palog_tests -XALOG_BUILD="profiling"
	valgrind -q --tool=callgrind \
		--callgrind-out-file=$(OBJECTDIR)/callgrind.out.%p $(OBJECTDIR)/profiler
	callgrind_annotate $(OBJECTDIR)/callgrind.* > $(OBJECTDIR)/profile.txt

doc:
	$(MAKE) -C doc

.PHONY: cov doc
