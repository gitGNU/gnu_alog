#
#  Copyright (c) 2008,
#  Reto Buerki, Adrian-Ken Rueegsegger
#  secunet SwissIT AG
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
BUILD_TYPE ?= "base"

MAJOR = 0
MINOR = 3
VERSION = $(MAJOR).$(MINOR)
ALOG = libalog-$(VERSION)

SOURCEDIR = src
APIDOCDIR = doc
COVDIR = $(APIDOCDIR)/cov
ALI_FILES = lib/*.ali
SO_LIBRARY = libalog.so.$(VERSION)

TMPDIR = /tmp
DISTDIR = $(TMPDIR)/$(ALOG)
TARBALL = $(ALOG).tar.bz2
PWD = `pwd`

all: build_lib

tests: build_tests
	@obj/runner_$(BUILD_TYPE)

build_lib: prepare
	@gnatmake -Palog_lib -XALOG_VERSION="$(VERSION)" -XALOG_BUILD="$(BUILD_TYPE)"

build_tests: prepare
	@gnatmake -Palog_tests -XALOG_BUILD="$(BUILD_TYPE)"

prepare: $(SOURCEDIR)/alog-version.ads
	@mkdir -p obj/lib obj/tests lib

$(SOURCEDIR)/alog-version.ads:
	@echo "package Alog.Version is"                 > $@
	@echo "   Version_Number : constant Float  :="  >> $@
	@echo "      $(VERSION);"                       >> $@
	@echo "end Alog.Version;"                       >> $@

clean:
	@rm -f alog.specs
	@rm -rf obj/*
	@rm -rf lib/*

distclean: clean
	@rm -rf obj
	@rm -rf lib
	@rm -rf $(APIDOCDIR)

dist: distclean $(SOURCEDIR)/alog-version.ads docs
	@echo -n "Creating release tarball '$(ALOG)' ... "
	@mkdir -p $(DISTDIR)
	@cp -R * $(DISTDIR)
	@tar -C $(TMPDIR) -cjf $(TARBALL) $(ALOG)
	@rm -rf $(DISTDIR)
	@echo "DONE"

install: install_lib

install_lib: build_lib
	@mkdir -p $(PREFIX)/include/alog
	@mkdir -p $(PREFIX)/lib/alog
	$(INSTALL) -m 644 $(SOURCEDIR)/* $(PREFIX)/include/alog
	$(INSTALL) -m 444 $(ALI_FILES) $(PREFIX)/lib/alog
	$(INSTALL) -m 444 lib/$(SO_LIBRARY) $(PREFIX)/lib/alog
	@cd $(PREFIX)/lib/alog && \
	ln -sf $(SO_LIBRARY) libalog.so && \
	ln -sf $(SO_LIBRARY) libalog.so.$(MAJOR)

docs: prepare
	@echo -n "Creating Alog API doc for version $(VERSION)  ... "
	@mkdir -p $(APIDOCDIR)
	@ls $(SOURCEDIR)/*.ads > alog.specs
	@adabrowse -c config/adabrowse.cfg -q -p -t -i -I src/ -f@alog.specs \
		-o $(APIDOCDIR)/
	@echo "DONE"

cov: prepare
	@gnatmake -p -Palog_tests -XALOG_BUILD="$(BUILD_TYPE)" -XCOVERAGE="True"
	@obj/cov/runner_$(BUILD_TYPE)
	@lcov -c -d obj/cov/ -o obj/cov/alog_tmp.info
	@lcov -e obj/cov/alog_tmp.info "$(PWD)/src/*.adb" -o obj/cov/alog.info
	@genhtml obj/cov/alog.info -o $(COVDIR)

.PHONY: cov dist tests
