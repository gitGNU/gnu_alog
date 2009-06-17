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
TARGET ?= "base"

MAJOR = 0
MINOR = 3
VERSION = $(MAJOR).$(MINOR)
ALOG = libalog-$(VERSION)
SO_LIBRARY = libalog.so.$(VERSION)

SOURCEDIR = src
OBJECTDIR = obj/$(TARGET)
LIBDIR = lib/$(TARGET)
APIDOCDIR = doc
COVDIR = cov/$(TARGET)
ALI_FILES = lib/$(TARGET)/*.ali

TMPDIR = /tmp
DISTDIR = $(TMPDIR)/$(ALOG)
TARBALL = $(ALOG).tar.bz2
PWD = `pwd`

all: build_lib

tests: build_tests
	@$(OBJECTDIR)/runner_$(TARGET)

build_lib: prepare
	@gnatmake -Palog_$(TARGET) -XALOG_VERSION="$(VERSION)" -XALOG_BUILD="release"

build_tests: prepare
	@gnatmake -Palog_$(TARGET) -XALOG_BUILD="tests"

prepare: $(SOURCEDIR)/alog-version.ads
	@mkdir -p $(OBJECTDIR)/lib $(LIBDIR) $(COVDIR)

$(SOURCEDIR)/alog-version.ads:
	@echo "package Alog.Version is"                 > $@
	@echo "   Version_Number : constant Float  :="  >> $@
	@echo "      $(VERSION);"                       >> $@
	@echo "end Alog.Version;"                       >> $@

clean:
	@rm -f alog.specs
	@rm -rf $(OBJECTDIR)/lib/*
	@rm -rf $(OBJECTDIR)/*
	@rm -rf $(LIBDIR)/*
	@rm -rf $(COVDIR)/*

distclean: clean
	@rm -rf obj
	@rm -rf lib
	@rm -rf cov
	@rm -rf $(APIDOCDIR)
	@rm -f $(SOURCEDIR)/alog-version.ads

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
	$(INSTALL) -m 444 $(LIBDIR)/$(SO_LIBRARY) $(PREFIX)/lib/alog
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
	@gnatmake -p -Palog_$(TARGET) -XALOG_BUILD="coverage"
	@$(OBJECTDIR)/cov/runner_$(TARGET)
	@lcov -c -d $(OBJECTDIR)/cov/ -o $(OBJECTDIR)/cov/alog_tmp.info
	@lcov -e $(OBJECTDIR)/cov/alog_tmp.info "$(PWD)/src/*.adb" -o $(OBJECTDIR)/cov/alog.info
	@genhtml $(OBJECTDIR)/cov/alog.info -o $(COVDIR)

.PHONY: cov dist tests
