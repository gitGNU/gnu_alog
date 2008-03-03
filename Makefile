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

PREFIX?=$(HOME)/libraries
INSTALL=install

SOURCES=src/*
ALI_FILES=lib/*.ali
SO_LIBRARY=libalog.so.0.1

all:
	@mkdir -p lib obj
	@gnatmake -Palog
	@gnatmake -Palog_lib

clean:
	@rm -rf obj/*
	@rm -rf lib/*

distclean:
	@rm -rf obj
	@rm -rf lib

control:
	@rm -f obj/*.adt objects/*.ali
	cd obj && adactl -f ../rules/alog.aru ../src/*.ad[bs]

tests: all
	@obj/runner

install: install_lib

install_lib:
	@mkdir -p $(PREFIX)/include/alog
	@mkdir -p $(PREFIX)/lib/alog
	$(INSTALL) -m 644 $(SOURCES) $(PREFIX)/include/alog
	$(INSTALL) -m 444 $(ALI_FILES) $(PREFIX)/lib/alog
	$(INSTALL) -m 444 lib/$(SO_LIBRARY) $(PREFIX)/lib/alog
	@ln -s $(PREFIX)/lib/alog/$(SO_LIBRARY) $(PREFIX)/lib/alog/libalog.so
