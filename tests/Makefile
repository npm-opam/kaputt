#
# This file is part of Kaputt.
# Copyright (C) 2008-2012 Xavier Clerc.
#
# Kaputt is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# Kaputt is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

include ../Makefile.config

default: FORCE
	@echo 'Available targets:'
	@echo '  all              runs all tests'
	@echo '  one NAME=dir     runs the test whose name is passed'
	@echo '  clean            deletes log and result files'
	@echo '  log              print log for last execution'

all: clean
	@for file in *; do \
	  if [ -d $$file ]; then \
	    $(MAKE) one NAME=$$file; \
	  fi \
	done 2>&1 | tee _log
	@echo ''
	@echo "`grep '\*\*\*' _log | wc -l` error(s) in `grep '^Running tests for' _log | wc -l` test(s)"

one: FORCE
	@if [ -z $(NAME) ]; then echo "*** value for 'NAME' is missing"; exit 1; fi
	@echo "Running tests for '$(NAME)' (bytecode)..."
	@cd $(NAME) && $(MAKE) PATH_OCAML_BIN=$(PATH_OCAML_PREFIX)/bin COMPILER=ocamlc EXECUTABLE=bytecode RUN=./ LIB_EXT=cma MDL_EXT=cmo && cd ..
	@echo "Running tests for '$(NAME)' (native)..."
	@cd $(NAME) && $(MAKE) PATH_OCAML_BIN=$(PATH_OCAML_PREFIX)/bin COMPILER=ocamlopt EXECUTABLE=native RUN=./ LIB_EXT=cmxa MDL_EXT=cmx && cd ..
	@if [ -x $(PATH_OCAML_PREFIX)/bin/ocamljava ]; then \
	  echo "Running tests for '$(NAME)' (java)..."; \
	  cd $(NAME) && $(MAKE) PATH_OCAML_BIN=$(PATH_OCAML_PREFIX)/bin COMPILER=ocamljava EXECUTABLE=java RUN=./ LIB_EXT=cmja MDL_EXT=cmj && cd ..; \
	fi

clean: FORCE
	@rm -f _log
	@rm -f */*.result */result */*.cm*

log: FORCE
	@cat _log
	@echo ''
	@echo "`grep '^\*\*\*' _log | wc -l` error(s) in `grep '^Running tests for' _log | wc -l` test(s)"

FORCE:
