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

COMPILE_FLAGS=-I ../../_build unix.$(LIB_EXT) bigarray.$(LIB_EXT) nums.$(LIB_EXT) kaputt.$(LIB_EXT) $(FLAGS)
PP=../../_build/src/syntax/kaputt_pp.byte
COMPILED_FILES=*.cm* *.o *.jo

default: clean compile run

clean::
	@rm -f $(COMPILED_FILES)

compile::
	@$(PATH_OCAML_BIN)/$(COMPILER) $(COMPILE_FLAGS) -pp '$(PP) on camlp4o' -o with-$(EXECUTABLE) source.ml
	@$(PATH_OCAML_BIN)/$(COMPILER) $(COMPILE_FLAGS) -pp '$(PP) off camlp4o' -o without-$(EXECUTABLE) source.ml

run::
	@$(RUN)with-$(EXECUTABLE) > result-with
	@$(RUN)without-$(EXECUTABLE) > result-without
	@diff -q result-with reference-with
	@diff -q result-without reference-without
	@rm -f $(COMPILED_FILES) with-$(EXECUTABLE) without-$(EXECUTABLE) result*
