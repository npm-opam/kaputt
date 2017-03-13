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

include Makefile.config

# PATHS

PATH_BASE=`pwd`
PATH_BUILD=$(PATH_BASE)/_build
PATH_OCAMLDOC=$(PATH_BASE)/ocamldoc
PATH_SRC=$(PATH_BASE)/src
PATH_TESTS=$(PATH_BASE)/tests
PATH_INSTALL=$(PATH_OCAML_PREFIX)/lib/ocaml/kaputt


# DEFINITIONS

PROJECT_NAME=kaputt
OCAMLBUILD=$(PATH_OCAML_PREFIX)/bin/ocamlbuild
OCAMLBUILD_FLAGS=-classic-display -no-links
MODULES_ODOCL=$(PROJECT_NAME).odocl
MODULES_MLPACK=$(PROJECT_NAME).mlpack


# TARGETS

default:
	@echo "available targets:"
	@echo "  all         compiles all files"
	@echo "  doc         generates ocamldoc documentations"
	@echo "  tests       runs tests"
	@echo "  clean       deletes all produced files (excluding documentation)"
	@echo "  veryclean   deletes all produced files (including documentation)"
	@echo "  install     copies executable and library files"
	@echo "  generate    generates files needed for build"

all: generate
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) $(PROJECT_NAME).otarget
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) $(PROJECT_NAME)_pp.byte
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) $(PROJECT_NAME)Bigarray.cmo
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) $(PROJECT_NAME)Nums.cmo
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) $(PROJECT_NAME)Bigarray.cmx
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) $(PROJECT_NAME)Nums.cmx

doc: FORCE
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) $(PROJECT_NAME).docdir/index.html
	cp $(PATH_BUILD)/$(PROJECT_NAME).docdir/*.html $(PATH_BUILD)/$(PROJECT_NAME).docdir/*.css $(PATH_OCAMLDOC)

tests: FORCE
	test -f $(PATH_TESTS)/Makefile && (cd $(PATH_TESTS) && $(MAKE) $(MAKE_QUIET) all && cd ..) || true

clean: FORCE
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) -clean
	test -f $(PATH_TESTS)/Makefile && (cd $(PATH_TESTS) && $(MAKE) $(MAKE_QUIET) clean && cd ..) || true
	rm -f $(MODULES_ODOCL) $(MODULES_MLPACK) $(PROJECT_NAME).itarget

veryclean: clean
	rm -f $(PATH_OCAMLDOC)/*.html $(PATH_OCAMLDOC)/*.css

install: FORCE
	if [ -x "$(PATH_OCAMLFIND)" ]; then \
	  $(PATH_OCAMLFIND) query $(PROJECT_NAME) && $(PATH_OCAMLFIND) remove $(PROJECT_NAME) || true; \
	  $(PATH_OCAMLFIND) install $(PROJECT_NAME) META -optional \
	    $(PATH_BUILD)/src/syntax/$(PROJECT_NAME)_pp.byte \
	    $(PATH_BUILD)/src/extensions/$(PROJECT_NAME)Bigarray.cm* \
	    $(PATH_BUILD)/src/extensions/$(PROJECT_NAME)Bigarray.o \
	    $(PATH_BUILD)/src/extensions/$(PROJECT_NAME)Bigarray.jo \
	    $(PATH_BUILD)/src/extensions/$(PROJECT_NAME)Nums.cm* \
	    $(PATH_BUILD)/src/extensions/$(PROJECT_NAME)Nums.o \
	    $(PATH_BUILD)/src/extensions/$(PROJECT_NAME)Nums.jo \
	    $(PATH_BUILD)/$(PROJECT_NAME).a \
	    $(PATH_BUILD)/$(PROJECT_NAME).o \
	    $(PATH_BUILD)/$(PROJECT_NAME).cma \
	    $(PATH_BUILD)/$(PROJECT_NAME).cmi \
	    $(PATH_BUILD)/$(PROJECT_NAME).cmo \
	    $(PATH_BUILD)/$(PROJECT_NAME).cmx \
	    $(PATH_BUILD)/$(PROJECT_NAME).cmxa \
	    $(PATH_BUILD)/$(PROJECT_NAME).cmja \
	    $(PATH_BUILD)/$(PROJECT_NAME).ja; \
	else \
	  mkdir -p $(PATH_INSTALL); \
	  cp $(PATH_BUILD)/src/syntax/$(PROJECT_NAME)_pp.byte $(PATH_OCAML_PREFIX)/bin; \
	  for ext in cmi cmo cmx o cmj jo; do \
	    test -f $(PATH_BUILD)/src/extensions/$(PROJECT_NAME)Bigarray.$$ext && cp $(PATH_BUILD)/src/extensions/$(PROJECT_NAME)Bigarray.$$ext $(PATH_INSTALL) || true; \
	    test -f $(PATH_BUILD)/src/extensions/$(PROJECT_NAME)Nums.$$ext && cp $(PATH_BUILD)/src/extensions/$(PROJECT_NAME)Nums.$$ext $(PATH_INSTALL) || true; \
	  done; \
	  for ext in a cma cmi cmo cmx cmxa cmja ja; do \
	    test -f $(PATH_BUILD)/$(PROJECT_NAME).$$ext && cp $(PATH_BUILD)/$(PROJECT_NAME).$$ext $(PATH_INSTALL) || true; \
	  done \
	fi

generate: FORCE
	echo '$(PROJECT_NAME).cma' > $(PROJECT_NAME).itarget
	if [ -x "$(PATH_OCAMLOPT)" ]; then \
		echo '$(PROJECT_NAME).cmxa' >> $(PROJECT_NAME).itarget; \
	else \
		(test -x $(PATH_OCAML_PREFIX)/bin/ocamlopt && echo '$(PROJECT_NAME).cmxa' >> $(PROJECT_NAME).itarget) || true; \
	fi
	(test -x $(PATH_OCAML_PREFIX)/bin/ocamljava && echo '$(PROJECT_NAME).cmja' >> $(PROJECT_NAME).itarget) || true

FORCE:
