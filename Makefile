# This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2016 by LexiFi.

PACKAGES=-package gen_js_api
OCAMLFLAGS=-w -40 -g -bin-annot

.PHONY: all demo clean doc install uninstall

all:
	ocamlfind gen_js_api/gen_js_api js_browser.mli
	ocamlfind ocamlc $(OCAMLFLAGS) $(PACKAGES) -c \
	    js_browser.mli js_browser.ml \
            vdom.mli vdom.ml \
            vdom_blit.mli vdom_blit.ml
	ocamlfind ocamlc -a -o vdom.cma js_browser.cmo vdom.cmo vdom_blit.cmo

demo:
	ocamlfind ocamlc $(OCAMLFLAGS) $(PACKAGES) -no-check-prims -linkpkg -o demo.exe vdom.cma demo.ml
	js_of_ocaml +gen_js_api/ojs_runtime.js -o demo.js demo.exe


clean:
	rm -rf *~ *.cm* js_browser.ml demo.exe demo.js doc

doc:
	rm -rf doc
	mkdir doc
	ocamlfind ocamldoc $(PACKAGES) -d doc -html js_browser.mli vdom.mli vdom_blit.mli


INSTALL = \
  META \
  vdom.cma \
  vdom.cmi vdom.mli vdom.cmti \
  js_browser.cmi js_browser.mli js_browser.cmti \
  vdom_blit.cmi vdom_blit.mli vdom_blit.cmti

install:
	ocamlfind install vdom $(INSTALL)

uninstall:
	ocamlfind remove vdom
