# This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2016 by LexiFi.

PACKAGES=-package gen_js_api
OCAMLFLAGS=-w -40 -g -bin-annot

.PHONY: all demo clean doc install uninstall

all:
	ocamlfind gen_js_api/gen_js_api lib/js_browser.mli
	cd lib && ocamlfind ocamlc $(OCAMLFLAGS) $(PACKAGES) -c \
	    js_browser.mli js_browser.ml \
            vdom.mli vdom.ml \
            vdom_blit.mli vdom_blit.ml

	cd lib && ocamlfind ocamlc -a -o vdom.cma js_browser.cmo vdom.cmo vdom_blit.cmo

demo:
	cd examples && ocamlfind ocamlc $(OCAMLFLAGS) $(PACKAGES) -I ../lib -no-check-prims -linkpkg -o demo.exe vdom.cma vdom_ui.mli vdom_ui.ml demo.ml
	cd examples && js_of_ocaml +gen_js_api/ojs_runtime.js -o demo.js demo.exe
	cd examples && ocamlfind ocamlc $(OCAMLFLAGS) $(PACKAGES) -I ../lib -no-check-prims -linkpkg -o counters.exe vdom.cma counters.ml
	cd examples && js_of_ocaml +gen_js_api/ojs_runtime.js -o counters.js counters.exe
	cd examples && ocamlfind ocamlc $(OCAMLFLAGS) $(PACKAGES) -I ../lib -no-check-prims -linkpkg -o svg.exe vdom.cma svg.ml
	cd examples && js_of_ocaml +gen_js_api/ojs_runtime.js -o svg.js svg.exe
	cd examples && ocamlfind ocamlc $(OCAMLFLAGS) $(PACKAGES) -I ../lib -no-check-prims -linkpkg -o date.exe vdom.cma date.ml
	cd examples && js_of_ocaml +gen_js_api/ojs_runtime.js -o date.js date.exe
	cd examples && ocamlfind ocamlc $(OCAMLFLAGS) $(PACKAGES) -I ../lib -no-check-prims -linkpkg -o file.exe vdom.cma file.ml
	cd examples && js_of_ocaml +gen_js_api/ojs_runtime.js -o file.js file.exe

clean:
	cd lib && rm -f rm -rf *~ *.cm* js_browser.ml
	cd examples && rm -f rm -rf *~ *.cm* demo.exe
	rm -rf doc

doc:
	rm -rf doc
	mkdir doc
	ocamlfind ocamldoc $(PACKAGES) -d doc -html -I lib lib/js_browser.mli lib/vdom.mli lib/vdom_blit.mli


INSTALL = \
  META \
  lib/vdom.cma \
  lib/vdom.cmi lib/vdom.mli lib/vdom.cmti \
  lib/js_browser.cmi lib/js_browser.mli lib/js_browser.cmti \
  lib/vdom_blit.cmi lib/vdom_blit.mli lib/vdom_blit.cmti

install:
	ocamlfind install ocaml-vdom $(INSTALL)

uninstall:
	ocamlfind remove ocaml-vdom
