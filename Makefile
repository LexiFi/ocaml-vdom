# This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2016 by LexiFi.

.PHONY: all examples clean install uninstall doc

all:
	dune build @install @DEFAULT

examples:
	dune build @examples/DEFAULT

doc:
	dune build @doc

clean:
	dune clean

PREFIX := $$(opam config var prefix)

install:
	opam-installer --prefix $(PREFIX) ocaml-vdom.install

uninstall:
	opam-installer -u --prefix $(PREFIX) ocaml-vdom.install
