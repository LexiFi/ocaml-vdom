# This file is part of the ocaml-vdom package, released under the terms of an MIT-like license.
# See the attached LICENSE file.
# Copyright 2016 by LexiFi.

.PHONY: all examples clean install uninstall doc

all:
	dune build @all

examples:
	dune build @examples/all

doc:
	dune build @doc

clean:
	dune clean
