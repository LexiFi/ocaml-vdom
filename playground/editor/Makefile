DEPS = \
  rollup \
  codemirror \
  split.js \
  @rollup/plugin-node-resolve \
  @codemirror/theme-one-dark \
  @codemirror/language \
  @codemirror/legacy-modes

.PHONY: bundle
bundle:
	npm i $(DEPS)
	node_modules/.bin/rollup editor.mjs -f iife -o editor.js -p @rollup/plugin-node-resolve
