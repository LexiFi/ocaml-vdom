.PHONY: bundle
bundle: editor.mjs
	npm i rollup @rollup/plugin-node-resolve codemirror @codemirror/lang-javascript
	node_modules/.bin/rollup editor.mjs -f iife -o editor.bundle.js -p @rollup/plugin-node-resolve
	cp editor.bundle.js ../editor.js
