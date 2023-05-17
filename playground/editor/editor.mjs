import {EditorView, basicSetup} from 'codemirror'
import {oneDark} from '@codemirror/theme-one-dark'
import {StreamLanguage} from '@codemirror/language'
import {oCaml} from '@codemirror/legacy-modes/mode/mllike'
import Split from 'split.js'

Split(['#left', '#right'], {
  sizes: [50, 50],
  cursor: 'ew-resize'
});

window.editor = new EditorView({
  extensions: [
    basicSetup,
    oneDark,
    StreamLanguage.define(oCaml),
    EditorView.lineWrapping
  ],
  lineWrapping: true,
  parent: document.getElementById('editor')
})
