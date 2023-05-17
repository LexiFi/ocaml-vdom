import {EditorView, basicSetup} from "codemirror"
import {oneDark} from "@codemirror/theme-one-dark"
import {StreamLanguage} from "@codemirror/language"
import {oCaml} from "@codemirror/legacy-modes/mode/mllike"

window.editor = new EditorView({
  extensions: [basicSetup, oneDark, StreamLanguage.define(oCaml)],
  parent: document.getElementById('editor')
})
