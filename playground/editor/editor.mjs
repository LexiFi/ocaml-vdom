import {EditorView, basicSetup} from "codemirror"
import {javascript} from "@codemirror/lang-javascript"

window.editor = new EditorView({
  extensions: [basicSetup, javascript()],
  parent: document.getElementById('editor')
})
