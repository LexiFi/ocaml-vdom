Next release
============

0.3
===

- `Vdom.on`: Custom browser event handlers with Elm-style JavaScript decoders
- Fragments: virtual nodes not associated to a concrete DOM
- Option to propagate browser events occurring inside custom elements to the VDOM
- `Vdom.to_html`: Native conversion of the VDOM to HTML
- `Vdom_blit.Cmd.after_redraw`: register a callback for after the next redraw in a command handler
- Numerous additions to `Js_browser` including support for the JavaScript modules `Base64`, `Blob`, `ClassList`, `FetchResponse`, `Navigator`, `TextDecoder`

Warning: this version is not fully retro-compatible with the previous one. Existing code should be adapted slightly.

0.2
===

- GPR#14: delay rendering (view + DOM updates) with requestAnimationFrame
- GPR#15: click/dblclick handlers take a mouse_event argument (API change)
- GPR#16: improve support for checkboxes
- GPR#18: propagate DOM events upwards until finding a handler
- GPR#21: bindings for WebSockets (contributed by Levi Roth)
- GPR#22: Add window.inner{Width,Height} (contributed by 'copy')
- GPR#23: Add KeyboardEvent.{code,key} (contributed by 'copy')
- GPR#24: Make the vdom type covariant in the 'msg parameter (contributed by 'copy')
- GPR#25: Add MouseEvent.{movementX,movementY} (contributed by 'copy')
- GRP#30: Port build to dune + adds travis support
- GRP#32: Disposing custom elements

0.1
===

 - GPR#5: double click handler (contributed by Stéphane Legrand)
 - GPR#10: bindings for Date (contributed by Philippe Veber)
 - GPR#8: binding for windows.location (contributed by Philippe Veber)

<!-- Local Variables:  -->
<!-- coding: utf-8     -->
<!-- End:              -->
