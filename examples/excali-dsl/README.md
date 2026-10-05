# Excali DSL examples

Open any `.excalidsl` file below in Emacs with excali-mode installed. Press
`C-c C-c` to preview or `C-c C-e` to export an editable `.excalidraw` scene.
Build with `make` first. See the [language reference](../../docs/excali-dsl.md).

- `application.excalidsl`: dotted containment, cross-container relative placement,
  and explicit arrow attachment sides.
- `styled-application.excalidsl`: defaults, reusable styles, inline overrides,
  and compact/multiline style blocks.
- `review-flow.excalidsl`: a diamond decision and branching with elbow arrows.

These files are exercised by `test/excali-dsl-test.el`. Tests assert measured
geometry relationships, rather than font-dependent pixel snapshots.
