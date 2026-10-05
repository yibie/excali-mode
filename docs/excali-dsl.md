# Excali DSL (.excalidsl) — v0.1 language reference

**Status: implemented.** Open `.excalidsl` in `excali-dsl-mode`; `C-c C-c`
previews the scene and `C-c C-e` exports `.excalidraw`. The former `.edsl`
grammar is no longer accepted. Build the native module and Lisp with `make`
before use; batch export requires the same module and fonts, but no GUI.

Excali DSL is a text authoring language for excali-mode. It borrows relative
placement and dotted containment from [reladraw](https://github.com/reladraw/reladraw),
but defines its own syntax and semantics. It does not claim reladraw compatibility.
The output remains an editable `.excalidraw` scene, not a flattened picture.

## 1. Design contract

- Describe structure and placement in readable statements.
- Put appearance in an explicit, trailing `{ ... }` block.
- Dotted identifiers express containment; braces never contain nodes.
- Constraints are promises: report contradictions instead of ignoring them.
- Resolve styles and measure the actual fonts before calculating positions.
- Keep the language independent of Org. `ob-excali` is an optional adapter.

## 2. A complete diagram

```text
node system "Application"
node system.ui "Interface"
node system.api "API" below system.ui

node database "Database" right of system level with system.api

edge system.ui -> system.api "request" from: bottom to: top
edge system.api -> database "query" from: right to: left
```

This is the baseline example and must be accepted without changes. The database
sits outside the application container, horizontally beyond its boundary and
vertically centered on the API node. Relationships, not edge direction, decide
node positions.

See [the runnable examples](../examples/excali-dsl/README.md).

## 3. Lexical rules and statement boundaries

A file is UTF-8. Top-level statements are `node`, `edge`, `style`, and `default`.
They begin on a new line at the beginning of the line (column 1 in diagnostics). Blank lines are allowed between them.

An indented nonempty line continues the previous statement; indentation does
not establish scope or containment. A blank line ends an unbraced statement.
Inside braces, indentation is cosmetic. A closing brace completes its statement:
no placement or other attribute may follow it on the same line.

`//` starts a line comment outside strings. `#` is not a comment marker. Color
literals must be quoted, for example `fill: "#dbeafe"`.

Identifiers consist of segments matching `[A-Za-z_][A-Za-z0-9_-]*`, separated by
periods. Style names are single segments. Node and style names have separate
namespaces. Names are case-sensitive. All node references use their full name;
there is no implicit short-name lookup inside a container.

Labels and string values use double quotes. Supported escapes are `\"`, `\\`,
`\n`, and `\t`; unknown escapes are errors. Unicode is allowed in strings. A slash
in a label is literal, not a line break. Literal multiline strings are deferred.

## 4. Nodes and containment

```text
node <id> ["label"] [placement ...] [style-block]
```

A missing label uses the identifier's last segment. `""` means an intentionally
empty label. Every node must be declared explicitly and exactly once. Edges do
not implicitly create nodes.

`node system.api` makes `system.api` a child of `system`. A parent must be declared
before its children. Other node references may point forward and are resolved
after parsing the entire file. A node with children is a container automatically.

Containers have a visible boundary and a title band, and expand to enclose their
children with padding. They are not clipping frames: nesting does not hide parts
of children or create an Excalidraw frame implicitly. In v0.1, containers must be
rectangular. Boundary and title share an editing group; children remain independent
elements, not a movable parent/child group. Other container shapes and explicit frame semantics are deferred.

Within a container, the first unplaced child is compacted within its content area. Further
unplaced children stack below the preceding sibling in declaration order. A
child with explicit placement does not also receive that implicit placement.
Styles on a parent never automatically apply to its children.

## 5. Relative placement

The initial vocabulary is deliberately small:

```text
above <id>
below <id>
left of <id>
right of <id>
level with <id>
```

- Directional placement imposes a minimum gap between the measured outer bounds.
- `level with` aligns vertical centers; it does not align top edges or text baselines.
- A single directional placement also centers on the other axis unless another
  explicit placement supplies that axis.
- Multiple directional constraints on one axis require an explicit constraint on
  the other axis when the implicit center would be ambiguous.
- A placement against a container refers to its entire outer bounds, not its title
  or one arbitrarily selected child.
- Except for the one unplaced top-level anchor and implicit child stacking, nodes
  must be connected to the anchored layout by placement/containment constraints.
  A disconnected node is an error; connections alone do not position it.
- Impossible constraints, including positive-gap cycles, are errors with the
  conflicting node names and source locations.

The baseline example deliberately permits cross-container references. These must
be solved together with container sizing; a one-pass coordinate substitution is
not sufficient. A descendant's relationship to its ancestor must not be mistaken
for an ordinary sibling placement if it contradicts containment.

Directional gaps are at least 80 scene units; container padding is 30.
For a directly related pair with an edge label, the gap also accommodates
the measured label extent on that axis plus 24 units. A container's
content starts 45 units plus measured title height below its top. Leaf minimum
size is 120 × 60, expanded for the measured label and shape. Labels are not
hard-wrapped by the DSL; use `\n` for explicit line breaks.

Layout uses a joint linear program with nonnegative box-edge coordinates,
fixed leaf sizes and expandable container sizes. It minimizes the sum of box-edge
coordinates; declaration/index order deterministically breaks remaining ties.
Thus the whole drawing is translated into the positive quadrant, rather than
pinning the first declared node to an arbitrary origin. Explicit gaps,
horizontal-center alignment and diagonal placement are deferred.

## 6. Connections

```text
edge <source> -> <target> ["label"] [from: <side>] [to: <side>] [style-block]
```

`side` is `top`, `right`, `bottom`, or `left`. `from:` and `to:` are connection
attributes outside the style block. Either order is allowed, each at most once.
When omitted, choose attachment sides from the resolved geometry, deterministically.

Each edge produces a bound arrow, with a bound label when text is supplied.
`routing: elbow` must produce an actual bound elbow arrow, not an unbound polyline.
Moving a node in the resulting drawing should retain its arrow connections.

The initial operator is `->`, with one arrowhead at the target. Reverse/bidirectional
operators, edge chains, custom arrowheads, self-loops, and parallel edges between
the same ordered pair are deferred and must produce explicit unsupported-feature
errors rather than misleading overlapping output.

## 7. Style blocks

A style block is optional, belongs to one statement, and always comes last:

```text
node api "API" below ui { fill: "#dbeafe" }
node cache "Cache" below api { style: service; opacity: 60 }
```

Each entry is `key: value`. Entries are separated by a newline or semicolon;
a trailing separator is allowed. Whitespace alone does not separate entries.
Comments do not act as separators. Nested blocks and node declarations inside a
style block are errors. An empty block is allowed.

### Named styles and defaults

```text
default node {
  font: hand
  font-size: 20
}

default edge {
  stroke: "#495057"
  stroke-width: 2
}

style service {
  fill: "#dbeafe"
  fill-style: hachure
}

node api "API" { style: service; fill: "#ffe3e3" }
```

Precedence, lowest to highest:

1. Fixed language defaults.
2. `default node` or `default edge`.
3. The element's named style.
4. Properties written directly on the element.

Declarations apply document-wide, regardless of order. One default declaration
per kind and one declaration per style name are permitted. Nodes and edges may
reference one named style each. Style definitions and default blocks cannot
reference styles in v0.1: no inheritance, multiple styles, selectors, or cascading
from parents. Duplicate properties in a block are errors, not last-write-wins.
A local property wins over a named style regardless of where `style:` appears.

### Initial property vocabulary

| Property | Values | Applies to |
|---|---|---|
| `style` | A declared style name | Node/edge blocks only |
| `shape` | `rectangle`, `ellipse`, `diamond` | Leaf nodes; containers require `rectangle` |
| `stroke` | Quoted `#RGB` or `#RRGGBB` | Nodes and edges |
| `fill` | Quoted `#RGB` or `#RRGGBB`, or `none` | Nodes |
| `stroke-width` | Positive finite number, in scene units | Nodes and edges |
| `stroke-style` | `solid`, `dashed`, `dotted` | Nodes and edges |
| `fill-style` | `solid`, `hachure`, `cross-hatch`, `zigzag` | Nodes |
| `roughness` | `0`, `1`, `2` | Nodes and edges |
| `roundness` | `sharp`, `round` | Rectangles and diamonds |
| `opacity` | Number from 0 through 100 | Nodes and edges |
| `font` | `hand`, `sans`, `mono` | Element labels |
| `font-size` | Positive finite number, in scene units | Element labels |
| `text-color` | Quoted `#RGB` or `#RRGGBB` | Element labels |
| `text-align` | `left`, `center`, `right` | Lines within an element label |
| `routing` | `straight`, `elbow` | Edges |

`stroke` does not recolor a label; use `text-color`. Node opacity applies to its
boundary and label, not to contained nodes. `text-align` does not position the
node itself. `fill: none` means transparent fill. v0.1 does not promise arbitrary
CSS colors or host-dependent color-name lookup.

Known properties incompatible with the target are errors, including when they
come from a named style. For example, applying a style containing `fill` to an
edge is an error. Defaults are also type-checked. Unknown keys, enum values, style
names, non-finite numbers, and out-of-range values are errors.

Defaults must be language-owned, not inherited from the active canvas or its last
selected tool. The defaults are a transparent rectangular node, dark stroke
and text, stroke width 2, solid fill/line patterns, roughness 1, rounded rectangle
corners, opacity 100, hand font at 20, and straight edges. Container titles are
left-aligned; leaf and edge labels are centered unless explicitly overridden.
Hand/sans/mono map to excali's hand-drawn/normal/code font categories. Missing-font
fallbacks are reported in Messages for the primary Latin font; use
`M-x excali-font-report` to inspect Latin/CJK fallback details. Measurements use
the actual native font backend.

## 8. Diagnostics and reproducibility

Parser diagnostics include line, column, and the relevant identifier/property.
Interactive commands prefix the source buffer name. Lisp callers receive
`excali-dsl-error` data `(LINE MESSAGE COLUMN)`. Constraint errors list participating
node names and declaration lines; they do not yet isolate a minimal conflict set. Unsupported constructs are never silently
ignored or interpreted as the old language.

Given the same source, renderer, and installed fonts, geometry and hand-drawn seeds
must be repeatable. Scene IDs and serialization timestamps need not be byte-identical.
Changing a style or label triggers measurement before layout. A failed parse or
layout must not overwrite a previous generated file; export should replace the
destination only after successful generation.

The parser must not evaluate Lisp, invoke a shell, fetch URLs, or read external
assets as a side effect of a declaration. Image imports and includes are deferred.

## 9. Files, Org, and manual editing

- The new extension is `.excalidsl`; the editing mode remains `excali-dsl-mode`.
- `ob-excali` provides Org source blocks named `excali`, with file results in
  SVG, PNG or `.excalidraw` format. See the [README setup](../README.org) and
  [Org example](../examples/org-babel/application.org).
- The source compiles to editable `.excalidraw` elements; PNG/SVG are exports.
- The DSL remains authoritative for generated diagrams. Canvas edits do not
  automatically rewrite source, and regeneration replaces those edits. Save an
  independent scene when retaining manual changes.
- No new grammar is added to `.edsl`, and there is no silent syntax autodetection.
  The intended release replaces old-language support. Any migration aid is a
  separate, explicit conversion tool, not a permanent compatibility layer.

## 10. Regression coverage and future work

1. Accept every example in `examples/excali-dsl/`, including the unchanged baseline.
2. Distinguish continuation indentation from dotted containment.
3. Handle quoted comment markers, escapes, Unicode labels, and style separators.
4. Verify style precedence and declaration-order independence.
5. Reject duplicates, unresolved names, incompatible properties, and nested blocks.
6. Check direction gaps and alignment against actual rendered bounding boxes.
7. Resize a container after a child label changes; preserve external constraints.
8. Report contradictory and disconnected layouts without replacing output files.
9. Produce bound arrows/labels and retain connections after manual node movement.
10. Repeat rendering without geometry/roughness jitter or active-buffer style leakage.
11. Export without opening an interactive canvas; verify batch/headless requirements.
12. Org integration tests cover execution confirmation, relative paths, result
    links, edit buffers, and source-block error locations.

### Current limits

- No source/scene round-trip, image nodes, includes, style inheritance,
  self-loops, parallel edges, or legacy `.edsl` parsing.
- Straight routing joins the selected ports directly, without obstacle avoidance.
  Elbow routing uses excali-mode's native router. Neither mode promises automatic
  label/edge collision removal; placement may need adjustment.
- Unconstrained overlaps are not automatically separated or diagnosed. Explicit
  placement and containment constraints are always enforced.
- The dense solver is intended for small/medium authored diagrams, not large graph
  visualization. Text metrics can differ across machines with different fonts.
