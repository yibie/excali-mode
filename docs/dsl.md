# The .edsl diagram language in excali

A compact reference of what `excali-dsl.el` reads.  The language is that
of [excalidraw-dsl](https://github.com/tyrchen/excalidraw-dsl); files
written for it work here.  A diagram is laid out automatically (layered,
like dagre) and drawn as ordinary Excalidraw elements: shapes with bound
labels, arrows bound to their shapes.  Hand this page to a program or
code generator that should write diagrams for excali.

## Shape of a file

```
---                         # optional YAML front matter
direction: LR               # TB (default), BT, LR, RL
---
statements...               # one per line, or separated by ;
```

`#` starts a comment.  Ids are letters, digits, `_` and `.`.

## Nodes

```
api                         # label = id
api[API Server]             # label in brackets
api "API Server"            # or quoted
api[API] @service           # component type
api[API] { type: service }  # the same
db[Database] { shape: ellipse; backgroundColor: "#fce4ec"; width: 160 }
```

Attributes: `shape` (`rectangle`, `ellipse`, `diamond`, `cylinder`,
`text`), `backgroundColor` or `fill`, `fillStyle` (`solid`, `hachure`,
`cross-hatch`, `zigzag`), `strokeColor`, `strokeWidth`, `strokeStyle`
(`solid`, `dashed`, `dotted`), `roughness` (0–2), `roundness` (0 for
sharp corners), `opacity` (0–100), `font` (`Excalifont`, `Virgil`,
`Helvetica`, `Cascadia`, `Nunito`, `Comic Shanns`, …), `fontSize`,
`textColor` or `color`, `width`, `height`.  A node is never drawn smaller
than its label.

Nodes an edge mentions but nothing defines are made where first used.

## Edges

```
a -> b                      # arrow
a <-> b                     # both heads
a -- b                      # no heads (also ---)
a ~> b                      # curved arrow
a -> b: label text          # label to the end of the line
a -> b : "quoted label"
a -> b "label"
a -> b {label}
a -> b -> c: flow           # a chain; the label goes on every link
a -> b { strokeStyle: dashed; strokeColor: "#868e96"; endArrowhead: triangle }
a -> b @orthogonal          # elbow arrow; also @curved, @straight
```

Edge attributes: `strokeColor` or `color`, `strokeWidth` or `width`,
`strokeStyle`, `startArrowhead` and `endArrowhead` (`none`, `arrow`,
`triangle`, `dot`, `circle`, `diamond`, `bar`), `routing` (`straight`,
`orthogonal`, `curved`), `fontSize`, `textColor`.  Straight edges bend
around shapes in their way.

```
connection { from: "lb"; to: "api"; style { type: dashed; label: "HTTPS"; color: "#2196f3"; width: 2 } }
connections { from: "lb"; to: ["api1", "api2"]; style { type: arrow; } }
```

`type` is `arrow`, `line`, `dashed` or `dotted`.

## Containers and groups

```
container "Backend" as backend {
  style: { backgroundColor: "#f8f9fa"; strokeStyle: dashed }
  api[API]
  container "Data" { db[DB] }
  api -> db
}
container backend "Backend" { ... }      # older form, same meaning
service "Core Services" { auth; user }   # semantic groups:
                                          # group flow service layer
                                          # component subsystem zone cluster
group team:people { alice; bob }
frontend -> backend.api                   # qualified reference
```

Clusters nest.  Each is laid out on its own and placed as one block, so
clusters never overlap.  A cluster's `key: value` lines style it
(`backgroundColor`, `strokeColor`, `strokeWidth`, `strokeStyle`,
`opacity`, `textColor`, `padding`, `direction`); `layout: horizontal`,
`vertical` or `grid(3)` arranges members that have no edges between
them.  Edges may end at a cluster.

## Component types and templates

```
componentType service {
  shape: rectangle;
  style { fill: "#e3f2fd"; strokeColor: "#1976d2"; strokeWidth: 2; }
}
```

or in the front matter:

```
---
component_types:
  database:
    backgroundColor: "#fce4ec"
    shape: ellipse
templates:
  microservice:
    api: "$name API"
    db: "$name Database"
    edges:
      - api -> db
---
microservice users { name: "User" }       # makes users.api, users.db
users.api -> billing
```

Layer templates:

```
template stack {
  layers {
    "Clients" { components: ["Web", "Mobile"]; layout: horizontal }
    "Services" { components: ["API"] }
  }
  connections { pattern: each-to-next-layer }   # or mesh, star("API"), custom
  layout { direction: top-to-bottom; spacing: { node_spacing: 50; layer_spacing: 120 } }
}
diagram "My System" { type: architecture; template: stack }
```

## Front matter

| key | meaning |
|---|---|
| `direction`, `rankdir`, `layout_options.rankdir` | `TB`, `BT`, `LR`, `RL` (also `top-to-bottom`, …) |
| `nodeSpacing`, `nodesep` | space between nodes of a layer (60) |
| `rankSpacing`, `ranksep` | space between layers (90; more for long edge labels) |
| `theme` | `dark` |
| `font`, `fontSize` | default label font |
| `sketchiness` | default roughness, 0–2 (1) |
| `stroke_width` | default stroke width (2) |
| `background_color` | canvas color |
| `routing` | default edge routing (`straight`) |
| `layout` | `manual` places nodes at their `x`, `y`; others are laid out in layers |

## Not supported

The force and ELK layouts (the layered layout is used instead), the ML
layout, and template layouts other than per-layer `horizontal`,
`vertical` and `grid(N)`.  Errors report the line they occur on.
