# Excalidraw reference spec for the excali Emacs port

- **Date compiled:** 2026-09-27
- **Describes:** `excalidraw/excalidraw` **`master` as fetched on 2026-09-27**. Sourcegraph reported commit `438d898` for App.tsx lookups. Source files were read through raw.githubusercontent.com, the GitHub contents API and the Sourcegraph search API.
- **Scope:** the core editor, images, frames, export and the element library. Out of scope: collaboration, AI/magic frames, embeddables/iframes (preserve only; see 1b.11), Mermaid, and the app shell.
- **Conventions:**
  - Every fact cites its upstream path (relative to the repo root) and, where possible, the function it comes from.
  - "UNVERIFIED" marks anything not confirmed against source. Re-check those items before relying on them.
  - The fetch tool paraphrased comments, so identifiers and numbers are exact but comment wording may not be.
  - App.tsx line numbers (≈) are approximate.

## 0. Read-me-first: changes since older Excalidraw and conflicts resolved

These are the points most likely to surprise anyone who knows the 2023–2024 codebase. Each one was cross-checked between at least two research passes.

1. **Dark mode is a per-colour transform, not a CSS filter over the canvas.** Every colour passes through `applyDarkModeFilter` (invert 93%, then hue-rotate 180°) when drawn (`packages/common/src/colors.ts`). Raster images are drawn untouched; SVG images get `DARK_THEME_FILTER`. See 2a.13 and 2c.1.
2. **Only the FixedPointBinding format exists in `types.ts`:** `{elementId, fixedPoint:[x,y], mode:"inside"|"orbit"|"skip"}`. Legacy `{elementId, focus, gap}` exists only as input to `repairBinding` in `packages/excalidraw/data/restore.ts`. That function discards `focus` and `gap` and rebuilds the binding from the current endpoint position. See 1b.3.
3. **The default `fillStyle` is `"solid"`**, per `DEFAULT_ELEMENT_PROPS` in `packages/common/src/constants.ts`.
4. **Stroke widths** (`packages/common/src/constants.ts`, verified directly):
   - `STROKE_WIDTH = {thin:1, medium:2, bold:4, extraBold:8}`.
   - `FREEDRAW_STROKE_WIDTH = {thin:0.5, medium:1, bold:2, extraBold:4}`.
   - appState stores a key, `currentItemStrokeWidthKey` (default `"medium"`), not a number. A legacy numeric `currentItemStrokeWidth` is mapped back to a key on restore.
5. **New element type `stickynote`**, with an extra field `baseHeight` (`packages/element/src/stickyNote.ts`).
   - It is a text container, bindable, and drawn with plain canvas paths, not roughjs.
   - `autoshape` and `bucketfill` are **tools only, not element types** (see `TOOL_TYPE` in constants.ts):
     - `autoshape` recognises freedraw strokes into shapes (`convertToShape.ts`).
     - `bucketfill` creates an ordinary `line` element with `polygon:true` (`bucketFill.ts`).
6. **New text fields.** `baseFontSize` is for sticky-note font fitting. Optional `labelPosition` (0..1) sets where a label sits along its arrow.
7. **New font id and fallbacks.** `FONT_FAMILY.Assistant = 10`, and id 4 is unused. Fallback pseudo-ids: Xiaolai 100, sans-serif 998, monospace 999, Segoe UI Emoji 1000.
8. **Arrowheads.** `dot` → `circle` and `crowfoot_*` → `cardinality_*` are renamed on load. There are 14 arrowhead values.
9. **No side resize handles on desktop.** Only corner handles and the rotation handle are drawn. Edges resize from a 4 px (screen) band along the selection border (`resizeTest.ts`, `getOmitSidesForEditorInterface`).
10. **The binding code was rewritten** (`packages/element/src/binding.ts`).
    - Bind distance is `clamp(15/(min(zoom,1)*1.5), 15, 30)`, and the gap is `5 + strokeWidth/2`.
    - Hovering an endpoint over a shape for 700 ms, or holding Alt, gives `"inside"` mode.
    - Ctrl/Cmd inverts binding while held.
    - `COMPLEX_BINDINGS` is false by default, so port the `_simple` paths.
11. **Box selection** follows the `appState.boxSelectionMode` preference (`"contain"` by default, or `"overlap"`), not the drag direction.
12. **Saved appState is tiny.** Only `viewBackgroundColor`, `gridSize`, `gridStep`, `gridModeEnabled` and `lockedMultiSelections` are written to `.excalidraw` files (`cleanAppStateForExport`).
13. **Embedded scene in PNG and SVG.**
    - The payload is zlib deflate stored as a byte string, then (for SVG) base64 encoded.
    - PNG uses a `tEXt` chunk with keyword `application/vnd.excalidraw+json`, inserted before `IEND`.
    - Readers also accept `compressed:false`, so the port can write uncompressed payloads until it has a deflate encoder. See 1b.6 and 1b.7.
14. **Upstream drops unknown element types on load.** The port should keep each element's original JSON and merge its edits into it, so unknown fields and types survive a save.
15. **`groupIds` is ordered innermost → outermost.** A click selects the outermost group.
16. **Deleting a frame releases its children** (`frameId` → null) instead of deleting them.
17. **Tool changes** (`packages/excalidraw/components/Tools.tsx`; `components/shapes.tsx` no longer exists):
    - Image has no letter key; `I` is the eye dropper.
    - `X` is an alias for freedraw.
    - New tools: `N` sticky note, `Shift+X` autoshape, `B` bucket fill.
    - Pressing `A` while the arrow tool is active cycles the arrow type sharp → round → elbow.

Where two research passes disagreed, the value in this section is the one checked against source. The subsections below may keep a pass's own wording.

---

# 1. Data model & file formats

## 1a. Data model: element types, fields, creation defaults

Sources: `packages/element/src/types.ts`, `packages/element/src/newElement.ts`,
`packages/element/src/typeChecks.ts`, `packages/common/src/constants.ts`,
`packages/common/src/colors.ts`, `packages/element/src/stickyNote.ts`.
Read through WebFetch, which summarises what it returns: the field names and types are
reliable, but the spelling of the comments is paraphrased.

### 1a.1 Scalar / enum types (types.ts)

| Type | Definition |
|---|---|
| `FillStyle` | `"hachure" \| "cross-hatch" \| "solid" \| "zigzag"` |
| `StrokeStyle` | `"solid" \| "dashed" \| "dotted"` |
| `StrokeRoundness` | `"round" \| "sharp"` (legacy name, see restore) |
| `RoundnessType` | `ValueOf<typeof ROUNDNESS>` → `1 \| 2 \| 3` |
| `TextAlign` | `"left" \| "center" \| "right"` (TEXT_ALIGN) |
| `VerticalAlign` | `"top" \| "middle" \| "bottom"` (VERTICAL_ALIGN) |
| `FontFamilyValues` | values of `FONT_FAMILY` (1,2,3,5,6,7,8,9,10) |
| `Theme` | `"light" \| "dark"` (THEME) |
| `ChartType` | `"bar" \| "line" \| "radar"` (defined, not used by any element field) |
| `StrokeVariability` | `"variable" \| "constant"` (freedraw; defined locally) |
| `BindMode` | `"inside" \| "orbit" \| "skip"` |
| `FixedPoint` | `[number, number]` (ratio 0.0–1.0 of the bindable's width/height) |
| `FileId` | branded string |
| `GroupId` | string |
| `ExcalidrawElementType` | `ExcalidrawElement["type"]` |

**Element `type` strings accepted** (`isExcalidrawElement`, typeChecks.ts):
`text, diamond, rectangle, stickynote, iframe, embeddable, ellipse, arrow, freedraw, line, frame, magicframe, image, selection`.

### 1a.2 `_ExcalidrawElementBase` (every element has these fields)

| Field | Type | Notes |
|---|---|---|
| `id` | string | `randomId()` (nanoid) |
| `x`, `y` | number | scene coords of the unrotated top-left corner |
| `strokeColor` | string | CSS color, or `"transparent"` |
| `backgroundColor` | string | |
| `fillStyle` | FillStyle | |
| `strokeWidth` | number | |
| `strokeStyle` | StrokeStyle | |
| `roundness` | `null \| { type: RoundnessType; value?: number }` | |
| `roughness` | number | 0/1/2 (architect/artist/cartoonist) |
| `opacity` | number | 0–100 |
| `width`, `height` | number | |
| `angle` | Radians | rotation about the centre |
| `seed` | number | roughjs seed: the same seed gives the same shape |
| `version` | number | incremented on every change |
| `versionNonce` | number | random, regenerated on every change |
| `index` | `FractionalIndex \| null` | fractional z-order key |
| `isDeleted` | boolean | tombstone: deleted elements stay in the array |
| `groupIds` | `readonly GroupId[]` | ordered deepest → shallowest |
| `frameId` | `string \| null` | id of the frame containing the element |
| `boundElements` | `readonly BoundElement[] \| null` | `BoundElement = Readonly<{id; type: "arrow" \| "text"}>` |
| `updated` | number | epoch ms of the last update |
| `created` | `number \| null` | wall-clock creation time, preserved across edits (**new field**) |
| `link` | `string \| null` | |
| `locked` | boolean | |
| `customData?` | `Record<string, any>` | optional; preserve it untouched |

### 1a.3 Per-type fields

| Type string | TS type | Extra fields |
|---|---|---|
| `selection` | ExcalidrawSelectionElement | none (transient, never saved) |
| `rectangle` | ExcalidrawRectangleElement | none |
| `diamond` | ExcalidrawDiamondElement | none |
| `ellipse` | ExcalidrawEllipseElement | none |
| `stickynote` | ExcalidrawStickyNoteElement | `baseHeight: number` (**new element type**) |
| `embeddable` | ExcalidrawEmbeddableElement | none (URL lives in `link`) |
| `iframe` | ExcalidrawIframeElement | `customData?: { generationData?: MagicGenerationData }` |
| `image` | ExcalidrawImageElement | `fileId: FileId \| null`; `status: "pending" \| "saved" \| "error"`; `scale: [number, number]` (−1 flips an axis); `crop: ImageCrop \| null` |
| `frame` | ExcalidrawFrameElement | `name: string \| null` |
| `magicframe` | ExcalidrawMagicFrameElement | `name: string \| null` |
| `text` | ExcalidrawTextElement | see below |
| `line` | ExcalidrawLineElement | linear fields + `polygon: boolean` |
| `arrow` | ExcalidrawArrowElement | linear fields + `elbowed: boolean` |
| arrow with `elbowed: true` | ExcalidrawElbowArrowElement | `elbowed: true`; `fixedSegments: readonly FixedSegment[] \| null`; `startIsSpecial: boolean \| null`; `endIsSpecial: boolean \| null` ("temporarily hide the first/last segment") |
| `freedraw` | ExcalidrawFreeDrawElement | `points: readonly LocalPoint[]`; `pressures: readonly number[]`; `simulatePressure: boolean`; `strokeOptions: Readonly<{ variability: StrokeVariability; streamline: number }>` (**new**) |

`ImageCrop = { x; y; width; height; naturalWidth; naturalHeight }` (all numbers; crop rect in natural-image pixels).

**Text fields** (`ExcalidrawTextElement`):

| Field | Type | Notes |
|---|---|---|
| `fontSize` | number | |
| `fontFamily` | FontFamilyValues | |
| `baseFontSize` | `number \| null` | font size the user picked, used for auto-fit (sticky-note labels) (**new**) |
| `text` | string | the displayed text, with the wrap newlines inserted |
| `originalText` | string | the text before wrapping |
| `textAlign` | TextAlign | |
| `verticalAlign` | VerticalAlign | |
| `containerId` | `ExcalidrawTextContainer["id"] \| null` | |
| `autoResize` | boolean | true = width grows to fit the text; false = wrap at a fixed width |
| `lineHeight` | `number & {_brand:"unitlessLineHeight"}` | unitless ("aligned to W3C"); px = lineHeight × fontSize (`getLineHeightInPx`) |
| `labelPosition?` | `number \| null` | normalised arc-length position of the label along its arrow (**new**; optional) |

**Linear fields** (`ExcalidrawLinearElement`, type `"line" | "arrow"`):

| Field | Type |
|---|---|
| `points` | `readonly LocalPoint[]` (relative to x,y; the first point is normally [0,0]) |
| `startBinding`, `endBinding` | `FixedPointBinding \| null` |
| `startArrowhead`, `endArrowhead` | `Arrowhead \| null` |

`lastCommittedPoint` is **no longer present** in types.ts. Old files may still contain it; preserve or drop it.

`FixedSegment = { start: LocalPoint; end: LocalPoint; index: number }`.

**Bindings.** The only binding type in types.ts is
`FixedPointBinding = { elementId: ExcalidrawBindableElement["id"]; fixedPoint: FixedPoint; mode: BindMode }`.
There is **no `PointBinding` (focus/gap) type** in types.ts any more. The legacy
`{elementId, focus, gap}` shape only survives through migration in restore.ts, covered
in the restore section.

**Arrowhead union.** `arrow, bar, circle, circle_outline, triangle, triangle_outline, diamond, diamond_outline, cardinality_one, cardinality_many, cardinality_one_or_many, cardinality_exactly_one, cardinality_zero_or_one, cardinality_zero_or_many`.
`ArrowheadLegacy`: `dot, crowfoot_one, crowfoot_many, crowfoot_one_or_many`. These are
migrated on load. Probable mapping, based on the old code: dot→circle,
crowfoot_*→cardinality_*. **UNVERIFIED**; check restore.ts.

**Unions** (types.ts):
- `ExcalidrawGenericElement` = Selection | Rectangle | Diamond | Ellipse
- `ExcalidrawElement` = Generic | StickyNote | Text | Linear | Arrow | FreeDraw | Image | Frame | MagicFrame | Iframe | Embeddable
- `ExcalidrawTextContainer` = Rectangle | StickyNote | Diamond | Ellipse | Arrow
- `ExcalidrawBindableElement` = Rectangle | StickyNote | Diamond | Ellipse | Text | Image | Iframe | Embeddable | Frame | MagicFrame
- `ExcalidrawFlowchartNodeElement` = Rectangle | StickyNote | Diamond | Ellipse
- `ExcalidrawFrameLikeElement` = Frame | MagicFrame
- The helper types `NonDeleted<T>`, `Ordered<T>` / `OrderedExcalidrawElement` (index non-null), `ElementsMap`, `SceneElementsMap` and `NonDeletedSceneElementsMap` also exist.

### 1a.4 Type predicates that decide behaviour (typeChecks.ts)

| Predicate | Types |
|---|---|
| `isTextBindableContainer` | rectangle, stickynote, diamond, ellipse, arrow (unlocked unless `includeLocked`) |
| `isBindableElement` | rectangle, stickynote, diamond, ellipse, image, iframe, embeddable, frame, magicframe, and text **without** a containerId (unlocked unless `includeLocked`) |
| `isRectanguloidElement` | rectangle, stickynote, diamond, image, iframe, embeddable, frame, magicframe, uncontained text (i.e. not ellipse) |
| `isFlowchartNodeElement` | rectangle, stickynote, ellipse, diamond |
| `isUsingAdaptiveRadius` | rectangle, embeddable, iframe, image |
| `isUsingProportionalRadius` | line, arrow, diamond, stickynote |
| `getDefaultRoundnessTypeForElement` | PROPORTIONAL (2) for line/arrow/diamond/stickynote; ADAPTIVE (3) for rectangle/embeddable/iframe/image; otherwise null |
| `canApplyRoundnessTypeToElement` | true when the roundness type matches the element's radius family (adaptive vs proportional); exact LEGACY handling **UNVERIFIED** |
| `isLinearElementType` | arrow, line |
| `canBecomePolygon(points)` | `points.length > 3`, or exactly 3 points with first ≠ last |
| `isElbowArrow` | arrow && `elbowed === true`; `isSimpleArrow` = arrow && !elbowed |

Ellipse and freedraw have no roundness family, so their `roundness` is null and ignored.
Frames are always `roundness: null` (FRAME_STYLE).

### 1a.5 Creation defaults (newElement.ts)

**`_newElementBase(type, opts)`**: parameter defaults, then the returned object.

| Field | Default |
|---|---|
| strokeColor | `DEFAULT_ELEMENT_PROPS.strokeColor` = `"#1e1e1e"` |
| backgroundColor | `DEFAULT_ELEMENT_PROPS.backgroundColor` = `"transparent"` |
| fillStyle | `DEFAULT_ELEMENT_PROPS.fillStyle` = `"solid"` |
| strokeWidth | `DEFAULT_ELEMENT_PROPS.strokeWidth` = `STROKE_WIDTH.medium` = 2 |
| strokeStyle | `"solid"` |
| roughness | `ROUGHNESS.artist` = 1 |
| opacity | 100 |
| width / height | 0 / 0 |
| angle | 0 |
| groupIds | `[]` |
| frameId | null |
| index | null |
| roundness | null |
| boundElements | null |
| link | null |
| locked | false |
| id | `rest.id \|\| randomId()` |
| seed | `rest.seed ?? randomInteger()` |
| version | `rest.version \|\| 1` |
| versionNonce | `rest.versionNonce ?? 0` |
| isDeleted | false |
| updated | `getUpdatedTimestamp()` (now, in ms) |
| created | `rest.created === undefined ? timestamp : rest.created` |
| customData | `rest.customData` |

The UI passes the `appState.currentItem*` values as opts. These defaults apply only to
programmatic creation.

Exported constructors: `newElement, newStickyNoteElement, newEmbeddableElement, newIframeElement, newFrameElement, newMagicFrameElement, newTextElement, newFreeDrawElement, newLinearElement, newArrowElement, newImageElement`. Also exported: `normalizeStickyNoteStyle, normalizeStickyNoteGeometry, normalizeStickyNote, getTextAnchorRatios, refreshTextDimensions`.

| Constructor | Fields set beyond the base |
|---|---|
| `newElement` | generic (rectangle/diamond/ellipse/selection); base only |
| `newTextElement` | `fontSize = DEFAULT_FONT_SIZE (20)`, `fontFamily = DEFAULT_FONT_FAMILY (5, Excalifont)`, `textAlign = "left"`, `verticalAlign = "top"`, `containerId = null`, `lineHeight = getLineHeight(fontFamily)`, `autoResize = true`, `baseFontSize = null`, `labelPosition = null`, `originalText = opts.originalText ?? text`. Width and height come from `measureText(text, getFontString({fontFamily,fontSize}), lineHeight)`. Position: `x = opts.x - offsets.x`, `y = opts.y - offsets.y`, where `getTextAnchorRatios` gives x ratio 0/0.5/1 for left/center/right and y ratio 0/0.5/1 for top/middle/bottom, multiplied by the measured width/height. The given (x,y) is therefore the anchor point. |
| `refreshTextDimensions` | Case 1: `maxWidth` is given, autoResize is true, there is no container, and the current width ≤ maxWidth. If the text is wider than maxWidth, it is wrapped at maxWidth and `autoResize` becomes false. Case 2: text in a container, or with autoResize false, wraps at the container's max width or at its own width. Finally `getAdjustedDimensions` recomputes the size and shifts x/y to keep the anchor fixed. |
| `newFreeDrawElement` | `points = []`, `pressures = []`, `simulatePressure` (required argument), `strokeOptions = { variability: "variable", streamline: DEFAULT_STROKE_STREAMLINE (0.5) }` |
| `newLinearElement` | `points = []`, `startBinding = null`, `endBinding = null`, `startArrowhead = null`, `endArrowhead = null`; for lines `polygon = false` |
| `newArrowElement` | `elbowed` comes from opts. If elbowed: `fixedSegments = []`, `startIsSpecial = false`, `endIsSpecial = false`. Arrowheads come from opts (the UI uses currentItemStart/EndArrowhead). Roundness is not forced here; the UI passes null for elbow arrows. |
| `newImageElement` | `status = "pending"`, `fileId = null`, `scale = [1,1]`, `crop = null`; strokeColor is reported as forced to `"transparent"` (**UNVERIFIED**: the summariser said "hardcoded") |
| `newFrameElement` / `newMagicFrameElement` | `name = opts.name \|\| null`. The UI supplies the FRAME_STYLE props (below). |
| `newEmbeddableElement` / `newIframeElement` | base only |
| `newStickyNoteElement` | base, then `normalizeStickyNoteStyle`; `baseHeight = opts.baseHeight ?? height` |

**Sticky-note normalisation** (newElement.ts plus stickyNote.ts):
- Style: `backgroundColor` becomes `DEFAULT_STICKY_NOTE_BG` (`"#ffdf6b"`) if it is null or transparent. `strokeColor` becomes the default stroke if it is null or transparent. `fillStyle = "solid"`.
- Geometry: `width = max(width, STICKY_NOTE_MIN_SIZE=75)`; `baseHeight = max(baseHeight || height || DEFAULT_STICKY_NOTE_SIZE=250, 75)`; `height = max(height, baseHeight)`.

### 1a.6 Constants (packages/common/src/constants.ts, colors.ts)

| Constant | Value |
|---|---|
| `ROUNDNESS` | `{ LEGACY: 1, PROPORTIONAL_RADIUS: 2, ADAPTIVE_RADIUS: 3 }` |
| `DEFAULT_PROPORTIONAL_RADIUS` | 0.25 |
| `DEFAULT_ADAPTIVE_RADIUS` | 32 |
| `DEFAULT_FONT_SIZE` | 20 |
| `DEFAULT_FONT_FAMILY` | `FONT_FAMILY.Excalifont` = 5 |
| `FONT_FAMILY` | Virgil 1, Helvetica 2, Cascadia 3, Excalifont 5, Nunito 6, "Lilita One" 7, "Comic Shanns" 8, "Liberation Sans" 9, **Assistant 10** (4 is unused) |
| `FONT_FAMILY_FALLBACKS` | Xiaolai 100, sans-serif 998, monospace 999, "Segoe UI Emoji" 1000 |
| `FONT_FAMILY_GENERIC_FALLBACKS` | sans-serif 998, monospace 999 |
| `CJK_HAND_DRAWN_FALLBACK_FONT` | `"Xiaolai"` |
| `WINDOWS_EMOJI_FALLBACK_FONT` | `"Segoe UI Emoji"` |
| `DEFAULT_TEXT_ALIGN` / `DEFAULT_VERTICAL_ALIGN` | `"left"` / `"top"` |
| `MIN_FONT_SIZE` | 1 |
| `BOUND_TEXT_PADDING` | 5 |
| `TEXT_AUTOWRAP_THRESHOLD` | 36 (px) |
| `ARROW_LABEL_WIDTH_FRACTION` | 0.7 |
| `ARROW_LABEL_FONT_SIZE_TO_MIN_WIDTH_RATIO` | 11 |
| `DEFAULT_TRANSFORM_HANDLE_SPACING` | 2 |
| `STROKE_WIDTH` | thin 1, medium 2, bold 4, **extraBold 8** |
| `DEFAULT_ELEMENT_STROKE_WIDTH_KEY` | `"medium"` |
| `FREEDRAW_STROKE_WIDTH` | thin 0.5, medium 1, bold 2, extraBold 4 (**freedraw uses a separate, halved scale**) |
| `DEFAULT_STROKE_STREAMLINE` / `DEFAULT_STROKE_STREAMLINE_PRECISE` | 0.5 / 0.2 |
| `ROUGHNESS` | architect 0, artist 1, cartoonist 2 |
| `DEFAULT_ELEMENT_PROPS` | strokeColor `#1e1e1e`, backgroundColor `transparent`, fillStyle `solid`, strokeWidth 2, strokeStyle `solid`, roughness 1, opacity 100, locked false |
| `FRAME_STYLE` | strokeColor `#bbb`, strokeWidth 2, strokeStyle solid, fillStyle solid, roughness 0, roundness null, backgroundColor transparent, radius 8, nameOffsetY 3, nameColorLightTheme `#999999`, nameColorDarkTheme `#7a7a7a`, nameFontSize 14, nameLineHeight 1.25 |
| `ARROW_TYPE` | `{ sharp: "sharp", round: "round", elbow: "elbow" }` (appState currentItemArrowType) |
| `THEME` | `{ LIGHT: "light", DARK: "dark" }` |
| `ELEMENT_LINK_KEY` | `"element"` (URL query key for element links) |
| `DEFAULT_GRID_SIZE` / `DEFAULT_GRID_STEP` | 20 / 5 |
| `DEFAULT_EXPORT_PADDING` | 10 |
| `ELEMENT_READY_TO_ERASE_OPACITY` | 20 |
| `DEFAULT_REDUCED_GLOBAL_ALPHA` | 0.3 |
| `LINE_CONFIRM_THRESHOLD` | 8 |
| `EXPORT_DATA_TYPES` | excalidraw `"excalidraw"`, excalidrawLibrary `"excalidrawlib"` (the clipboard type is in the clipboard section) |
| `VERSIONS` | excalidraw 2, excalidrawLibrary 2 |
| `STRING_MIME_TYPES` | text `text/plain`, html `text/html`, json `application/json`, excalidraw `application/vnd.excalidraw+json` |
| `IMAGE_MIME_TYPES` (partial) | svg `image/svg+xml`, png `image/png`, jpg `image/jpeg` (the full list, e.g. gif/webp/bmp/ico/avif/jfif, is **UNVERIFIED** here) |
| `SVG_NS` | `http://www.w3.org/2000/svg` |
| Sticky notes | `STICKY_NOTE_MIN_SIZE` 75, `DEFAULT_STICKY_NOTE_SIZE` 250, `STICKY_NOTE_PADDING` 16, `STICKY_NOTE_MIN_FONT_SIZE` 16, `STICKY_NOTE_MAX_FONT_SIZE` 512, `STICKY_NOTE_FONT_STEP` 2, `STICKY_NOTE_SHADOW_OFFSET` 3, `STICKY_NOTE_SHADOW_OPACITY` 0.16, `STICKY_NOTE_EDGE_SHADOW_WIDTH` 0.5, `STICKY_NOTE_EDGE_SHADOW_OPACITY` 0.08, `STICKY_NOTE_FOOTER` = {height 20, fontSize 12, fontFamily "Helvetica, Arial, sans-serif", baselineFromBottom 14, opacity 1, minBodyWidthForYear 80}, `STICKY_NOTE_BODY_INSET_Y = PADDING*2 + FOOTER.height` (= 52) |

**COLOR_PALETTE** (colors.ts). Each colour has five shades, indices 0–4:

| key | shades |
|---|---|
| transparent | `transparent` |
| black | `#1e1e1e` |
| white | `#ffffff` |
| gray | #f8f9fa #e9ecef #ced4da #868e96 #343a40 |
| red | #fff5f5 #ffc9c9 #ff8787 #fa5252 #e03131 |
| pink | #fff0f6 #fcc2d7 #f783ac #e64980 #c2255c |
| grape | #f8f0fc #eebefa #da77f2 #be4bdb #9c36b5 |
| violet | #f3f0ff #d0bfff #9775fa #7950f2 #6741d9 |
| blue | #e7f5ff #a5d8ff #4dabf7 #228be6 #1971c2 |
| cyan | #e3fafc #99e9f2 #3bc9db #15aabf #0c8599 |
| teal | #e6fcf5 #96f2d7 #38d9a9 #12b886 #099268 |
| green | #ebfbee #b2f2bb #69db7c #40c057 #2f9e44 |
| yellow | #fff9db #ffec99 #ffd43b #fab005 #f08c00 |
| orange | #fff4e6 #ffd8a8 #ffa94d #fd7e14 #e8590c |
| bronze | #f8f1ee #eaddd7 #d2bab0 #a18072 #846358 |

`DEFAULT_ELEMENT_STROKE_COLOR_INDEX` = 4, `DEFAULT_ELEMENT_BACKGROUND_COLOR_INDEX` = 1, `DEFAULT_CHART_COLOR_INDEX` = 4.
- Stroke quick picks (`DEFAULT_ELEMENT_STROKE_PICKS`): black, red[4], green[4], blue[4], yellow[4], i.e. `#1e1e1e #e03131 #2f9e44 #1971c2 #f08c00`.
- Background quick picks (`DEFAULT_ELEMENT_BACKGROUND_PICKS`): `transparent #ffc9c9 #b2f2bb #a5d8ff #ffec99`.
- `BUCKET_FILL_BACKGROUND_PICKS`: `#ffffff #ffc9c9 #b2f2bb #a5d8ff #ffec99`.
- `DEFAULT_CANVAS_BACKGROUND_PICKS`: `#ffffff #f8f9fa #f5faff #fffce8 #fdf8f6`.
- `STICKY_NOTE_BACKGROUND_PICKS`: `#ffdf6b #fcc2d7 #b2f2bb #a5d8ff #ffd8a8`. `STICKY_NOTE_STROKE_PICKS` = the stroke picks.

Dark mode helpers in colors.ts: `applyDarkModeFilter(color, enable)` applies CSS-style
`invert(93%)` followed by `hue-rotate(180deg)` to a single colour, and
`removeDarkModeFilter` reverses it. Dark mode is therefore computed per colour, not as a
canvas filter. The rendering section has the details.

### 1a.7 New element-related modules (for awareness)

| File | What it is |
|---|---|
| `stickyNote.ts` | Sticky notes (`type: "stickynote"`). Always filled (fill forced to solid); corner radius = `min(dim × 0.04, 16)`; corner jitter from `STICKY_NOTE_RENDER_ROUGHNESS = [0, 1.5, 8]`, indexed by roughness; at roughness 2 one corner, chosen by the seed, is "lifted" (quadratic bend) and gets a shadow. The bound label auto-fits: font size is the largest value in `{ceiling − k·STEP(2)} ∪ {min}` that fits; height grows past `baseHeight` only when the text still overflows at the minimum font size. A creation-date footer ("7 Sep", or "7 Sep 2024" when not the current year) is drawn at `x = width − 16`, `y = height − 14`, 12 px Helvetica. Label stroke colour is synced with the note (`syncStickyNoteInk`). The label stores `baseFontSize`. |
| `heading.ts` | Cardinal "heading" vectors RIGHT [1,0], DOWN [0,1], LEFT [-1,0], UP [0,-1], used by elbow-arrow routing (`vectorToHeading`, `headingForPoint`, `headingForPointFromElement`, `flipHeading`, …). Adds no fields. |
| `arrowEndpointText.ts` | Lets you create a text label at an unbound arrow tip. The text is linked by the arrow's `startBinding`/`endBinding` pointing at the text element's id (uncontained text is bindable). Adds no fields. |
| `bucketFill.ts` | Bucket-fill tool. Computes the smallest closed region under the click from nearby element outlines (islands become holes via keyhole bridges) and creates a **`line` element with `polygon: true`**, an opaque background and a transparent stroke. Constants: `BUCKET_FILL_GAP_TOLERANCE` 6, `BUCKET_FILL_CURVE_MAX_DEVIATION` 0.5, `BUCKET_FILL_REGION_MATCH_TOLERANCE` 2, `DEFAULT_BUCKET_FILL_OPTIONS` {snapEpsilon 0.5, maxBoundarySegments 2560, maxGeneratedPoints 1536}. No marker field: fills are recognised by shape (5% area tolerance). |
| `convertToShape.ts` | Recognises freedraw strokes (`recognizeShape`, `convertToShape`) and converts them to rectangle/diamond/ellipse/arrow/line. Thresholds: min screen size 25 px, closed-shape gap ratio ≤ 0.15, straight elongation ≤ 0.25, arrow skew ≥ 0.3, match distance 1.5. Adds no fields. |

## 1b. File formats, restore/migration, clipboard, library, export, fractional index

Sources fetched 2026-09-27 from `excalidraw/excalidraw@master` (raw.githubusercontent.com; line numbers from sourcegraph search). Paths are repo-relative.

### 1b.1 Constants (packages/common/src/constants.ts)

| Constant | Value |
|---|---|
| `EXPORT_DATA_TYPES` | `excalidraw: "excalidraw"`, `excalidrawClipboard: "excalidraw/clipboard"`, `excalidrawLibrary: "excalidrawlib"`, `excalidrawClipboardWithAPI: "excalidraw-api/clipboard"` |
| `VERSIONS` | `excalidraw: 2`, `excalidrawLibrary: 2` |
| `MIME_TYPES.excalidraw` | `"application/vnd.excalidraw+json"` (also the PNG tEXt keyword and SVG payload-type) |
| `MIME_TYPES.excalidrawClipboard` | `"application/vnd.excalidraw.clipboard+json"` |
| `MIME_TYPES.excalidrawlib` | `"application/vnd.excalidrawlib+json"` |
| `MIME_TYPES.excalidrawlibIds` | `"application/vnd.excalidrawlib.ids+json"` |
| `MIME_TYPES.binary` | `"application/octet-stream"` |
| `IMAGE_MIME_TYPES` | svg `image/svg+xml`, png `image/png`, jpg `image/jpeg`, gif `image/gif`, webp `image/webp`, bmp `image/bmp`, ico `image/x-icon`, avif `image/avif`, jfif `image/jfif` |
| `getExportSource()` | `window.EXCALIDRAW_EXPORT_SOURCE \|\| window.location.origin` (e.g. `"https://excalidraw.com"`); a port may write its own string, it is informational only |
| `DEFAULT_EXPORT_PADDING` | `10` |
| `EXPORT_SCALES` | `[1, 2, 3]` |
| `DEFAULT_IMAGE_OPTIONS` | `maxWidthOrHeight: 1440`, `maxFileSizeBytes: 4 * 1024 * 1024` |
| `DEFAULT_FILENAME` | `"Untitled"` |
| `SVG_NS` | `"http://www.w3.org/2000/svg"` |
| `SVG_DOCUMENT_PREAMBLE` | `<?xml version="1.0" standalone="no"?>\n<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" \n"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">\n` (note the trailing space after `EN"`) |
| `DEFAULT_ELEMENT_PROPS` | `strokeColor: COLOR_PALETTE.black`, `backgroundColor: COLOR_PALETTE.transparent`, `fillStyle: "solid"`, `strokeWidth: STROKE_WIDTH["medium"]` (=2), `strokeStyle: "solid"`, `roughness: ROUGHNESS.artist` (=1), `opacity: 100`, `locked: false` |
| `STROKE_WIDTH` | `thin: 1`, `medium: 2`, `bold: 4`, `extraBold: 8`; `STROKE_WIDTH_KEYS = ["thin","medium","bold"]`; `DEFAULT_ELEMENT_STROKE_WIDTH_KEY = "medium"` |
| `ROUGHNESS` | `architect: 0`, `artist: 1`, `cartoonist: 2` |
| `ROUNDNESS` | `LEGACY: 1`, `PROPORTIONAL_RADIUS: 2`, `ADAPTIVE_RADIUS: 3` |
| `DEFAULT_FONT_SIZE` | `20` |
| `DEFAULT_FONT_FAMILY` | `FONT_FAMILY.Excalifont` (=5) |
| `FONT_FAMILY` | `Virgil: 1, Helvetica: 2, Cascadia: 3, Excalifont: 5, Nunito: 6, "Lilita One": 7, "Comic Shanns": 8, "Liberation Sans": 9, Assistant: 10` |
| `FONT_FAMILY_FALLBACKS` | `{ Xiaolai: 100, ...FONT_FAMILY_GENERIC_FALLBACKS, "Segoe UI Emoji": 1000 }` (generic fallbacks' ids UNVERIFIED here) |
| `DEFAULT_TEXT_ALIGN` / `DEFAULT_VERTICAL_ALIGN` | `"left"` / `"top"` |
| `DEFAULT_STROKE_STREAMLINE` | `0.5` |
| `DEFAULT_GRID_SIZE` / `DEFAULT_GRID_STEP` | `20` / `5` |
| `FRAME_STYLE` | `strokeColor "#bbb", strokeWidth 2, strokeStyle "solid", fillStyle "solid", roughness 0, roundness null, backgroundColor "transparent", radius 8, nameOffsetY 3, nameColorLightTheme "#999999", nameColorDarkTheme "#7a7a7a", nameFontSize 14, nameLineHeight 1.25` |
| `ARROW_TYPE` | `sharp`, `round`, `elbow` |

Per-family line heights (packages/common/src/font-metadata.ts `FONT_METADATA[..].metrics.lineHeight`; `getLineHeight(fontFamily)` falls back to Excalifont's metrics):

| Family (id) | unitsPerEm | ascender | descender | lineHeight | flags |
|---|---|---|---|---|---|
| Excalifont (5) | 1000 | 886 | -374 | 1.25 | |
| Nunito (6) | 1000 | 1011 | -353 | 1.25 | |
| Lilita One (7) | 1000 | 923 | -220 | 1.15 | |
| Comic Shanns (8) | 1000 | 750 | -250 | 1.25 | |
| Virgil (1) | 1000 | 886 | -374 | 1.25 | deprecated |
| Helvetica (2) | 2048 | 1577 | -471 | 1.15 | deprecated, local |
| Cascadia (3) | 2048 | 1900 | -480 | 1.2 | deprecated |
| Liberation Sans (9) | 2048 | 1854 | -434 | 1.15 | private |
| Assistant (10) | 2048 | 1021 | -287 | 1.25 | private |
| Xiaolai (100) | 1000 | 880 | -144 | 1.25 | fallback |
| Segoe UI Emoji (1000) | 1000 | 886 | -374 | 1.25 | local, fallback |

### 1b.2 `.excalidraw` file (packages/excalidraw/data/json.ts `serializeAsJSON`)

```json
{
  "type": "excalidraw",          // EXPORT_DATA_TYPES.excalidraw
  "version": 2,                  // VERSIONS.excalidraw
  "source": "https://excalidraw.com",  // getExportSource()
  "elements": [ ... ],           // ALL elements incl. isDeleted ones, in z-order (array order)
  "appState": { ... },           // cleanAppStateForExport(appState)
  "files": { "<fileId>": BinaryFileData, ... }
}
```

- Serialized with `JSON.stringify(data, null, 2)` (2-space indent). MIME `application/vnd.excalidraw+json`, extension `.excalidraw`.
- `elements` is passed as-is: deleted elements are NOT filtered out by `serializeAsJSON` (callers pass `scene.getElementsIncludingDeleted()`; UNVERIFIED which list the save action passes, but load marks invisibles deleted anyway).
- `files` = `filterOutDeletedFiles(elements, files)`: keep only `files[el.fileId]` for non-deleted elements that have a `fileId` and whose file exists. For `type === "database"` (collab backend) `files` is `undefined` and appState uses the `server` flags.
- Valid scene check (`isValidExcalidrawData`): `data.type === "excalidraw" && (!data.elements || (Array.isArray(data.elements) && (!data.appState || typeof data.appState === "object")))`. Version number is not checked.
- `ExportedDataState` / `ImportedDataState` types in data/types.ts; `ImportedDataState` also allows `scrollToContent?: boolean`, `libraryItems?`.

**appState keys saved** (`APP_STATE_STORAGE_CONF` in packages/excalidraw/appState.ts; `cleanAppStateForExport` keeps keys whose `export` flag is true): exactly

| key | default (`getDefaultAppState`) |
|---|---|
| `gridSize` | `DEFAULT_GRID_SIZE` = 20 |
| `gridStep` | `DEFAULT_GRID_STEP` = 5 |
| `gridModeEnabled` | `false` |
| `viewBackgroundColor` | `COLOR_PALETTE.white` (`"#ffffff"`) |
| `lockedMultiSelections` | `{}` |

(`server` flags are true for the same five keys. Everything else — `theme`, `currentItem*`, `exportBackground`, `exportEmbedScene`, `exportScale`, `exportWithDarkMode`, `name`, `zoom`, `scrollX/Y`, `selectedElementIds` etc. — is `browser`-only, i.e. localStorage, never written into files.)

`BinaryFileData` (packages/excalidraw/types.ts):

```ts
type BinaryFileData = {
  mimeType: ValueOf<typeof IMAGE_MIME_TYPES> | "application/octet-stream";
  id: FileId;            // == image element's fileId
  dataURL: DataURL;      // "data:<mime>;base64,<...>"
  created: number;       // epoch ms
  lastRetrieved?: number;// epoch ms
  version?: number;
};
type BinaryFiles = Record<FileId, BinaryFileData>;
```

- New file ids: `generateIdFromFile` (data/blob.ts) = lowercase hex SHA-1 of the file bytes (40 chars); fallback `nanoid(40)`.
- Inserted images are downscaled if max(w,h) > `maxWidthOrHeight` (1440) via `resizeImageFile` (SVG never resized); max file size 4 MiB.
- `dataURLToFile` parses `data:<mime>;base64,<payload>`.

**Loading a file** (data/blob.ts `loadSceneOrLibraryFromBlob`):
1. `parseFileContents`: if blob type `image/png` → `decodePngMetadata` (1b.6); if `image/svg+xml` → `decodeSvgBase64Payload` (1b.7); else read as UTF-8 text. MIME from extension when missing (`getMimeType`: `.excalidraw|.json` → json, `.png`, `.jpe?g`, `.svg`, `.excalidrawlib`).
2. `JSON.parse`; if `isValidExcalidrawData` →
   - `elements = restoreElements(data.elements, localElements, { repairBindings: true, deleteInvisibleElements: true })` (no `refreshDimensions`),
   - `appState = restoreAppState({ theme: localAppState?.theme, fileHandle, ...cleanAppStateForExport(data.appState || {}), ...scrollToContentState }, localAppState)` — i.e. only the five export keys are honored from the file, the rest come from local state/defaults,
   - `files = data.files || {}` (no validation of file entries).
3. else if `isValidLibrary(data)` → library (1b.5). Else "invalid file".

Note: there is **no longer a single `restore()` function** in data/restore.ts on master (only the type `RestoredDataState = {elements, appState, files}`); hosts call `restoreElements` + `restoreAppState` directly.

### 1b.3 Element restore / migration (packages/excalidraw/data/restore.ts)

#### `restoreElements(targetElements, existingElements, opts?)`
opts: `{ refreshDimensions?, repairBindings?, deleteInvisibleElements? }`. Steps (verbatim logic):
1. Skip elements with `type === "selection"`.
2. `restoreElement(el, targetElementsMap, existingElementsMap, {deleteInvisibleElements})`; exceptions → element dropped.
3. If `deleteInvisibleElements && isInvisiblySmallElement(original)` → `bumpVersion` + `isDeleted: true`.
   `isInvisiblySmallElement` (element/src/sizeHelpers.ts): linear/freedraw → `points.length < 2 || (points.length === 2 && isArrow && pointsEqual(p0, pLast, 0.1))`; other types → `width === 0 && height === 0`.
4. Duplicate ids → later duplicate gets `id: randomId()`.
5. Whole array passed through `syncInvalidIndices` (1b.9).
6. If `!repairBindings` stop here. Otherwise, per element:
   - `repairFrameMembership`: `frameId` pointing to a missing element → `frameId = null`.
   - text with `containerId` → `repairBoundElement`: text `angle` := container.angle (0 if container is an arrow, 0 if none); missing container → `containerId = null`; if container's `boundElements` lacks this text, push `{type:"text", id}`.
   - else if element has `boundElements` → `repairContainerElement`: dedupe `boundElements` by id, drop entries whose element is missing or `isDeleted`; bound text without `containerId` gets `containerId = container.id`.
   - `refreshDimensions` (only when asked, e.g. after fonts load) → `refreshTextDimensions` for text not bound to a sticky note.
   - linear elements: `startBinding`/`endBinding` set to `null` if target id missing, or if element is not an arrow (lines never bind).
7. `restoreStickyNotes` (sticky-note text: `baseFontSize` normalized, stroke color synced container↔text; non-sticky text gets `baseFontSize: null`; with refreshDimensions re-layout via `getStickyNoteLayout`).
8. `repairBoundTextElementOrder`: `normalizeBoundElementsOrder` (element/src/sortElements.ts) moves each container's bound text(s) to immediately after the container (container keeps its position); then `syncMovedIndices` for moved texts.
9. Elbow-arrow fixes: an elbow arrow not bound on both sides (`!isArrowBoundToElement`) whose points fail `validateElbowPoints` is re-routed with `updateElbowArrowPoints(el, map, {points: [ (0,0), lastPoint ]})` keeping `index`. A self-bound elbow arrow (start==end element) with any |coord| > 1e6 is replaced by a fixed 4-point loop: `x = bx + bw/2`, `y = by - 5`, points `[(0,0),(0,-10),(bw/2+5,-10),(bw/2+5,bh/2+5)]`.

#### `restoreElementWithProperties(element, extra)` — base fields for every type
Result = `{...element, ...base, ...getNormalizedDimensions(base), ...extra}` then `delete strokeSharpness`, `delete boundElementIds`. **Unknown extra properties on the input are preserved** (spread first).

| field | rule |
|---|---|
| `type` | `extra.type \|\| element.type` |
| `version` | `element.version \|\| 1` |
| `versionNonce` | `?? 0` |
| `index` | `?? null` (then fixed by syncInvalidIndices) |
| `isDeleted` | `?? false` |
| `id` | `\|\| randomId()` |
| `fillStyle` | `\|\| "solid"` |
| `strokeWidth` | `\|\| 2` (note: `\|\|`, so 0 → 2) |
| `strokeStyle` | `?? "solid"` |
| `roughness` | `?? 1` |
| `opacity` | `== null ? 100 : opacity` |
| `angle` | `\|\| 0` |
| `x`, `y` | `extra.x ?? element.x ?? 0` |
| `strokeColor` | `\|\| "#1e1e1e"` (COLOR_PALETTE.black; hex value UNVERIFIED here, see colors section) |
| `backgroundColor` | `\|\| "transparent"` |
| `width`, `height` | `\|\| 0` |
| `seed` | `?? 1` |
| `groupIds` | `?? []` |
| `frameId` | `?? null` |
| `roundness` | if present keep; else legacy `strokeSharpness === "round"` → `{type: isUsingAdaptiveRadius(type) ? ROUNDNESS.LEGACY (1) : ROUNDNESS.PROPORTIONAL_RADIUS (2)}`; else `null` |
| `boundElements` | legacy `boundElementIds: string[]` → `ids.map(id => ({type:"arrow", id}))`; else `?? []` |
| `updated` | `?? getUpdatedTimestamp()` (epoch ms) |
| `created` | `?? null` |
| `link` | `link ? normalizeLink(link) : null` (`normalizeLink`: trim, escape `"`, `@braintree/sanitize-url` → dangerous schemes like `javascript:` become `about:blank`) |
| `locked` | `?? false` |
| `customData` | copied only if key present |

`getNormalizedDimensions` (element/src/sizeHelpers.ts): negative width → `width = |w|, x = x - |w|`; same for height/y.
`isUsingAdaptiveRadius(type)`: `rectangle | embeddable | iframe | image`. `isUsingProportionalRadius`: `line | arrow | diamond | stickynote` (element/src/typeChecks.ts).

#### Per-type cases of `restoreElement`

| type | extra fields / rules |
|---|---|
| `text` | delete `rawText`. Legacy `font: "20px Virgil"` → `fontSize = parseFloat("20px")`, `fontFamily = getFontFamilyByName("Virgil")` (key lookup in `FONT_FAMILY`, unknown → `DEFAULT_FONT_FAMILY` = 5 Excalifont). Non-finite fontSize → 20. `text = string or ""`. `lineHeight = element.lineHeight \|\| (element.height ? detectLineHeight(el) : getLineHeight(el.fontFamily))`, where `detectLineHeight = height / lineCount / fontSize` (`lineCount = splitIntoLines(text).length`). `textAlign \|\| "left"`, `verticalAlign \|\| "top"`, `containerId ?? null`, `originalText \|\| text`, `autoResize ?? true`, `labelPosition`: finite → `clamp(v,0,1)` else `null`; `baseFontSize`: finite → `normalizeStickyNoteFontSize(v)` else `null`. If `deleteInvisibleElements` and `text` empty and not deleted → `isDeleted: true` + bumpVersion. (Note: `getLineHeight` is called with the *original* `element.fontFamily`, not the one derived from legacy `font`.) |
| `freedraw` | `restoreFreedrawPoints`: non-array → `[]`; keep only valid `[x,y]` points; for each kept point, if `index in pressures` push pressure (non-finite → `0.5`) — so `pressures` may be shorter than points or empty. `simulatePressure` copied as-is. `strokeOptions = { variability: "constant"\|"variable" (default "variable"), streamline: finite ? v : 0.5 }`. |
| `image` | `status \|\| "pending"`, `fileId` as-is, `scale \|\| [1, 1]`, `crop ?? null` |
| `line` (and legacy `draw` → becomes `line`) | arrowheads via `normalizeArrowhead`; points via `restoreLinearElementPoints` (<2 valid points → `[(0,0),(w\|\|0, h\|\|0)]`); if `points[0] != (0,0)` → `LinearElementEditor.getNormalizeElementPointsAndCoords` (shift so first point is origin, move x/y). `startBinding: null`, `endBinding: null`. `polygon: isValidPolygon(points) ? (polygon ?? false) : false` (`isValidPolygon`: `points.length > 3 && pointsEqual(first,last)`). width/height from `getSizeFromPoints(points)`. Then `handleOversizedLinearElements`. |
| `arrow` | `startArrowhead = normalizeArrowhead(v)`; `endArrowhead = v === undefined ? "arrow" : normalizeArrowhead(v)`; points restored as for line; `startBinding/endBinding = repairBinding(...)`; `elbowed` copied. If elbow: `elbowed: true`, `fixedSegments = (fixedSegments?.length && points.length >= 4) ? fixedSegments : null`, `startIsSpecial`, `endIsSpecial` copied. Then normalize points/coords, then `handleOversizedLinearElements`. |
| `rectangle`, `ellipse`, `diamond`, `iframe`, `embeddable` | base only (`extra = {}`) — all other properties preserved by the spread |
| `stickynote` | base + `baseHeight: baseHeight ?? maxHeight(legacy) ?? height`, then `normalizeStickyNote` (element/src/newElement.ts; details UNVERIFIED here — see data-model section) |
| `frame`, `magicframe` | base + `name: name ?? null` |
| anything else | returns `null` → element dropped |

`normalizeArrowhead` (element/src/arrowheads.ts): `undefined|null → null`, `"dot" → "circle"`, `"crowfoot_one" → "cardinality_one"`, `"crowfoot_many" → "cardinality_many"`, `"crowfoot_one_or_many" → "cardinality_one_or_many"`, others unchanged.

`handleOversizedLinearElements`: `MAX_LINEAR_PX = 75_000`; if `width > 75000 || height > 75000` → replaced by `{x:0,y:0,width:100,height:100,points:[(0,0),(100,100)],isDeleted:true}`.

#### Binding formats and migration (`repairBinding`)

Current type (element/src/types.ts) — the **only** binding type on master; `PointBinding`, `focus`, `gap` no longer appear anywhere in types.ts:

```ts
type FixedPoint = [number, number];            // ratios of the bound element's (unrotated) w/h
type BindMode = "inside" | "orbit" | "skip";
type FixedPointBinding = { elementId: string; fixedPoint: FixedPoint; mode: BindMode };
type BoundElement = { id: string; type: "arrow" | "text" };   // entries of boundElements
```

Legacy (v1) binding on disk: `{ elementId, focus: number(-1..1), gap: number }`, sometimes with `fixedPoint` (older elbow arrows). Rules:

1. `binding` null → `null`.
2. Elbow arrow → `{...binding, fixedPoint: normalizeFixedPoint(binding.fixedPoint), mode: binding.mode || "orbit"}` (extra legacy keys like `focus`/`gap` are kept by the spread).
3. Simple arrow with `mode` present ("schema v2") → `{elementId, mode, fixedPoint: normalizeFixedPoint(fixedPoint)}` (or `null` if no elementId).
4. Simple arrow without `mode` ("schema v1") → migrate. **`focus` and `gap` are ignored**; the new binding is derived from the arrow's current endpoint geometry:
   - find bound element in the loaded elements, else in `existingElements`; if not found → `null` (console error).
   - `p` = global coords of point 0 (start) or last point (end).
   - `mode = isPointInElement(p, boundElement) ? "inside" : "orbit"`.
   - `focusPoint = mode === "inside" ? p : (projectFixedPointOntoDiagonal(arrow, p, boundElement, startOrEnd, map, DEFAULT_ZOOM) || p)`.
   - `fixedPoint = calculateFixedPointForNonElbowArrowBinding(arrow, boundElement, startOrEnd, map, focusPoint).fixedPoint`.
   - return `{mode, elementId, fixedPoint}`.

`calculateFixedPointForNonElbowArrowBinding` (element/src/binding.ts): rotate `focusPoint` by `-angle` around element center; if `w < 1 || h < 1` (`MIN_BINDABLE_SIZE = 1`) → `normalizeFixedPoint([0.5,0.5])`; else `fx = (px - x) / max(w, gap)`, `fy = (py - y) / max(h, gap)` with `gap = getBindingGap(el) = BASE_BINDING_GAP (5) + strokeWidth/2`; then `normalizeFixedPoint`.

`normalizeFixedPoint`: non-`[num,num]` → `[0.5001, 0.5001]`; clamp each ratio to `[-10, 10]` (`FIXED_POINT_BOUND = 10`); if either coordinate is within `1e-4` of 0.5, every coordinate within `1e-4` of 0.5 becomes `0.5001` (exact center is avoided).

Inverse (`getGlobalFixedPointForBindableElement`): `rotate((x + w*fx, y + h*fy), center, angle)`.

`projectFixedPointOntoDiagonal` (element/src/utils.ts): (a) if midpoint snapping enabled and the point is within `maxBindingDistance_simple(zoom) + strokeWidth/2` of a side midpoint (outside the shape) → that midpoint; (b) if `arrow.width < 3 && arrow.height < 3` → `null`; (c) cast a ray from the adjacent arrow point (point 1 / n-2; for 2-point arrows the other end's bound fixed point if any) through `p`, intersect with the two diagonals of the element (rectangular: corner-to-corner; others: vertical/horizontal center lines; each shrunk by 15 px at both ends for `rectangle`, 0 otherwise), take the nearest intersection to the ray origin, return it if it lies in the element else `null`. `maxBindingDistance_simple(zoom) = clamp(15 / (min(zoom,1) * 1.5), 15, 30)`.

Other binding constants in binding.ts: `BASE_BINDING_GAP = 5`, `BASE_ARROW_MIN_LENGTH = 10`, `FOCUS_POINT_SIZE = 10/1.5`.

Porting advice: a native port can implement steps 1–3 exactly and, for step 4, the simplified version (mode from point-in-shape; fixedPoint from the endpoint itself, skipping the diagonal projection) — results differ only in where the arrow re-attaches after the first move of the bound shape.

#### `restoreAppState(appState, localAppState)`
- Legacy key migration: `isSidebarDocked` → `defaultSidebarDockedPreference`.
- For every key of `getDefaultAppState()`: file value if `!== undefined`, else local value, else default.
- `colorTopPicks` / `fontTopPicks` sanitized (`COLOR_TOP_PICKS_SLOTS` UNVERIFIED; `FONT_TOP_PICKS_SLOTS = 3`, private/fallback fonts rejected).
- Legacy numeric `currentItemStrokeWidth` → `currentItemStrokeWidthKey` via `STROKE_WIDTH` reverse lookup (1/2/4 → thin/medium/bold), else default `"medium"`.
- `activeTool`: kept only if `AllowedExcalidrawActiveTools[type]` (false for `eraser`, `laser`, `autoshape`, `magicframe`; true for selection, lasso, text, rectangle, diamond, ellipse, line, image, arrow, freedraw, stickynote, custom, frame, embeddable, hand, bucketfill), else selection; `locked ?? false`, `lastActiveTool: null`.
- `zoom`: accepts legacy number or `{value}`, normalized. `gridSize`/`gridStep` normalized (`getNormalizedGridSize/Step` in scene/; bounds UNVERIFIED). `openSidebar` legacy string → `{name:"default"}`. `editingFrame: null`, `cursorButton: local || "up"`.

Relevant `getDefaultAppState()` defaults: `exportBackground: true`, `exportEmbedScene: false`, `exportScale: EXPORT_SCALES.includes(devicePixelRatio) ? devicePixelRatio : 1`, `exportWithDarkMode: false`, `theme: "light"`, `name: null`, `frameRendering: {enabled:true, clip:true, name:true, outline:true}`, `objectsSnapModeEnabled: false`, `gridModeEnabled: false`, `isBindingEnabled: true`, `bindingPreference: "enabled"`, `bindMode: "orbit"`, `boxSelectionMode: "contain"`, `currentItemEndArrowhead: "arrow"`, `currentItemStartArrowhead: null`, `currentItemRoundness: "round"`, `currentItemArrowType: "round"`, `currentItemStrokeVariability: "constant"`, `currentItemStrokeWidthKey: "medium"`, `currentItemFontSize: 20`, `currentItemFontFamily: 5`.

### 1b.4 Versioning helpers (element/src/mutateElement.ts)
`bumpVersion(el, version?)`: `version = (version ?? el.version) + 1; versionNonce = randomInteger(); updated = Date.now()`. `mutateElement`/`newElementWith` do the same on every change. A port should bump `version`/`versionNonce`/`updated` on every edit so files merge correctly with other Excalidraw clients.

### 1b.5 Library `.excalidrawlib` (data/json.ts, data/blob.ts, data/restore.ts, data/library.ts)

v2 (written by `serializeLibraryAsJSON`, `JSON.stringify(data, null, 2)`, MIME `application/vnd.excalidrawlib+json`, default file name `library.excalidrawlib`):
```json
{ "type": "excalidrawlib", "version": 2, "source": "<origin>",
  "libraryItems": [ { "id": "<id>", "status": "published"|"unpublished",
                      "elements": [ ...non-deleted elements... ],
                      "created": 1690000000000, "name": "optional" } ] }
```
`LibraryItem` also has optional `error?: string`.

v1: `{ "type": "excalidrawlib", "version": 1, "source": ..., "library": [ [el, el, ...], [ ... ] ] }` — each item is a bare element array.

Load (`parseLibraryJSON`): `isValidLibrary` = object with `type === "excalidrawlib"` and `version` 1 or 2; items = `data.libraryItems || data.library`; `restoreLibraryItems(items, defaultStatus = "unpublished")`: array item → `{status: default, elements: item, id: randomId(), created: Date.now()}`; object item → fill missing `id`/`status`/`created`. Each item's elements go through `restoreElements(elements, null)` (no repairBindings) then deleted ones dropped; items with 0 elements are dropped.

Dedup when importing (`mergeLibraryItems(local, other)`): an incoming item is skipped if a local item has the same element count and pairwise equal `id` + `versionNonce`; new items are prepended: `[...newItems, ...localItems]`.

Inserting library items into the scene: `distributeLibraryItemsOnSquareGrid(items)` lays items out on a grid, `ITEMS_PER_ROW = ceil(sqrt(n))`, `PADDING = 50`, each item centered in its cell (cell width = max width in that column, height = max height in that row), then `addElementsFromPasteOrLibrary({elements, position: "center" (menu insert) or the drop point, ...})` (1b.8) — i.e. new ids/seeds, centered on the target point.

### 1b.6 PNG with embedded scene (data/image.ts)

Export (`encodePngMetadata`, called from data/index.ts `exportCanvas` when `appState.exportEmbedScene`, file extension `.excalidraw.png` instead of `.png`):
1. `metadata = serializeAsJSON(elements, appState, files, "local")` (same text as a .excalidraw file).
2. `encoded = encode({text: metadata, compress: true})` (data/encode.ts) →
   `{"version":"1","encoding":"bstring","compressed":true,"encoded":"<bytestring>"}` where `<bytestring>` = pako `deflate(utf8(text))` (**zlib-wrapped** deflate, RFC 1950), each byte mapped to one JS char U+0000..U+00FF. If deflate fails: `compressed:false`, `encoded` = UTF-8 bytes of text as bytestring. Key order in JSON: `version, encoding, compressed, encoded`.
3. Chunk: `tEXt` with keyword `application/vnd.excalidraw+json`, text = `JSON.stringify(encoded)`, written as Latin-1 (one byte per char, `png-chunk-text`); JSON escaping turns control bytes (incl. 0x00) into `\u00XX`, bytes 0x80–0xFF appear raw.
4. Inserted as the second-to-last chunk (just before `IEND`, `chunks.splice(-1, 0, chunk)`), CRC recomputed.

Import (`decodePngMetadata`): take the **first** `tEXt` chunk; keyword must equal `application/vnd.excalidraw+json` else "INVALID"; parse text (Latin-1) as JSON; if it lacks `encoded` but has `type === "excalidraw"` → it is the raw scene JSON (legacy); else `decode`: `encoding` must be `"bstring"`; if `compressed` → bytes(encoded) → zlib inflate → UTF-8 string; else `byteStringToString` (UTF-8 decode of the byte string).

Emacs: `zlib-decompress-region` handles zlib-wrapped data; for writing, a zlib encoder is needed (no built-in compressor in Emacs — call out to the C module / `zlib` or use `compressed:false`, which readers accept).

### 1b.7 SVG with embedded scene (scene/export.ts)

Root structure produced by `exportToSvg` (in order):
```xml
<svg version="1.1" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 W H" width="W*scale" height="H*scale">
  <!-- svg-source:excalidraw -->
  <metadata>
    <!-- payload-type:application/vnd.excalidraw+json --><!-- payload-version:2 --><!-- payload-start -->BASE64<!-- payload-end -->
  </metadata>
  <defs><style class="style-fonts">@font-face{...}...</style> <clipPath id="<frameId>">...</clipPath>...</defs>
  <rect x="0" y="0" width="W" height="H" fill="<bg>"/>   <!-- only if exportBackground -->
  ...elements...
</svg>
```
- Comments are created with `document.createComment(" " + text + " ")`, i.e. `<!-- payload-start -->` with single spaces.
- Payload (`encodeSvgBase64Payload`, only when `exportEmbedScene` and export type is file-svg, not clipboard-svg): `base64( JSON.stringify( encode({text: serializeAsJSON(..., "local")}) ) )` — `encode` compresses by default, and the JSON string (chars ≤ U+00FF) is base64-ed **as a byte string** (`btoa` directly, `isByteString = true`).
- Decode (`decodeSvgBase64Payload`): requires substring `payload-type:application/vnd.excalidraw+json`; regex `/<!-- payload-start -->\s*(.+?)\s*<!-- payload-end -->/`; version from `/<!-- payload-version:(\d+) -->/`, default `"1"`; version `"1"` → base64 decodes to UTF-8 text; version ≥2 → base64 decodes to byte string. Then same `encoded`/legacy handling as PNG.
- File saved as `SVG_DOCUMENT_PREAMBLE + svg.outerHTML`, extension `.excalidraw.svg` when `exportEmbedScene` else `.svg`.
- Fonts: `@font-face` declarations for used families are inlined (subset, base64) unless `skipInliningFonts` (details in the text/fonts section).
- Background fill = `applyDarkModeFilter(viewBackgroundColor, exportWithDarkMode)`.
- Frames: for every frame-like element a `<clipPath id="<frame.id>">` containing a `<rect transform="translate(frame.x+offsetX frame.y+offsetY) rotate(frame.angle cx cy)" width height rx=8 ry=8>` (`rx/ry` omitted when exporting a single frame). Note: upstream passes `frame.angle` (radians) to SVG `rotate()` (degrees) — looks like an upstream bug; frames are rarely rotated.

### 1b.8 Export options and geometry (scene/export.ts, data/index.ts)

| option | source | default | effect |
|---|---|---|---|
| `exportBackground` | appState (browser-only) | `true` | draw `viewBackgroundColor` behind content; else transparent |
| `exportPadding` | arg | `DEFAULT_EXPORT_PADDING = 10` (forced to `0` when exporting a single frame) | added on all four sides, in scene units |
| `exportScale` | appState | `devicePixelRatio` if in `[1,2,3]` else `1` | PNG canvas size = size × scale; SVG `width/height` attrs = size × scale, `viewBox` unscaled |
| `exportWithDarkMode` | appState | `false` | render with `theme: "dark"`: every color goes through `applyDarkModeFilter` |
| `exportEmbedScene` | appState | `false` | embed scene (PNG tEXt / SVG metadata) |
| `frameRendering` | appState | `{enabled, clip, name, outline}` all true | see below |

Canvas size (`getCanvasSize`): `[minX, minY, maxX, maxY] = getCommonBounds(rootElements)`; `width = maxX - minX + 2*padding`, `height = maxY - minY + 2*padding`; scene is translated by `(-minX + padding, -minY + padding)`, zoom 1, no grid. `getExportSize` truncates `dim * scale`.

`applyDarkModeFilter(color)` (common/src/colors.ts): per-color emulation of CSS `invert(93%) hue-rotate(180deg)`: `c' = round(clamp(c*(1-0.93) + (255-c)*0.93))` per channel, then the standard CSS hue-rotate matrix with θ=180° (`[0.213+c*0.787-s*0.213, 0.715-c*0.715-s*0.715, 0.072-c*0.072+s*0.928; 0.213-c*0.213+s*0.143, 0.715+c*0.285+s*0.140, 0.072-c*0.072-s*0.283; 0.213-c*0.213-s*0.787, 0.715-c*0.715+s*0.715, 0.072+c*0.928+s*0.072]`, clamp 0..1, round), alpha kept (hex8 if alpha < 1). Parsed with tinycolor2 (any CSS color).

Selection-only export (`prepareElementsForExport`): non-deleted elements; if "only selected" and something is selected: selected elements incl. bound text; if exactly one element selected and it is a frame → `exportingFrame = frame`, exported elements = `getElementsOverlappingFrame`; if >1 selected → also include elements inside selected frames. Result is deep-cloned.

Frame handling on export:
- `getFrameRenderingConfig(exportingFrame, fr)`: when exporting a single frame → `{enabled:true, outline:false, name:false, clip:true}` (canvas path then sets `clip = false`), padding 0, bounds = the frame itself.
- Otherwise, if `frameRendering.enabled && frameRendering.name`, frame titles are added as real text elements (`addFrameLabelsAsTextElements`): `newTextElement({x: frame.x, y: frame.y - 3, fontFamily: Helvetica (2), fontSize: 14, lineHeight: 1.25, strokeColor: dark ? "#7a7a7a" : "#999999", text: getFrameLikeTitle(frame)})`, then `y -= height`, then `truncateText(text, frame.width)`; inserted just before the frame. So export bounds include frame titles.
- Children are clipped to their frame (SVG clipPath above; canvas clip in renderer).

Export types (`exportCanvas`): `"png"`, `"svg"`, `"clipboard"` (PNG blob to clipboard), `"clipboard-svg"` (SVG text, never with embedded scene). Empty element list → error `cannotExportEmptyCanvas`.

### 1b.9 Clipboard (packages/excalidraw/clipboard.ts, components/App.duplicate.ts)

Copy (`actionCopy`, `copyToClipboard`): elements = selected with `includeBoundTextElement: true, includeElementsInFrames: true`; written as the same JSON string under both `application/vnd.excalidraw.clipboard+json` and `text/plain`:
```json
{"type":"excalidraw/clipboard","elements":[...],"files":{...}}
```
- Not pretty-printed (`JSON.stringify(contents)`).
- `files` = entries for every initialized image element's `fileId` (key omitted/undefined if host passed no files).
- Elements whose containing frame is not itself being copied are deep-copied with `frameId: null`. No other transformation (ids, seeds, indices, versions kept).
- Cut = copy + delete selected (Ctrl/Cmd+X). Copy as PNG shortcut Alt+Shift+C.

Paste (`parseClipboard`): if HTML present and not plain-paste, parse into mixed text/image-URL content; else `JSON.parse(text)`; accepted if `type ∈ {"excalidraw", "excalidraw/clipboard", "excalidraw-api/clipboard"}` and `elements` is an array (so a whole `.excalidraw` file pasted as text also works). Otherwise treated as text (→ new text element), image files, or (web only) Mermaid/spreadsheet.

`addElementsFromPasteOrLibrary(opts)` (components/App.tsx ~L4884):
1. `elements = restoreElements(opts.elements, null, { deleteInvisibleElements: true })` (no binding repair).
2. Target point: `position` object → its client coords; `"cursor"` → last pointer viewport position; `"center"` → viewport center (`state.width/2 + offsetLeft`, same for y); converted to scene coords. Keyboard paste uses the cursor; library-menu insert uses center.
3. `duplicateAtSceneCoords(elements, {x, y}, {retainSeed})` (App.duplicate.ts):
   ```
   [minX,minY,maxX,maxY] = getCommonBounds(elements)
   dx = x - (maxX-minX)/2 ; dy = y - (maxY-minY)/2
   [gx, gy] = getGridPoint(dx, dy, effectiveGridSize)   // snap only if grid mode on
   each el: x' = el.x + gx - minX ; y' = el.y + gy - minY   // bbox centered on target point
   duplicateElements({type:"everything", elements, randomizeSeed: !retainSeed})
   ```
   `retainSeed` is true for plain paste (Ctrl+Shift+V) and some library/drop paths, otherwise seeds are re-randomized.
4. New elements appended on top (`[...prev, ...dup]`), `syncMovedIndices` gives them indices after the current top element.
5. If the target point is inside a top-level frame, eligible elements are added to that frame (`addElementsToFrame`).
6. `addMissingFiles(opts.files)`; the pasted elements become the selection (`getSelectionStateForElements`).

`duplicateElement` (element/src/duplicate.ts): deep copy; `id = randomId()`; `updated = created = now`; if randomizeSeed: `seed = randomInteger()` and bumpVersion; group ids remapped to fresh random ids (consistent within one operation; `getNewGroupIdsForDuplication` keeps groups above `editingGroupId`). `fixDuplicatedBindingsAfterDuplication` (element/src/binding.ts) then remaps references among the duplicates: `boundElements` entries → new ids (entries pointing to non-duplicated elements are **dropped**), `containerId` → new id or `null`, `startBinding`/`endBinding` → new elementId or `null`; elbow arrows re-routed. `frameId` likewise remapped when the frame is duplicated too.

Alt-drag duplication (`duplicateDraggedSelection`): the originals are reset to their drag-start position and the drag continues with the duplicates (which become the selection); duplicates get new seeds and are placed above the originals in z-order.

### 1b.10 Fractional `index` (packages/element/src/fractionalIndex.ts, packages/fractional-indexing/src/index.ts)

- Every element has `index: string | null` (`FractionalIndex`); saved in files. It is a lexicographically-ordered key (base-62 "order key", vendored `fractional-indexing` npm package, CC0, algorithm by David Greenspan). Must satisfy `elements[i-1].index < elements[i].index < elements[i+1].index` (plain string comparison, ASCII/code-unit order — Emacs `string<` is equivalent for these ASCII keys).
- **The array order is authoritative** for rendering and z-order; indices are kept in sync with it (used by collaboration/reconciliation and undo). A port that never collaborates could ignore them, but should still write valid indices so files open cleanly and merge in Excalidraw; on load Excalidraw fixes invalid ones anyway (`syncInvalidIndices`), so writing `null` is tolerated but causes the loading client to regenerate them.
- Key format: `BASE_62_DIGITS = "0-9A-Za-z"`. Integer part head char: `a`–`z` → length `head - 'a' + 2` (e.g. `a0` has length 2), `A`–`Z` → length `'Z' - head + 2`; fractional part must not end in `'0'`; key `A` + 26×`0` invalid. First key: `generateKeyBetween(null, null) = "a0"`, next ones `a1`, `a2`, … `az`, `b00`, ….
- Maintenance APIs:
  - `syncMovedIndices(elements, movedElementsMap)`: find contiguous runs of moved elements; for each run generate `n` keys strictly between the neighbours' indices with `generateNKeysBetween(lower?.index, upper?.index, n)`; validate the whole array; on any failure fall back to `syncInvalidIndices`. Use after insert, paste, duplicate, z-order change.
  - `syncInvalidIndices(elements)`: find runs of elements whose index is missing/malformed/out of order relative to the nearest valid neighbours (`getInvalidIndicesGroups`) and regenerate only those. Applied to every loaded scene.
  - `orderByFractionalIndex`: sort by index, ties by `id` (used when reconciling remote elements).
  - `generateNKeysBetween(a, b, n)`: `n=0 → []`; `n=1 → [between(a,b)]`; `b==null` → repeatedly `between(prev, null)`; `a==null` → repeatedly `between(null, prev)` then reverse; else recursive midpoint split.
- Full source of `midpoint`, `incrementInteger`, `decrementInteger`, `generateKeyBetween` is ~150 lines of pure string code; port verbatim from packages/fractional-indexing/src/index.ts.
- Bound text must stay directly above its container in the array (`normalizeBoundElementsOrder` on load; `validateFractionalIndices` can check `text.index > container.index`).

### 1b.11 Preserving out-of-scope elements (embeddable, iframe, magicframe)

- `restoreElement` handles `iframe`/`embeddable` with only base-field defaults, and `magicframe` like `frame` (`name ?? null`); all other properties (e.g. `customData.generationData` on iframes, `link` of embeddables) survive because the input element is spread first. A port should therefore keep **the entire parsed JSON object** of every element (including unknown keys and unknown element types if it wants to be forward-compatible — note upstream itself *drops* unknown types), only overlaying the fields it edits.
- Embeddable `link` is sanitized on load via `normalizeLink`.
- They are bindable targets (`isBindableElement` includes `iframe`, `embeddable`, `frame`, `magicframe`, and unbound `text`), use adaptive radius (iframe/embeddable), and `magicframe` counts as a frame (`isFrameLikeElement`) for membership/clipping/export. Render them as a placeholder rectangle (upstream draws a rounded rect with the link text when not rendering the iframe; exact placeholder style UNVERIFIED here).
- `activeTool` of `magicframe` is not restored (falls back to selection).

### 1b.12 UNVERIFIED items in this section
- Exact hex of `COLOR_PALETTE.black` / `.white` (commonly `#1e1e1e` / `#ffffff`).
- `COLOR_TOP_PICKS_SLOTS` value; `getNormalizedGridSize/Step` bounds; `FONT_FAMILY_GENERIC_FALLBACKS` ids.
- `normalizeStickyNote*` behaviour (stickynote is a new element type on master).
- Whether the save action passes deleted elements to `serializeAsJSON` (it does not filter them itself).
- Exact paste-path `retainSeed` values for library drag-drop (App.tsx ~L13161/13225/13314 pass `retainSeed: true`; menu insert L2805 uses `position: "center"`).

---

# 2. Rendering rules

## 2a. Shape rendering

Sources: `packages/element/src/shape.ts` (`generateRoughOptions`, `adjustRoughness`, `_generateElementShape`, arrowhead helpers, freedraw outline), `packages/element/src/bounds.ts` (`getArrowheadSize`, `getArrowheadAngle`, `getArrowheadPoints`, `getDiamondPoints`), `packages/element/src/utils.ts` (`getCornerRadius`, `isPathALoop`), `packages/element/src/typeChecks.ts`, `packages/element/src/comparisons.ts`, `packages/element/src/elbowArrow.ts`, `packages/element/src/heading.ts`, `packages/element/src/renderElement.ts`, `packages/element/src/stickyNote.ts`, `packages/excalidraw/renderer/staticScene.ts`, `packages/excalidraw/renderer/helpers.ts`, `packages/excalidraw/renderer/staticSvgScene.ts`, `packages/excalidraw/scene/export.ts`, `packages/common/src/constants.ts`, `packages/common/src/colors.ts`.

Library versions (`packages/excalidraw/package.json`): `roughjs` 4.6.4, `perfect-freehand` 1.2.0, `@excalidraw/laser-pointer` 1.3.1 (also vendored as `packages/laser-pointer`). Colors are parsed with `tinycolor2`.

### 2a.1 roughjs defaults (the values Excalidraw does not override)

From `rough-stuff/rough` `src/generator.ts` `defaultOptions`. Any option not set by `generateRoughOptions` falls back to these:

| option | default |
|---|---|
| maxRandomnessOffset | 2 |
| roughness | 1 |
| bowing | 1 (Excalidraw never sets it) |
| stroke | `#000` |
| strokeWidth | 1 |
| curveTightness | 0 |
| curveFitting | 0.95 (Excalidraw sets 1 for ellipse) |
| curveStepCount | 9 |
| fillStyle | `hachure` (Excalidraw always passes the element's fillStyle when there is a fill) |
| fillWeight | -1 (meaning strokeWidth/2) |
| hachureAngle | -41 (Excalidraw never sets it) |
| hachureGap | -1 (meaning strokeWidth*4) |
| dashOffset / dashGap / zigzagOffset | -1 |
| seed | 0 |
| disableMultiStroke | false |
| disableMultiStrokeFill | false (never set by Excalidraw) |
| preserveVertices | false |
| fillShapeRoughnessGain | 0.8 |

The fill styles `"hachure" | "cross-hatch" | "zigzag" | "solid"` are all passed directly to roughjs. Excalidraw has no fill code of its own, except for freedraw strokes, which it fills itself.

### 2a.2 `generateRoughOptions(element, continuousPath = false, isDarkMode = false)` (shape.ts), verbatim logic

```ts
const getDashArrayDashed = (strokeWidth) => [8, 8 + strokeWidth];
const getDashArrayDotted = (strokeWidth) => [1.5, 6 + strokeWidth];

options = {
  seed: element.seed,
  strokeLineDash: strokeStyle === "dashed" ? [8, 8 + sw]
                : strokeStyle === "dotted" ? [1.5, 6 + sw] : undefined,
  disableMultiStroke: element.strokeStyle !== "solid",
  strokeWidth: strokeStyle !== "solid" ? sw + 0.5 : sw,
  fillWeight: sw / 2,
  hachureGap: sw * 4,
  roughness: adjustRoughness(element),
  stroke: applyDarkModeFilter(element.strokeColor, isDarkMode),
  preserveVertices: continuousPath || element.roughness < ROUGHNESS.cartoonist, // < 2
};
switch (type) {
  rectangle | iframe | embeddable | diamond | ellipse:
    options.fillStyle = element.fillStyle;
    options.fill = isTransparent(bg) ? undefined : applyDarkModeFilter(bg, isDarkMode);
    if ellipse: options.curveFitting = 1;
  line | freedraw:
    if (isPathALoop(element.points)) {           // only closed paths get a fill
      options.fillStyle = element.fillStyle;
      options.fill = bg === "transparent" ? undefined : applyDarkModeFilter(bg, isDarkMode);
    }
  arrow: no fill
  default: throw
}
```

Note that line/freedraw compares `=== "transparent"`, while shapes call `isTransparent()` (alpha == 0 through tinycolor).

`continuousPath = true` is passed for the rounded rectangle path, the rounded diamond path, and elbow arrows. So those always get `preserveVertices: true`.

`adjustRoughness(element)` reduces roughness for small elements:
```ts
const maxSize = max(w, h), minSize = min(w, h);
if ((minSize >= 20 && maxSize >= 50) ||
    (minSize >= 15 && !!element.roundness && canChangeRoundness(element.type)) ||
    (isLinearElement(element) && maxSize >= 50))
  return roughness;
return Math.min(roughness / (maxSize < 10 ? 3 : 2), 2.5);
```

Constants (`common/constants.ts`):

| constant | value |
|---|---|
| `ROUGHNESS` | `{architect: 0, artist: 1, cartoonist: 2}` |
| `STROKE_WIDTH` | `{thin: 1, medium: 2, bold: 4, extraBold: 8}`; `DEFAULT_ELEMENT_STROKE_WIDTH_KEY = "medium"` |
| `DEFAULT_ELEMENT_PROPS` | `{strokeColor: "#1e1e1e", backgroundColor: "transparent", fillStyle: "solid", strokeWidth: 2, strokeStyle: "solid", roughness: 1, opacity: 100, locked: false}` |
| `COLOR_PALETTE` | `black "#1e1e1e"`, `white "#ffffff"`, `transparent "transparent"` |
| `LINE_CONFIRM_THRESHOLD` | 8 (px) |
| `LINE_POLYGON_POINT_MERGE_DISTANCE` | 20 |
| `DEFAULT_STROKE_STREAMLINE` | 0.5 |

Note that the default fillStyle is now `"solid"`, not hachure.

### 2a.3 Roundness

`ROUNDNESS = { LEGACY: 1, PROPORTIONAL_RADIUS: 2, ADAPTIVE_RADIUS: 3 }`. Related constants: `DEFAULT_PROPORTIONAL_RADIUS = 0.25` and `DEFAULT_ADAPTIVE_RADIUS = 32`.

`getCornerRadius(x, element)` (utils.ts), where x is usually `min(w, h)`:
```ts
if (type === PROPORTIONAL_RADIUS || type === LEGACY) return x * 0.25;
if (type === ADAPTIVE_RADIUS) {
  const fixed = element.roundness?.value ?? 32;
  const CUTOFF_SIZE = fixed / 0.25;          // 128 by default
  return x <= CUTOFF_SIZE ? x * 0.25 : fixed;
}
return 0;
```

Which roundness applies to which type (typeChecks.ts):
- `isUsingAdaptiveRadius`: rectangle, embeddable, iframe, image.
- `isUsingProportionalRadius`: line, arrow, diamond, stickynote.
- `canApplyRoundnessTypeToElement(t, el)`: ADAPTIVE or LEGACY with an adaptive type returns true; PROPORTIONAL with a proportional type returns true; everything else returns false.
- `getDefaultRoundnessTypeForElement`: a proportional type gets `{type: 2}`, an adaptive type gets `{type: 3}`, anything else gets `null`.
- `canChangeRoundness(type)` (comparisons.ts), which controls whether the "Edges" UI appears: rectangle, iframe, embeddable, line, diamond, stickynote, image.
  - It does **not** include ellipse, arrow, freedraw, or text.
  - Arrows use the arrow-type picker instead (sharp/round/elbow; `ARROW_TYPE`), where "round" means `roundness: {type: 2}`.

Rounded rectangle path (`_generateElementShape`, rectangle case; `r = getCornerRadius(min(w, h), el)`). It uses quadratic corners and is passed to `generator.path(d, opts(continuousPath = true))`:
```
M r 0 L w-r 0 Q w 0, w r L w h-r Q w h, w-r h L r h Q 0 h, 0 h-r L 0 r Q 0 0, r 0
```
A sharp rectangle uses `generator.rectangle(0, 0, w, h, opts)`.

Diamond points (`getDiamondPoints`, bounds.ts). The "+1" avoids zero values that make rough.js throw.
```
topX = floor(w/2)+1, topY = 0;  rightX = w, rightY = floor(h/2)+1;
bottomX = topX, bottomY = h;    leftX = 0, leftY = rightY
```
- Sharp diamond: `generator.polygon([[top],[right],[bottom],[left]], opts(false))`.
- Rounded diamond:
  - `verticalRadius = getCornerRadius(|topX-leftX|, el)` and `horizontalRadius = getCornerRadius(|rightY-topY|, el)`.
  - It is drawn with cubic corners whose two control points both sit on the vertex, via `generator.path(d, opts(true))`.
  - The path string is quoted verbatim below. The asymmetry is preserved exactly as it appears in the source:
```
M topX+vr topY+hr L rightX-vr rightY-hr
C rightX rightY, rightX rightY, rightX-vr rightY+hr
L bottomX+vr bottomY-hr
C bottomX bottomY, bottomX bottomY, bottomX-vr bottomY-hr
L leftX+vr leftY+hr
C leftX leftY, leftX leftY, leftX+vr leftY-hr
L topX-vr topY+hr
C topX topY, topX topY, topX+vr topY+hr
```

Ellipse: `generator.ellipse(w/2, h/2, w, h, opts(false))`, with `curveFitting = 1`.

Images use `getCornerRadius(min(w, h))` (adaptive) with canvas `roundRect` + `clip()`. This is covered in 2a.7.

### 2a.4 Line / arrow shapes (`_generateElementShape`, `case "line": case "arrow":`)

- `points = element.points.length ? element.points : [[0, 0]]`, and `options = generateRoughOptions(el, false, dark)`.
- **Elbow arrow** (`elbowed: true`):
  - It is drawn as `generator.path(generateElbowArrowShape(points, 16), generateRoughOptions(el, true, dark))`.
  - If any |coordinate| > 1e6, the arrow is not rendered (`shape = []`).
  - `generateElbowArrowShape(points, radius = 16)` rounds every interior corner with `corner = min(16, dist(p, next)/2, dist(p, prev)/2)`. For each interior point it emits `L (point moved back toward prev by corner)` and then `Q point, (point moved toward next by corner)`. The path starts with `M p0` and ends with `L pLast`.
- **No roundness**: `generator.polygon(points, opts)` when `options.fill` is set (only closed loop lines), otherwise `generator.linearPath(points, opts)`.
- **With roundness** (any non-null roundness): `generator.curve(points, opts)`, which is roughjs's Catmull-Rom-like curve through the points with `curveTightness` 0.
- The curve is always `shape[0]`. Arrowheads are appended only for `type === "arrow"`:
  - destructure `{startArrowhead = null, endArrowhead = "arrow"}`,
  - call `getArrowheadShapes(el, shape, "start" | "end", head, generator, options, canvasBackgroundColor, isDarkMode)` for each non-null head.
- **Polygon lines**: `line.polygon: true` is valid when `isValidPolygon(points)`, meaning `points.length > 3 && first == last`. `canBecomePolygon` requires `len > 3 || (len === 3 && first != last)`.
  - Rendering does not look at `polygon` directly. The fill applies whenever `isPathALoop(points)`: `len >= 3 && dist(first, last) <= 8 / zoom` (zoom = 1 for rendering).
  - The merge distance used while editing is `LINE_POLYGON_POINT_MERGE_DISTANCE = 20` (see the interaction section).
- Canvas state for linear and shape elements: `lineJoin = "round"`, `lineCap = "round"` (`drawElementOnCanvas`).

### 2a.5 Arrowheads

Type (`types.ts`): `Arrowhead = "arrow" | "bar" | "circle" | "circle_outline" | "triangle" | "triangle_outline" | "diamond" | "diamond_outline" | CardinalityArrowhead`. `CardinalityArrowhead` covers `cardinality_one`, `cardinality_many`, `cardinality_one_or_many`, `cardinality_exactly_one`, `cardinality_zero_or_one`, and `cardinality_zero_or_many`. The member list is inferred from the switch in `getArrowheadShapes` and `getArrowheadSize`; the literal union was not quoted.

Legacy values are normalized on load by `normalizeArrowhead` (arrowheads.ts):
- `dot` → `circle`
- `crowfoot_one` → `cardinality_one`
- `crowfoot_many` → `cardinality_many`
- `crowfoot_one_or_many` → `cardinality_one_or_many`
- `undefined` / `null` → `null`

`getArrowheadSize` (px) and `getArrowheadAngle` (degrees), from bounds.ts:

| arrowhead | size | angle |
|---|---|---|
| arrow | 25 | 20 |
| bar | 15 | 90 |
| diamond, diamond_outline | 12 | 25 |
| cardinality_many, _one_or_many, _zero_or_many | 15 (`CROWFOOT_ARROWHEAD_SIZE`) | 25 |
| cardinality_one, _exactly_one, _zero_or_one | 20 (`CARDINALITY_MARKER_SIZE`) | 25 |
| everything else (circle*, triangle*) | 15 | 25 |

**`getArrowheadPoints(el, shape, position, arrowhead, offsetMultiplier = 0)`** (bounds.ts):
1. `ops = getCurvePathOps(shape[0])`. These are the roughjs ops of the first drawn path; every segment is a `bcurveTo` (6 numbers).
2. `index = start ? 1 : ops.length - 1`, so `[p1, p2, p3]` = that op's control points. `p0` is the preceding `move` or the previous `bcurveTo`'s end point.
3. Tip: `(x2, y2) = start ? p0 : p3`.
4. Direction sample: `(x1, y1) = B(0.3)`, evaluated with the source's (swapped) formula:
   `(1-t)^3*p3 + 3t(1-t)^2*p2 + 3t^2(1-t)*p1 + t^3*p0`.
   Direction: `n = normalize((x2, y2) - (x1, y1))`.
5. `length` = length of the last model segment: `points[n-1] - points[n-2]` for end, `points[0] - points[1]` for start. If there is only one point, `[0, 0]` is used.
6. `lengthMultiplier = (diamond | diamond_outline) ? 0.25 : 0.5`, and `minSize = min(size, length * lengthMultiplier)`. So arrowheads shrink on short last segments. **Size does not depend on strokeWidth**, except the circle diameter.
7. `tx = x2 - nx*minSize*offsetMultiplier`, `ty = y2 - ny*minSize*offsetMultiplier`, and `xs = tx - nx*minSize`, `ys = ty - ny*minSize`.
8. Result by head type:
   - Circle types: return `[tx, ty, diameter = hypot(ys-ty, xs-tx) + strokeWidth - 2]`.
   - `cardinality_many` / `cardinality_one_or_many` (the base is swapped): `x3, y3 = rotate((tx, ty), around (xs, ys), -angle)`, `x4, y4 = rotate((tx, ty), around (xs, ys), +angle)`, returning `[xs, ys, x3, y3, x4, y4]`.
   - All other types: `x3, y3 = rotate((xs, ys), around (tx, ty), -angle)` and `x4, y4 = rotate(.., +angle)`.
   - Diamond also computes the opposite point `o`, which is `(tx ± 2*minSize, ty)` rotated about `(tx, ty)` by the angle toward the neighboring point. It returns `[tx, ty, x3, y3, ox, oy, x4, y4]`.
   - Everything else returns `[tx, ty, x3, y3, x4, y4]`.

Porting note: because the direction comes from the roughjs bezier, a jittered rough stroke subtly changes the arrowhead direction. A native port without roughjs op output can use the last-segment direction, or the curve tangent at t ≈ 0.3 from the end.

**`getArrowheadShapes`** (shape.ts):
- Colors: `strokeColor = applyDarkModeFilter(el.strokeColor, dark)`, `backgroundFillColor = applyDarkModeFilter(canvasBackgroundColor, dark)`. The "outline" variants are therefore filled with the canvas background color, not transparent.
- Line options (`getArrowheadLineOptions`): a copy of the options with `roughness = min(1, roughness)`. For dotted strokes it uses `dash = getDashArrayDotted(sw - 1)` and then `strokeLineDash = [dash[0], dash[1] - 1]`. Otherwise `strokeLineDash` is deleted, so the head is solid.

| head | geometry |
|---|---|
| `arrow`, `bar`, default | `generateArrowheadLinesToTip`: 2 lines `(x3,y3)->(tip)` and `(x4,y4)->(tip)` with line options. `bar` has angle 90, so it becomes a perpendicular bar. |
| `circle` / `circle_outline` | `generator.circle(tx, ty, diameter*scale, {...options, fill, fillStyle: "solid", stroke: strokeColor, roughness: min(0.5, r)})`, no dash. `fill` = strokeColor (circle) or canvas bg (outline). |
| `triangle` / `triangle_outline` | `generator.polygon([[tip],[x3],[x4],[tip]], {...options, fill: stroke or bg, fillStyle: "solid", roughness: min(1, r)})`, dash deleted |
| `diamond` / `diamond_outline` | polygon `[tip, x3, o, x4, tip]`, with the same option rules as triangle |
| `cardinality_one` | one line `(x3,y3)-(x4,y4)`, i.e. a bar across the shaft at the tip |
| `cardinality_many` | lines-to-tip using the swapped crow's-foot points (the feet spread at the tip) |
| `cardinality_one_or_many` | crow's foot plus a `cardinality_one` bar at `offsetMultiplier = -0.25` |
| `cardinality_exactly_one` | two `cardinality_one` bars, at offsets -0.5 and 0 |
| `cardinality_zero_or_one` | outline circle at `circle_outline` offset 1.5, diameter scale 0.8, fill canvas bg; plus a `cardinality_one` bar at -0.5 |
| `cardinality_zero_or_many` | crow's foot plus the same outline circle (offset 1.5, scale 0.8) |

### 2a.6 Freedraw

Element fields:
- `points`, `pressures`, `simulatePressure`.
- `strokeOptions?: {variability: "constant" | "variable" (StrokeVariability), streamline: number}`. The literal union was inferred from the code, not quoted.

Shape (`_generateElementShape`, `case "freedraw"`) is `[optional background, stroke]`:
1. If `isPathALoop(points)`, it adds a fill: `generator.curve(simplify(points, 0.75), {...generateRoughOptions(el, false, dark), stroke: "none"})` (`getFreedrawFillCurvePoints`).
2. The stroke is an SVG path string: `getSvgPathFromStroke(getFreedrawOutlinePoints(el))`. It is drawn as `context.fillStyle = applyDarkModeFilter(strokeColor, dark); context.fill(new Path2D(d))`. It is filled, not stroked.

`getFreedrawOutlinePoints` dispatches on `strokeOptions?.variability === "constant"`. Anything else, including an absent `strokeOptions`, uses the variable path:
- **Variable** (perfect-freehand `getStroke`):
  - Input points: `simulatePressure ? points : points.map(([x, y], i) => [x, y, pressures[i]])`. If there are no points, the input is `[[0, 0, 0.5]]`.
  - Options:
    ```
    { simulatePressure: el.simulatePressure,
      size: el.strokeWidth * 4.25,           // VARIABLE_WIDTH_FREEDRAW.SIZE_FACTOR
      thinning: 0.6, smoothing: 0.5,
      streamline: el.strokeOptions?.streamline ?? 0.5,   // DEFAULT_STROKE_STREAMLINE
      easing: t => Math.sin(t * Math.PI / 2),
      last: true }
    ```
- **Constant**: a `LaserPointer` (from `@excalidraw/laser-pointer`) with `{size: strokeWidth * 1.4, streamline: <same>, simplify: 0, sizeMapping: d => max(0.1, d.pressure)}`. Every point is added as `[x, y, 1]`, and `getStrokeOutline()` produces the outline. The module also defines `CONSTANT_WIDTH_FREEDRAW.COLLISION_SIMPLIFY_TOLERANCE = 0.2`.

`getSvgPathFromStroke(points)`:
- Quadratic midpoint smoothing: `["M", p0, "Q", then for each i: p_i, mid(p_i, p_{i+1})]`.
- For the last point: `p_last, mid(p_last, p0), "L", p0, "Z"`.
- The result is joined with spaces, and numbers are truncated to 2 decimals with the regex `/(\s?[A-Z]?,?-?[0-9]*\.[0-9]{0,2})(([0-9]|e|-)*)/g → "$1"`.

In SVG export, the freedraw stroke is a `<path fill=strokeColor d=...>` with the wrapper `stroke="none"` (staticSvgScene.ts).

Canvas padding for element bitmap caches (`getCanvasPadding`): freedraw `strokeWidth * 12`, text `fontSize / 2`, arrow 40 (the arrowhead check is written as `endArrowhead || endArrowhead`, a source typo), everything else 20. A native port only needs this if it caches per-element bitmaps.

### 2a.7 Images (`drawElementOnCanvas` case "image", `drawImagePlaceholder`)

- **Transform order when exporting** (`drawElement`): translate to the center, `rotate(angle)`, then `scale(element.scale[0], element.scale[1])`. The scale must come after rotation. Then translate by `-shift` and draw. `scale` is `[±1, ±1]` for flips.
  - SVG export: `transform="translate(cx cy) scale(sx sy) translate(-cx -cy)"` on a `<use>` of a `<symbol>`.
- **Rounded image**: if `element.roundness`, then `roundRect(0, 0, w, h, getCornerRadius(min(w, h), el))` + `clip()`. This uses adaptive radius by default. SVG uses a clipPath rect with `rx` / `ry`.
- **Crop**: `element.crop = {x, y, width, height, naturalWidth, naturalHeight}` is the source rectangle in natural image pixels. The call is `drawImage(img, crop.x, crop.y, crop.width, crop.height, 0, 0, el.width, el.height)`. Without a crop, the source is the full `naturalWidth` / `naturalHeight`. SVG uses a `<mask id="mask-image-crop-<id>">`.
  - While cropping (`appState.croppingElementId`), the uncropped image is also drawn at `globalAlpha 0.1`.
- **Dark mode**: only **SVG** images (`mimeType === "image/svg+xml"`) are inverted, using the CSS filter `DARK_THEME_FILTER = "invert(93%) hue-rotate(180deg)"`. Safari instead inverts the pixels manually with `255 - c`. Raster images are never altered.
- **Placeholder** (image not loaded, status pending, or error):
  - Fill the rectangle with `#E7E7E7` (light) or `#2E2E2E` (dark).
  - Then draw a centered icon with `size = min(minWH, min(minWH * 0.4, 100))`: the Font Awesome "image" glyph with fill `#888`, or for `status === "error"` a variant with a "ban" circle overlay. Both SVGs are embedded verbatim in `renderElement.ts` (`IMAGE_PLACEHOLDER_IMG`, `IMAGE_ERROR_PLACEHOLDER_IMG`).
  - `IMAGE_RENDER_TIMEOUT = 500` ms.
- Supported image MIME types (`IMAGE_MIME_TYPES`): svg `image/svg+xml`, png, jpeg, gif, webp, bmp, ico `image/x-icon`, avif, jfif.

### 2a.8 Frames

`FRAME_STYLE` (common/constants.ts):
```
strokeColor "#bbb", strokeWidth 2, strokeStyle "solid", fillStyle "solid", roughness 0,
roundness null, backgroundColor "transparent", radius 8, nameOffsetY 3,
nameColorLightTheme "#999999", nameColorDarkTheme "#7a7a7a", nameFontSize 14, nameLineHeight 1.25
```

- **Border** (`drawElement` case frame/magicframe): drawn only if `appState.frameRendering.enabled && frameRendering.outline`.
  - Style: `lineWidth = 2 / zoom`, `strokeStyle = applyDarkModeFilter("#bbb", dark)`, `roundRect(0, 0, w, h, 8 / zoom)`, stroke only.
  - Border width and radius are therefore **constant in screen pixels**, not scene units.
  - A `fillStyle "rgba(0,0,200,0.04)"` is set but never used for a fill.
  - Magic frames (out of scope) use `#7affd7`.
- **Frame highlight** while dragging into a frame (`interactiveScene.ts renderFrameHighlight`): stroke `getThemedColor("rgb(0,118,255)")`, width `2 / zoom`, radius `8 / zoom`, rotated with the frame.
- **Clipping of children** (`staticScene.ts frameClip`): `roundRect(frame.x, frame.y, w, h, 8 / zoom)` + `clip()`. It applies when `frameRendering.enabled && frameRendering.clip` and one of the following holds:
  - `shouldApplyFrameClip` (frame.ts) is true: the element intersects the frame, or contains the frame (a background element).
  - A grouped element sits outside the frame bounds but its group is in the frame (checked by `frameId` when not dragging, or by geometry while dragging).
  - The element is in the frame and either it or the frame has a render offset.

  SVG export uses a `<clipPath>` rect with `rx = ry = 8`, the unscaled scene radius.
- **Opacity**: a child's alpha is `frameOpacity * elementOpacity / 10000` (they multiply). See 2a.9.
- **Title on the interactive canvas** (`App.tsx renderFrameNames`): a DOM element, not drawn on the canvas.
  - Style: `fontFamily: "Assistant"`, `fontSize: 14` (screen px, not scaled by zoom), color `#999999` (light) or `#7a7a7a` (dark).
  - Position: `bottom = viewportHeight + nameOffsetY(3) - y1 + offsetTop`, i.e. 3px above the frame's top edge in screen space, left-aligned to the frame's left `x1`. `maxWidth = frame.width * zoom`, with overflow ellipsis (UNVERIFIED CSS detail).
  - Title text is `getFrameLikeTitle`: `element.name` if non-null, else `DEFAULT_FRAME_NAME` (value UNVERIFIED; expected `"Frame"`).
- **Title in export** (`scene/export.ts addFrameLabelsAsTextElements`):
  - A text element is synthesized with `x = frame.x`, `y = frame.y - 3 - textHeight`, `fontFamily = FONT_FAMILY.Helvetica (2)`, `fontSize 14`, `lineHeight 1.25`, and color `#7a7a7a` if `exportWithDarkMode`, else `#999999`.
  - The text is truncated to the frame width by removing characters and appending `"..."` until it fits (`truncateText`).
  - `getFrameRenderingConfig`: when exporting a single frame, `{enabled: true, outline: false, name: false, clip: true}`; otherwise the appState values are used.

### 2a.9 Opacity and render state (`renderElement.ts`)

- `resolveElementRenderState` computes `opacity = clamp(frame.opacity, 0, 100) * clamp(el.opacity, 0, 100) / 10000`.
  - `frame.opacity` is that of the containing frame, or 100 if there is none.
  - The result is multiplied by `ELEMENT_READY_TO_ERASE_OPACITY / 100 = 0.2` if the element, or its frame, is pending erasure.
- `renderElement` sets `context.globalAlpha = opacity * (0.3 if the element-link-selector dialog is open and the element is not selected or hovered)`. That factor is `DEFAULT_REDUCED_GLOBAL_ALPHA = 0.3`.
- SVG export sets `stroke-opacity` and `fill-opacity` to the same frame × element product.

### 2a.10 Arrow bound-text hole

When an arrow has a bound label, the arrow is drawn with an **even-odd clip** that cuts out the label rectangle:
- Hole: `label bbox ± BOUND_TEXT_PADDING (5)`, axis-aligned in scene space.
- Outer rect: `outerHalf = max(|x2 - x1|, |y2 - y1|) + canvasPadding * 10`.

This keeps elements beneath the arrow visible through the gap (`drawElement`, export path).

### 2a.11 Sticky note (new element type, `type: "stickynote"`, extra field `baseHeight: number`)

Constants (common/constants.ts):

| constant | value |
|---|---|
| `STICKY_NOTE_MIN_FONT_SIZE` | 16 |
| `STICKY_NOTE_MAX_FONT_SIZE` | 512 |
| `STICKY_NOTE_FALLBACK_FONT_SIZE` | 28 |
| `STICKY_NOTE_FONT_STEP` | 2 |
| `STICKY_NOTE_PADDING` | 16 |
| `STICKY_NOTE_FOOTER` | `{height: 20, fontSize: 12, fontFamily: "Helvetica, Arial, sans-serif", baselineFromBottom: 14, opacity: 1, minBodyWidthForYear: 80}` |
| `STICKY_NOTE_BODY_INSET_Y` | `16*2 + 20` = 52 |
| `DEFAULT_STICKY_NOTE_SIZE` | 250 |
| `STICKY_NOTE_MIN_SIZE` | 75 |
| `STICKY_NOTE_SHADOW_OFFSET` | 3 |
| `STICKY_NOTE_SHADOW_OPACITY` | 0.16 |
| `STICKY_NOTE_EDGE_SHADOW_WIDTH` | 0.5 |
| `STICKY_NOTE_EDGE_SHADOW_OPACITY` | 0.08 |

Module-private constants in stickyNote.ts: `STICKY_NOTE_RENDER_ROUGHNESS = [0, 1.5, 8]`, `STICKY_NOTE_CORNER_RADIUS_RATIO = 0.04`, `STICKY_NOTE_MAX_CORNER_RADIUS = 16`.

Rendering (`drawElementOnCanvas` case "stickynote"). It does not use roughjs; everything is plain canvas paths:
1. Shadow: fill `rgba(0,0,0,0.16)` with `getStickyNotePathCommands(el, {shadow: true})`. The shadow points are offset by (3, 3), and the jitter uses `seed + 1`.
2. Body: fill with `applyDarkModeFilter(backgroundColor, dark)`.
3. Edge: clip to the body path, then stroke that path with `lineWidth 1` (0.5*2) and `rgba(0,0,0,0.08)`, giving an inner edge shadow.
4. Footer (date label):
   - Shown only if w and h are both ≥ 75.
   - Text: `"<day> <Mon>"`, plus `" <year>"` when the year differs from the current one and the body width (`w - 32`) is ≥ 80.
   - Position: `x = w - 16`, `y = h - 14`, right-aligned, alphabetic baseline.
   - Font: `12px Helvetica, Arial, sans-serif`, color `applyDarkModeFilter(strokeColor)`.
   - Built from `created`, using the English month abbreviations `Jan`..`Dec`.

Geometry (stickyNote.ts):
- Corner points (`getStickyNoteRenderPoints`): `roughness = clamp(round(el.roughness), 0, 2)` and `amount = min([0, 1.5, 8][roughness], min(w, h) * 0.012)`. Each of the 4 corners is jittered by `(rand*2 - 1) * amount` in x and y, using the `seededRandom(seed + seedOffset)` sequence in the order TL, TR, BR, BL (x then y).
- Corner radius: `roundness ? min(min(w, h) * 0.04, 16) : 0`.
- Path (`getStickyNotePathCommands`): each corner is rounded with a quadratic curve whose control point is the corner, with `cornerRadius = min(radius, halfPrevEdge, halfNextEdge)`.
- Lifted corner, used only when `roughness === 2`: corner index `floor(seededRandom(seed)() * 4)` curls.
  - `reach = min(size * 0.18, 40)` and `lift = min(size * 0.02, 5)` (halved for the shadow), where `size = min(w, h)`.
  - The tip is moved diagonally inward by `lift`. The corner is drawn as three quadratics.
- Sticky notes accept proportional roundness, have a background but no fillStyle (`hasFillStyle` excludes them), do have roughness, and have no strokeWidth or strokeStyle.
- Font auto-fit and layout: `getStickyNoteLayout`, `relayoutStickyNotes`, `normalizeStickyNoteFontSize` (details UNVERIFIED; outside this section).

### 2a.12 Elbow arrow routing (elbowArrow.ts)

Constants:
- `BASE_PADDING = 40`, `DEDUP_TRESHOLD = 1` (sic), `MAX_POS = 1e6`.
- From binding.ts: `BASE_BINDING_GAP = 5` and `getBindingGap(el) = 5 + el.strokeWidth / 2`.

Headings (heading.ts): `RIGHT [1,0]`, `DOWN [0,1]`, `LEFT [-1,0]`, `UP [0,-1]`.
- `vectorToHeading([x, y])`: `x > |y|` → RIGHT; `x <= -|y|` → LEFT; `y > |x|` → DOWN; else UP.

Pipeline (`updateElbowArrowPoints`):

0. **Early exits and validation**:
   - If there are fewer than 2 points, return the points unchanged.
   - If a binding points to a missing element, or there are no elements, just `normalizeArrowElementUpdate` the global points.
1. **Renormalize** when no points, fixedSegments, or bindings are in the update (`handleSegmentRenormalization`). This merges collinear segments and drops points within `DEDUP_TRESHOLD`.
2. **No fixedSegments**: the path is `normalizeArrowElementUpdate(getElbowArrowCornerPoints(removeElbowArrowShortSegments(routeElbowArrow(...))))`.
3. The fixed-segment count decreased: `handleSegmentRelease`, which re-routes only the released part.
4. No points in the update (a segment was dragged): `handleSegmentMove`.
5. Both points and fixedSegments in the update (resize): the update is used as-is.
6. Otherwise, endpoints were dragged while segments are fixed: `handleEndpointDrag`.

**Headings for endpoints** (`getBindPointHeading` → binding.ts `getHeadingForElbowArrowSnap`):
- With no bindable element, the heading points toward the other endpoint.
- If the original point is within binding distance of the element, the heading comes from `headingForPointFromElement`. This is a quadrant test: triangles from the center to the AABB corners scaled ×2 (`SEARCH_CONE_MULTIPLIER = 2`), with a special case for diamonds.
- Otherwise the heading is the vector from the element center to the point.

**Obstacle boxes** (`getElbowArrowData`):
- Unbound endpoint: a ±2 px box around the point.
- Bound endpoint: `aabbForElement(el, offsetFromHeading(heading, gap * (arrowhead ? 6 : 2), 1))`. Here `offsetFromHeading(h, head, side)` returns `[up, right, down, left]`, with `head` on the heading side and `side` on the others.
- `boundsOverlap`: either endpoint lies inside the other element's box padded by `BASE_PADDING`.
- `commonBounds`: the AABB of both boxes (or of the point boxes when overlapping).
- `generateDynamicAABBs(a, b, common, startDiff, endDiff, disableSideHack = boundsOverlap, startElBounds, endElBounds)`:
  - Each box grows toward the other, either to the midpoint between the two boxes or by the padding (`BASE_PADDING - gap*(6|2)` on the heading side, `BASE_PADDING` on the other sides, or 0 if neither end is bound).
  - The "side hack" splits the boxes at the midlines when they overlap diagonally. The full code is long; port it verbatim from the source.

**Dongles**: `getDonglePosition(dynamicAABB, heading, p)` projects the endpoint onto the dynamic box edge in the heading direction. Routing runs between the dongles, and the real endpoints are then prepended and appended.

**Grid** (`calculateGrid`):
- X coordinates:
  - the start x if the start heading is vertical,
  - the end x if the end heading is vertical,
  - both x edges of every AABB,
  - both x edges of the common bounds.
- Y coordinates follow the same rule with horizontal headings and the y edges.
- Nodes sit at every (x, y) intersection, sorted into a 2D grid with 4-neighbors (up, right, down, left).

**A\*** (`astar`):
- `bendMultiplier = manhattan(start, end)`, and the open set is a binary heap keyed on `f`.
- A neighbor is skipped when:
  - the midpoint between current and neighbor lies inside any AABB,
  - it reverses the previous direction,
  - it is the start node and the move is opposite the start heading (`compareHeading(neighborHeading, startHeading)` at the start address),
  - it is the end node entered against the end heading.
- `g = g_cur + manhattan + (directionChange ? bendMultiplier^3 : 0)`.
- `h = manhattan(end, n) + estimateSegmentCount(n, end, heading, endHeading) * bendMultiplier^2`. `estimateSegmentCount` returns 0–4 by heading-pair case; the full table is in the source.
- Start and end nodes are pre-closed when bound or hovered. When the dongles overlap, the AABB list passed to A\* is empty.

**Post-processing**:
- `removeElbowArrowShortSegments`: with ≥ 4 points, drop interior points within 1 px of their predecessor.
- `getElbowArrowCornerPoints`: drop interior points that do not change the horizontal/vertical orientation.
- `normalizeArrowElementUpdate`: `x, y = first point`, points become local, everything is clamped to ±1e6, width/height are recomputed, and `fixedSegments` becomes `null` when empty.

`FixedSegment = {start: LocalPoint, end: LocalPoint, index: number}`. Elbow arrow extra fields: `elbowed: true`, `fixedSegments | null`, `startBinding` / `endBinding: FixedPointBinding | null`, `startIsSpecial` / `endIsSpecial: boolean | null`.

Elbow arrows are rendered with a 16 px corner radius (see 2a.4). Their roughness applies, but with `preserveVertices` set.

### 2a.13 Dark mode (**important change**: no CSS canvas filter anymore)

In current master, the canvas does **not** get a CSS `filter: invert()`. As far as `css/styles.scss` shows, there is no filter rule; that absence is checked only for this file, so treat it as UNVERIFIED that no other stylesheet sets one. Instead, **every color is transformed at render time** with `applyDarkModeFilter(color, isDark)` (common/colors.ts). This covers stroke, fill, text, arrowhead fills, canvas background, grid colors, sticky notes, and the frame border.

The algorithm emulates the CSS filter `invert(93%) hue-rotate(180deg)`:
```ts
// cssInvert, p = 0.93:  c' = round(clamp(c*(1-p) + (255-c)*p, 0, 255))
// cssHueRotate(deg = 180), on r,g,b in [0,1], a = deg->rad, c = cos a, s = sin a:
m = [0.213 + c*0.787 - s*0.213,  0.715 - c*0.715 - s*0.715,  0.072 - c*0.072 + s*0.928,
     0.213 - c*0.213 + s*0.143,  0.715 + c*0.285 + s*0.14,   0.072 - c*0.072 - s*0.283,
     0.213 - c*0.213 - s*0.787,  0.715 - c*0.715 + s*0.715,  0.072 + c*0.928 + s*0.072]
R' = r*m0+g*m1+b*m2 ; G' = r*m3+g*m4+b*m5 ; B' = r*m6+g*m7+b*m8 ; each clamp01*255, round
result = rgbToHex(R',G',B', alpha)  // "#rrggbb", or "#rrggbbaa" if alpha < 1; alpha is preserved
```
Results are cached per input string.

- Background (`helpers.ts bootstrapCanvas`): if `viewBackgroundColor` is not an opaque 3- or 6-digit hex, clear first. Then, unless it is `"transparent"`, fill with `applyDarkModeFilter(viewBackgroundColor, dark)`.
- Grid (`staticScene.ts strokeGrid`), colors:
  - light: bold `#dddddd`, regular `#e5e5e5`,
  - dark: `applyDarkModeFilter()` of those same values.

  Geometry:
  - Minor lines are dashed with `[lw*3, spaceWidth + (lw + spaceWidth)]`, where `spaceWidth = 1/zoom`. They are skipped when `gridSize*zoom < 10`.
  - A line is bold (major) when `gridStep > 1` and `round(pos - scroll) % (gridStep*gridSize) == 0`.
  - Bold lines are solid, with max width 4 CSS px (minor: 1). Line positions are snapped to device pixels. Bold lines are always drawn, even below the 10 px threshold.
- Images: SVG images are inverted with the CSS filter string `"invert(93%) hue-rotate(180deg)"`; raster images are unchanged.
- SVG export applies the same `applyDarkModeFilter` to attribute colors (staticSvgScene.ts).

## 2b. Text & fonts

All paths relative to `excalidraw/excalidraw@master` (fetched 2026-09-27).

### 2b.1 Font family ids (`packages/common/src/constants.ts`, `FONT_FAMILY`)

| id | name (CSS family string) | status (`font-metadata.ts` flags) | generic fallback (`getGenericFontFamilyFallback`) |
|---|---|---|---|
| 1 | `Virgil` | `deprecated` (old hand-drawn default) | sans-serif |
| 2 | `Helvetica` | `deprecated`, `local` (system font, never embedded) | sans-serif |
| 3 | `Cascadia` | `deprecated` | monospace |
| 4 | — (unused id) | | |
| 5 | `Excalifont` | current default (`DEFAULT_FONT_FAMILY = FONT_FAMILY.Excalifont`) | sans-serif |
| 6 | `Nunito` | | sans-serif |
| 7 | `Lilita One` | | sans-serif |
| 8 | `Comic Shanns` | | monospace |
| 9 | `Liberation Sans` | `private` (hidden from picker) | sans-serif |
| 10 | `Assistant` | `private` (NEW; not registered in `Fonts.ts` init list — appears to be the UI font, not a scene font; UNVERIFIED) | sans-serif |

Fallback pseudo-ids (`FONT_FAMILY_FALLBACKS`): `Xiaolai` = 100 (`CJK_HAND_DRAWN_FALLBACK_FONT`), `sans-serif` = 998, `monospace` = 999, `Segoe UI Emoji` = 1000 (`WINDOWS_EMOJI_FALLBACK_FONT`).

Fallback chain (`getFontFamilyFallbacks`):
- Excalifont → `[Xiaolai, <generic>, Segoe UI Emoji]`
- every other family → `[<generic>, Segoe UI Emoji]`

CSS font string (`packages/common/src/utils.ts`):
```ts
getFontFamilyString({fontFamily}) // "Excalifont, Xiaolai, sans-serif, Segoe UI Emoji"
  // loops Object.entries(FONT_FAMILY); if id not found returns WINDOWS_EMOJI_FALLBACK_FONT (!)
getFontString({fontSize, fontFamily}) = `${fontSize}px ${getFontFamilyString({fontFamily})}`
```
Surprise: an unknown numeric `fontFamily` renders as just `"Segoe UI Emoji"`, while metrics (`getLineHeight`, `getVerticalOffset`) fall back to Excalifont metrics.

Other text constants (constants.ts): `FONT_SIZES = {sm:16, md:20, lg:28, xl:36}`; `MIN_FONT_SIZE = 1`; `DEFAULT_FONT_SIZE = 20`; `DEFAULT_TEXT_ALIGN = "left"`; `DEFAULT_VERTICAL_ALIGN = "top"`; `TEXT_ALIGN = left|center|right`; `VERTICAL_ALIGN = top|middle|bottom`; `BOUND_TEXT_PADDING = 5`; `ARROW_LABEL_WIDTH_FRACTION = 0.7`; `ARROW_LABEL_FONT_SIZE_TO_MIN_WIDTH_RATIO = 11`; `TEXT_AUTOWRAP_THRESHOLD = 36` (px; "distance when creating text before it's considered `autoResize: false`" — i.e. dragging with the text tool wider than 36px creates a fixed-width wrapping text box); `DEFAULT_TRANSFORM_HANDLE_SPACING = 2`.

### 2b.2 Font metrics (`packages/common/src/font-metadata.ts`, `FONT_METADATA`)

Keyed by numeric id. Cross-checked by two separate fetches (verbatim entries for Excalifont, Nunito, Lilita One, Cascadia, Helvetica, Xiaolai).

| family | unitsPerEm | ascender | descender | lineHeight (default) | flags |
|---|---|---|---|---|---|
| Excalifont (5) | 1000 | 886 | -374 | 1.25 | — |
| Nunito (6) | 1000 | 1011 | -353 | 1.25 | — |
| Lilita One (7) | 1000 | 923 | -220 | 1.15 | — |
| Comic Shanns (8) | 1000 | 750 | -250 | 1.25 | — |
| Virgil (1) | 1000 | 886 | -374 | 1.25 | deprecated |
| Helvetica (2) | 2048 | 1577 | -471 | 1.15 | deprecated, local |
| Cascadia (3) | 2048 | 1900 | -480 | 1.2 | deprecated |
| Liberation Sans (9) | 2048 | 1854 | -434 | 1.15 | private |
| Assistant (10) | 2048 | 1021 | -287 | 1.25 | private |
| Xiaolai (100) | 1000 | 880 | -144 | 1.25 | fallback |
| Segoe UI Emoji (1000) | 1000 | 886 | -374 | 1.25 | local, fallback |

There is **no `DEFAULT_LINE_HEIGHT` constant** any more; the default is per family:
```ts
export const getLineHeight = (fontFamily) => {
  const { lineHeight } = FONT_METADATA[fontFamily]?.metrics
    || FONT_METADATA[FONT_FAMILY.Excalifont].metrics;
  return lineHeight;
};
```
`lineHeight` stored on the element is **unitless** (`number & {_brand:"unitlessLineHeight"}`); pixel line height = `fontSize * lineHeight` (`getLineHeightInPx`, textMeasurements.ts).

Baseline (`getVerticalOffset`, font-metadata.ts) — y of the first line's alphabetic baseline measured from the element's top:
```ts
const fontSizeEm = fontSize / unitsPerEm;
const lineGap = (lineHeightPx - fontSizeEm * ascender + fontSizeEm * descender) / 2;
verticalOffset = fontSizeEm * ascender + lineGap;
```
(i.e. the ascender+|descender| box is centered vertically in each line box). Example: Excalifont 20px, lh 1.25 → lineHeightPx 25, content 25.2, lineGap -0.1, verticalOffset 17.62.

### 2b.3 Font files, hosting, licenses (`packages/excalidraw/fonts/`)

Directory contents: `Assistant/ Cascadia/ ComicShanns/ Emoji/ Excalifont/ Helvetica/ Liberation/ Lilita/ Nunito/ Virgil/ Xiaolai/ ExcalidrawFontFace.ts Fonts.ts fonts.css index.ts`. No LICENSE / OFL.txt files are present in the directories; license info exists only as header comments in some `index.ts` files.

| dir | files | subsetting | license (source) |
|---|---|---|---|
| Excalifont | 7 × `Excalifont-Regular-<hash>.woff2` | unicode-range subsets: _0 basic Latin (`U+20-7e,U+a0-a3,...,U+2212`), _1 Latin ext, _2 Cyrillic, _3 Greek, _4/_6 combining marks, _5 Cyrillic ext | SIL OFL 1.1 (header comment in `Excalifont/index.ts`, generated by cn-font-split 5.2.2) |
| Virgil | `Virgil-Regular.woff2` | none | not stated in repo; upstream Virgil is OFL 1.1 — UNVERIFIED |
| Nunito | 5 × `Nunito-Regular-<google hash>.woff2` | `GOOGLE_FONTS_RANGES` CYRILIC_EXT, CYRILIC, VIETNAMESE, LATIN_EXT, LATIN | not stated; Google Fonts, OFL 1.1 — UNVERIFIED |
| Lilita | 2 × `Lilita-Regular-<hash>.woff2` | Latin-ext / Latin (Google ranges) | not stated; Google Fonts OFL 1.1 — UNVERIFIED |
| ComicShanns | 4 × `ComicShanns-Regular-<hash>.woff2` + `ComicShanns-Regular.sfd` (FontForge source) | _0 Latin+punct, _1 Latin ext, _2 combining/math/arrows, _3 only U+3BB (λ) | **MIT** (header: "Comic Shanns Mono-Regular", v1.3.0; © Shannon Miwa 2018, Jesus Gonzalez 2023, Rodrigo Batista de Moraes 2023, Fini Jastrow 2024, Kyle Beechly 2024) |
| Cascadia | `CascadiaCode-Regular.woff2` | none | not stated; Cascadia Code is OFL 1.1 — UNVERIFIED |
| Liberation | `LiberationSans-Regular.woff2` | none | not stated; Liberation Sans 2.x is OFL 1.1 — UNVERIFIED |
| Assistant | `Assistant-{Regular,Medium,SemiBold,Bold}.woff2` | none | not stated (Google Fonts OFL) — UNVERIFIED |
| Xiaolai | 208 font-face entries, `Xiaolai-Regular-<hash>.woff2` each ~37–81 KB | CJK subsets (chinese-font.netlify.app / cn-font-split 5.2.2) | **SIL OFL 1.1**, "Xiaolai SC", Version 3.11 (Dec 4 2020), © 2020 LXGW (header in `Xiaolai/index.ts`) |
| Helvetica | only `index.ts` → `{ uri: LOCAL_FONT_PROTOCOL }` | — | system font, never shipped |
| Emoji | only `index.ts` → `{ uri: LOCAL_FONT_PROTOCOL }` | — | system "Segoe UI Emoji" |

Registration (`Fonts.ts` `init`): `Cascadia, Comic Shanns, Excalifont, Helvetica, Liberation Sans, Lilita One, Nunito, Virgil, Xiaolai (CJK fallback), Segoe UI Emoji`. Assistant is not registered there.

Hosting (`ExcalidrawFontFace.ts`, `createUrls`): URL list = each of `window.EXCALIDRAW_ASSET_PATH` (string or array) + always the fallback `https://esm.sh/@excalidraw/excalidraw@<version>/dist/prod/` (`ASSETS_FALLBACK_URL`), resolved against the woff2 asset path. Loading uses the FontFace API; up to 10 faces concurrently; after load, shape caches are invalidated and scene re-rendered. For SVG export, `generateFontFaceDeclarations` inlines subsetted `@font-face` for used characters and skips `metadata.local` fonts (Helvetica, Segoe UI Emoji).

Port guidance: ship Excalifont (OFL), Nunito, Lilita One, Comic Shanns (MIT), Virgil, Cascadia Code, Liberation Sans, Xiaolai; map Helvetica to a system sans (e.g. Helvetica/Arial/Liberation Sans). Download woff2 from the repo or esm.sh and convert to TTF/OTF for Pango/fontconfig (woff2 decompression needed). Use the fallback chain above as a Pango font list (`"Excalifont, Xiaolai, sans-serif, Segoe UI Emoji"`).

### 2b.4 Text element fields & creation (`packages/element/src/types.ts`, `newElement.ts` `newTextElement`)

```ts
type ExcalidrawTextElement = _ExcalidrawElementBase & {
  type: "text"; fontSize: number; fontFamily: FontFamilyValues;
  baseFontSize: number | null;          // NEW (sticky-note font fitting base size)
  text: string;                          // wrapped text as displayed (contains soft line breaks)
  textAlign: "left"|"center"|"right"; verticalAlign: "top"|"middle"|"bottom";
  containerId: string | null;            // rectangle | stickynote | ellipse | diamond | arrow
  originalText: string;                  // unwrapped source text (re-wrapped on resize)
  autoResize: boolean;                   // true = width follows content, no wrapping
  lineHeight: number;                    // unitless
  labelPosition?: number | null;         // NEW: arrow labels only, 0..1 arc-length parameter
};
```
`newTextElement(opts)`: `fontFamily = opts.fontFamily || DEFAULT_FONT_FAMILY (5)`; `fontSize = opts.fontSize || 20`; `lineHeight = opts.lineHeight || getLineHeight(fontFamily)`; `text = normalizeText(opts.text)`; width/height = `measureText(...)`; `x = opts.x - width*ax`, `y = opts.y - height*ay` where anchor ratios (`getTextAnchorRatios`) are `x: center 0.5 / right 1 / left 0`, `y: middle 0.5 / bottom 1 / top 0` (the passed x,y is the alignment anchor point); `containerId || null`; `originalText ?? text`; `autoResize ?? true`; `labelPosition ?? null`; `baseFontSize ?? null`.

Restore (`packages/excalidraw/data/restore.ts`, `restoreElement` case "text"): deletes legacy `rawText`; legacy `font: "20px Virgil"` string is split → `fontSize = parseFloat`, `fontFamily = getFontFamilyByName(name)` (unknown name → DEFAULT_FONT_FAMILY 5); non-finite fontSize → 20; `text` non-string → `""`; `lineHeight = element.lineHeight || (element.height ? detectLineHeight(element) : getLineHeight(element.fontFamily))` where `detectLineHeight = height / lineCount / fontSize` (old files keep their effective line height); `textAlign || "left"`, `verticalAlign || "top"`, `containerId ?? null`, `originalText || text`, `autoResize ?? true`, `labelPosition` finite → clamp 0..1 else null, `baseFontSize` finite → `normalizeStickyNoteFontSize(...)` else null.

### 2b.5 Measurement (`packages/element/src/textMeasurements.ts`)

- `normalizeText(t) = normalizeEOL(t).replace(/\t/g, "        ")` — tabs → spaces (8 spaces per the fetch; exact count UNVERIFIED, 4 or 8). `normalizeEOL` = `replace(/\r?\n|\r/g, "\n")`.
- `measureText(text, font, lineHeight)`: empty lines are replaced by `" "` before measuring; `fontSize = parseFloat(font)`; `height = getTextHeight = lineCount * fontSize * lineHeight`; `width = max over lines of getLineWidth(line, font)`.
- `getLineWidth` = canvas 2D `context.font = fontString; context.measureText(text).width` (advance width; no DPR adjustment; test env ×10). Port: Pango logical width of the line in the same font list.
- `getMinTextElementWidth = measureText("", font, lh).width + BOUND_TEXT_PADDING*2` (≈ width of a space + 10).
- `getApproxMinLineWidth = maxCharWidth(cache) + 10` (fallback: width of `"ABC…Z0-9"` stacked one char per line + 10); `getApproxMinLineHeight = fontSize*lineHeight + 10`.
- `charWidth` cache per font string (used by wrapWord).

Height is always exactly `lines * fontSize * lineHeight` — never from font ascent/descent. Width is the max advance width.

### 2b.6 Wrapping (`packages/element/src/textWrapping.ts`)

`wrapText(text, font, maxWidth) = getWrappedTextLines(...).map(l => l.text).join("\n")`.
1. If `!Number.isFinite(maxWidth) || maxWidth < 0` → return hard lines unchanged.
2. Split on `\n` (hard breaks). A line is only processed if its measured width `> maxWidth`.
3. `wrapLine`: tokens = `line.normalize("NFC").split(breakLineRegex).filter(Boolean)`; greedily append tokens; when a token doesn't fit: if current line empty → `wrapWord` (split into chars via `Array.from`, character by character using cached char widths; emoji tokens are never split), else push current line and start a new one. Trailing whitespace at soft breaks is removed (`trimLineEndAtSoftBreak`); at a hard line end `trimLine` keeps trailing whitespace only as far as it fits in maxWidth.

Break regex (`getLineBreakRegexAdvanced`; if lookbehind unsupported → `getLineBreakRegexSimple = or(emoji, Break.On(HYPHEN, WHITESPACE, CJK.CHAR))`):
```
or( emojiRegex,
    Break.Before(WHITESPACE),
    Break.After(WHITESPACE, HYPHEN),
    Break.Before(CJK.CHAR, CJK.CURRENCY).NotPrecededBy(COMMON.OPENING, CJK.OPENING),
    Break.After(CJK.CHAR).NotFollowedBy(COMMON.HYPHEN, COMMON.CLOSING, CJK.CLOSING),
    Break.BeforeMany(CJK.OPENING).NotPrecededBy(COMMON.OPENING),
    Break.AfterMany(CJK.CLOSING).NotFollowedBy(COMMON.CLOSING),
    Break.AfterMany(COMMON.CLOSING).FollowedBy(COMMON.OPENING) )
```
Character classes: COMMON `WHITESPACE /\s/`, `HYPHEN -`, `OPENING <([{`, `CLOSING >)]}.,:;!?…/`; CJK `CHAR` = Han, Hiragana, Katakana, Hangul + `｀＇＾〃〰〆＃＆＊＋－ー／＼＝｜￤〒￢￣`; CJK `OPENING （［｛〈《｟｢「『【〖〔〘〚＜〝`; CJK `CLOSING ）］｝〉》｠｣」』】〗〕〙〛＞。．，、〟‥？！：；・〜〞`; `CURRENCY ￥￦￡￠＄`; EMOJI `FLAG \p{RI}\p{RI}`, `ZWJ ‍`, `MOST [\p{Extended_Pictographic}\p{Emoji_Presentation}]`.
Summary of rules: break before/after spaces, after hyphens, between any two CJK ideographs (except no break after an opening bracket or before a closing punctuation/hyphen), emoji clusters are atomic. Port: implement this token splitter in Elisp/C rather than relying on Pango's UAX#14 wrapping (results would differ).

When wrapping applies: `container` present → maxWidth = `getBoundTextMaxWidth(container)`; free text with `autoResize: false` → maxWidth = `element.width`; free text with `autoResize: true` → never wrapped (width grows).

### 2b.7 Bound text in containers (`packages/element/src/textElement.ts`)

Valid containers (`VALID_CONTAINER_TYPES`): `rectangle, stickynote, ellipse, diamond, arrow` (not `line`).

Max text box (`getBoundTextMaxWidth` / `getBoundTextMaxHeight`, P = BOUND_TEXT_PADDING = 5):

| container | max width | max height |
|---|---|---|
| rectangle | `width - 2P` | `height - 2P` |
| ellipse | `round(width/2 * √2) - 2P` | `round(height/2 * √2) - 2P` |
| diamond | `round(width/2) - 2P` | `round(height/2) - 2P` |
| arrow | `max(0.7 * width, (text.fontSize ?? 20) * 11)` | `height - 80 <= 0 ? text.height : height` |
| stickynote | `width - 2*STICKY_NOTE_PADDING` (16) | `max(0, height - STICKY_NOTE_BODY_INSET_Y)` (= 16*2+20 = 52) |

Top-left of the text box (`getContainerCoords`): offset = P (16 for stickynote); ellipse adds `(w/2)*(1-√2/2)`, `(h/2)*(1-√2/2)`; diamond adds `w/4`, `h/4`; result `container.x + offX, container.y + offY`.

Position (`computeBoundTextPosition`; arrows delegate to `LinearElementEditor.getBoundTextElementPosition`):
- y: top → `coords.y`; bottom → `coords.y + maxH - text.height`; middle → `coords.y + maxH/2 - text.height/2` (stickynote middle: `coords.y + min((h - 32 - text.height)/2, maxH - text.height)`).
- x: left → `coords.x`; right → `coords.x + maxW - text.width`; center → `coords.x + maxW/2 - text.width/2`.
- If container.angle ≠ 0: rotate the text center around the content center (`coords + maxW/2, maxH/2`; stickynote: container center) by the angle; bound text `angle = container.angle` (arrow labels: always 0, `getTextElementAngle`).

Container growth (`computeContainerDimensionForBoundText(dim, type)`, `dim = ceil(dim)`, `pad = 2P = 10`): ellipse `round((dim+pad)/√2*2)`; arrow `dim + 80`; diamond `2*(dim+pad)`; others `dim + pad`.

`redrawTextBoundingBox(text, container, scene)` (called after any text/font/container change): stickynote → `updateStickyNoteLayout`; if container or `!autoResize` → `text = wrapText(originalText, font, maxWidth)`; measure; `autoResize` → width = measured; height = measured; for container: if (non-arrow) measured height > maxH → container.height = grow(height) and store in original-container cache; if measured width > maxW → container.width = grow(width); then position via `computeBoundTextPosition`. Containers never auto-shrink here.

`handleBindTextResize` (container being resized, `resizeElements.ts` callers): unless the handle is pure `n`/`s` without aspect lock, re-wrap `originalText` to new maxW and remeasure; if new text height > maxH, grow container height (anchored at top/bottom/center depending on handle, flip); then reposition text.

WYSIWYG (`packages/excalidraw/wysiwyg/textWysiwyg.tsx` `updateWysiwygStyle`): original container height cached (`containerCache.ts` `originalContainerCache[id] = {height}`); while typing, container grows when text height > maxH and **shrinks back** (not below the cached original height) when text shrinks.

Alignment UI availability: vertical align offered only for text bound to non-arrow containers (`shouldAllowVerticalAlign`); horizontal align for free text and non-arrow bound text (`suppportsHorizontalAlign` — sic spelling).

Default alignment when text is created inside a container by double-click/Enter: center/middle — lives in App.tsx `startTextEditing`, UNVERIFIED (App.tsx too large to fetch).

### 2b.8 Arrow labels (`packages/element/src/linearElementEditor.ts`)

- `computeBoundTextElementPosition`: points < 2 → keep; if `labelPosition != null` → point at arc-length fraction `labelPosition` along the (curved/elbow) path via `getPointAtPathParameter` (clamp 0..1 × totalLength, find segment by prefix sums, interpolate within segment); else center = `getBoundTextElementCenter`. Text box is centered on that point (`x = p.x - w/2`, `y = p.y - h/2`). Result cached by (container.version, text.version).
- `getBoundTextElementCenter`: odd number of points → the middle point `points[floor(n/2)]`; even → midpoint of segment `n/2` (1-based index `n/2`, i.e. between points `n/2-1` and `n/2`): straight → segment center; curved → `curvePointAtLength(curve, 0.5)`; elbow → center of the two points.
- Dragging a label: `handleBoundTextDragging` sets `labelPosition = clamp((prefixSums[seg] + lengthWithinSeg) / totalLength, 0, 1)` (nearest point on path).
- Rendering: the arrow stroke is clipped with an even-odd clip that punches a rectangle `text bbox ± BOUND_TEXT_PADDING (5)` around the label (`renderElement.ts` `drawElementFromCanvas`), so the arrow line has a gap behind the label. Label max width = `max(0.7*arrow.width, fontSize*11)`.

### 2b.9 Drawing text (`packages/element/src/renderElement.ts` `drawElementOnCanvas` case "text"; SVG: `packages/excalidraw/renderer/staticSvgScene.ts`)

Canvas:
```ts
context.font = getFontString(element);
context.fillStyle = applyDarkModeFilter(element.strokeColor, theme === DARK);
context.textAlign = element.textAlign;   // left|center|right
context.textBaseline = "alphabetic";
const lines = element.text.replace(/\r\n?/g, "\n").split("\n");
const horizontalOffset = center ? width/2 : right ? width : 0;
const lineHeightPx = fontSize * lineHeight;
const verticalOffset = getVerticalOffset(fontFamily, fontSize, lineHeightPx);
for (i) context.fillText(lines[i], horizontalOffset, i * lineHeightPx + verticalOffset);
```
Text color is `strokeColor`; `backgroundColor` is not drawn for text. RTL (`isRTL(text)`) sets canvas `dir="rtl"`. Opacity/rotation applied by the generic element transform.
SVG: one `<text>` per line with `x=horizontalOffset`, `y=i*lineHeightPx+verticalOffset`, `font-family=getFontFamilyString(el)`, `font-size="<n>px"`, `fill`, `text-anchor = center→"middle"; right or rtl→"end"; else "start"`, `style="white-space: pre;"`, `direction`, `dominant-baseline="alphabetic"`.
Port (Pango): position each line so its baseline is at `y + i*lineHeightPx + verticalOffset` (use `pango_layout_get_baseline` of a single-line layout to convert), x anchored per alignment; do not use Pango's own line spacing.

### 2b.10 autoResize semantics

- `autoResize: true` (default): no wrapping, width = widest line, height = lines × lineHeightPx. Editing text keeps the anchor per alignment (`getAdjustedDimensions`: center+middle free text grows symmetrically; otherwise uses `adjustXYWithRotation` with sides chosen by textAlign/verticalAlign).
- `autoResize: false`: fixed `width`; text wrapped to width; height recomputed. Entered when: side (e/w) resize of a text element; text-tool drag wider than `TEXT_AUTOWRAP_THRESHOLD` (36px) (App.tsx, UNVERIFIED exact code); pasting/typing that exceeds a supplied maxWidth in `refreshTextDimensions` (free text only, `autoResize && width <= maxWidth && measured > maxWidth` → wrap, `autoResize:false`).
- Reset: when a selected single text has `autoResize:false`, a small vertical "reset auto-resize" handle is drawn (`textAutoResizeHandle.ts`: `TEXT_AUTO_RESIZE_HANDLE_LENGTH = 16`, `_GAP = 12`, `_HITBOX_WIDTH = 10`, `_HITBOX_HEIGHT = 18`, all divided by zoom; located `width/2 + (2*2)/zoom + 12/zoom` to the right of the text center, vertically centered, rotated with the element; hidden when `16 > (height + 2*padding)*zoom*0.8` or not desktop). Action `actionTextAutoResize` ("autoResize") sets `autoResize:true`, `text = originalText`, width/height = measureText, and keeps the anchor: `x += (oldW - newW)*anchor.x`, `y += (oldH - newH)*anchor.y`.

### 2b.11 Text resize (`packages/element/src/resizeElements.ts`)

`resizeSingleElement` routes text to `resizeSingleTextElement`:
- Handle containing `n` or `s` (corners `ne/nw/se/sw` and `n/s`): uniform scale. `metricsWidth = width * nextHeight/height`; `fontSize' = fontSize * metricsWidth / width` (`measureFontSizeFromWidth`; width = container maxW if bound; abort if `< MIN_FONT_SIZE` = 1); set `fontSize, width = metricsWidth, height = nextHeight`, origin via `getResizedOrigin`. fontSize is NOT rounded here.
- Handle `e` or `w`: `newWidth = max(getMinTextElementWidth, nextWidth)`; re-wrap `originalText` to newWidth; height = measured; `autoResize: false`.
Which side handles exist is decided by `getOmitSidesForEditorInterface` / size threshold `5 * handleSize / zoom` in `transformHandles.ts`; the exact text-specific omission (n/s hidden for text) is UNVERIFIED — in the UI text shows corners + e/w.

### 2b.12 Font size / family actions (`packages/excalidraw/actions/actionProperties.tsx`)

- `FONT_SIZE_RELATIVE_INCREASE_STEP = 0.1`. Increase: `Ctrl/Cmd+Shift+>` or `.` → `round(base * 1.1)`; Decrease: `Ctrl/Cmd+Shift+<` or `,` → `round(base / 1.1)` (`base = getBaseFontSize(el)`, respects sticky-note `baseFontSize`). After change: `redrawTextBoundingBox`, update bound arrows; free autoResize text repositioned by `offsetElementAfterFontResize` (left keeps x; center shifts by Δw/2; right by Δw; y shifts by Δh/2).
- Changing fontFamily sets `lineHeight = getLineHeight(nextFontFamily)` and redraws.

### 2b.13 WYSIWYG editor (`packages/excalidraw/wysiwyg/textWysiwyg.tsx`)

- `Escape` → submit (not cancel). `Ctrl/Cmd+Enter` → submit (ignored during IME composition, keyCode 229). Plain Enter inserts newline.
- `Tab` indents / `Shift+Tab` outdents the selected lines by `TAB_SIZE = 4` spaces (also bracket-key variants for indent/outdent — UNVERIFIED which keys).
- Font-size shortcuts above work while editing.
- Textarea styled with same `font`, `lineHeight`, width/height, rotation+zoom transform, textAlign.
- On submit: if `!value.trim()` the text element is deleted (and removed from the container's `boundElements`); otherwise `originalText` = typed text, `text` re-wrapped.

### 2b.14 Sticky notes (text-relevant constants, `packages/common/src/constants.ts`, `packages/element/src/stickyNote.ts`)

`STICKY_NOTE_MIN_FONT_SIZE = 16`, `STICKY_NOTE_MAX_FONT_SIZE = 512`, `STICKY_NOTE_FALLBACK_FONT_SIZE = 28`, `STICKY_NOTE_FONT_STEP = 2`, `STICKY_NOTE_PADDING = 16`, `STICKY_NOTE_FOOTER = {height:20, fontSize:12, fontFamily:"Helvetica, Arial, sans-serif", baselineFromBottom:14, opacity:1, minBodyWidthForYear:80}`, `STICKY_NOTE_BODY_INSET_Y = 52`, `DEFAULT_STICKY_NOTE_SIZE = 250`, `STICKY_NOTE_MIN_SIZE = 75`, `STICKY_NOTE_SHADOW_OFFSET = 3`, `STICKY_NOTE_SHADOW_OPACITY = 0.16`, `STICKY_NOTE_EDGE_SHADOW_WIDTH = 0.5`, `STICKY_NOTE_EDGE_SHADOW_OPACITY = 0.08`; stickyNote.ts: `STICKY_NOTE_RENDER_ROUGHNESS = [0, 1.5, 8]`, `STICKY_NOTE_CORNER_RADIUS_RATIO = 0.04`, `STICKY_NOTE_MAX_CORNER_RADIUS = 16`. Font fitting (`updateStickyNoteLayout` → `fitStickyNoteFont`): picks the largest size on the grid `ceiling − k·2` (down to 16) whose wrapped text fits the note body; if nothing fits, uses 16 and grows the note. `baseFontSize` stores the user-chosen ceiling.

## 2c. Interactive / selection UI rendering, canvas, grid, theme, colors, zoom

All values fetched from excalidraw `master` (2026-09-27). All interactive-layer sizes are in **screen px** and are divided by `zoom.value` before being drawn in scene coordinates, unless noted otherwise. Excalidraw uses two canvases: a **static** canvas (background, grid, elements, link icons; `packages/excalidraw/renderer/staticScene.ts`) and an **interactive** canvas on top of it (selection, handles, highlights, snap lines, scrollbars; `packages/excalidraw/renderer/interactiveScene.ts`). The interactive canvas is cleared to transparent every frame (`bootstrapCanvas` without `viewBackgroundColor`).

### 2c.1 Theme / dark mode (IMPORTANT: this changed from older versions)

| Item | Value | Source |
|---|---|---|
| `THEME` | `{ LIGHT: "light", DARK: "dark" }` | `packages/common/src/constants.ts` |
| `DARK_THEME_FILTER` | `"invert(93%) hue-rotate(180deg)"` | `common/src/constants.ts` |
| `DARK_MODE_FILTER_INVERT_PERCENT` | `93` | `common/src/colors.ts` |
| `DARK_MODE_FILTER_HUE_ROTATE_DEGREES` | `180` | `common/src/colors.ts` |
| CSS `--theme-filter` | light `none`; `.theme--dark`: `invert(93%) hue-rotate(180deg)` | `packages/excalidraw/css/theme.scss` (used for UI chrome, not the canvas) |

- **The canvas is NOT inverted with a CSS filter any more** (`css/styles.scss` has no `filter`/`--theme-filter` rule on canvas). Instead **every color is transformed individually** with `applyDarkModeFilter(color, enable)` (`common/src/colors.ts`) when `theme === "dark"`:
  1. parse with tinycolor, keep alpha;
  2. `cssInvert(r,g,b,93)`: per channel `round(clamp(c*(1-p) + (255-c)*p, 0, 255))`, `p = 0.93`;
  3. `cssHueRotate(r,g,b,180)`: CSS hue-rotate matrix on `[0..1]` channels, `a = deg→rad`, `c=cos a`, `s=sin a`:
     ```
     [0.213 + c*0.787 - s*0.213,  0.715 - c*0.715 - s*0.715,  0.072 - c*0.072 + s*0.928,
      0.213 - c*0.213 + s*0.143,  0.715 + c*0.285 + s*0.140,  0.072 - c*0.072 - s*0.283,
      0.213 - c*0.213 - s*0.787,  0.715 - c*0.715 + s*0.715,  0.072 + c*0.928 + s*0.072]
     ```
     each result clamped to [0,1], `*255`, rounded;
  4. `rgbToHex(r,g,b,alpha)` → `#RRGGBB` or `#RRGGBBAA`. Results cached in a `Map` (`DARK_MODE_COLORS_CACHE`).
  - Inverse exists: `removeDarkModeFilter` (hue-rotate 180 again, then `(c - 255p)/(1-2p)`).
- Where it is applied (verified):
  - `generateRoughOptions(element, continuousPath, isDarkMode)` in `packages/element/src/shape.ts`: `stroke: applyDarkModeFilter(element.strokeColor, isDarkMode)`, `fill: isTransparent(bg) ? undefined : applyDarkModeFilter(element.backgroundColor, isDarkMode)`.
  - Text fill in `drawElementOnCanvas` (`element/src/renderElement.ts`): `applyDarkModeFilter(element.strokeColor, theme===DARK)`.
  - Sticky-note background/footer, frame outline stroke, selection-rectangle fill (`renderElement.ts`), canvas background (`bootstrapCanvas`), grid colors, all interactive-scene colors via `getThemedColor(color, theme) = applyDarkModeFilter(color, theme === THEME.DARK)` (`interactiveScene.ts`).
  - SVG export: same per-color transform (`renderer/staticSvgScene.ts`).
- Images: only **SVG** images are inverted in dark mode: `shouldInvertImage = renderConfig.theme === THEME.DARK && cacheEntry?.mimeType === MIME_TYPES.svg`; non-Safari sets `context.filter = DARK_THEME_FILTER` before `drawImage`; Safari draws to a temp canvas and does `255 - c` per channel (pure invert, no hue rotate). Raster images (PNG/JPEG…) are drawn untouched. (`element/src/renderElement.ts`)
- Port note: in Emacs, implement `applyDarkModeFilter` as a pure color function (cache it) and call it at every color use site; don't post-process the framebuffer.
- `theme` is saved to browser storage only: `APP_STATE_STORAGE_CONF.theme = { browser: true, export: false, server: false }` (`packages/excalidraw/appState.ts`). Default `theme: THEME.LIGHT`.

### 2c.2 Canvas background

- Default `viewBackgroundColor: COLOR_PALETTE.white` = `"#ffffff"` (`appState.ts getDefaultAppState`). Saved in files (`{browser,export,server}: true`).
- `bootstrapCanvas` (`renderer/helpers.ts`): `setTransform(1,0,0,1,0,0)`, `scale(scale,scale)` (scale = devicePixelRatio / export scale). If `viewBackgroundColor` is a string: if it is not an opaque `#rgb`/`#rrggbb` (regex `/^#([0-9a-f]{3}|[0-9a-f]{6})$/i`) → `clearRect` first; if it is not `"transparent"` → `fillRect` whole canvas with `applyDarkModeFilter(viewBackgroundColor, theme===DARK)`. Non-string → `clearRect`.
- Canvas-background quick picks `DEFAULT_CANVAS_BACKGROUND_PICKS = ["#ffffff", "#f8f9fa", "#f5faff", "#fffce8", "#fdf8f6"]` (`colors.ts`).
- Scroll is snapped to whole device pixels before rendering (not when exporting): `snapScrollToDevicePixels`: `devicePixels = zoom*scale; scrollX = round(scrollX*devicePixels)/devicePixels` (same for Y) (`renderer/helpers.ts`).
- Static scene transform: after bootstrap, `context.scale(zoom, zoom)`; elements are drawn at `element.x + appState.scrollX` etc. (i.e. screen = (scene + scroll) * zoom).

### 2c.3 Grid (`renderer/staticScene.ts strokeGrid`)

| Item | Value |
|---|---|
| `DEFAULT_GRID_SIZE` | `20` (`common/constants.ts`) |
| `DEFAULT_GRID_STEP` | `5` (every 5th line is "bold") |
| `gridModeEnabled` default | `false` (saved in file) |
| normalization | `getNormalizedGridSize(v) = clamp(round(v), 1, 100)`, `getNormalizedGridStep` same (`scene/normalize.ts`) |
| Line colors (light) | bold `#dddddd`, regular `#e5e5e5` |
| Line colors (dark) | `applyDarkModeFilter("#dddddd")`, `applyDarkModeFilter("#e5e5e5")` |

Algorithm (verbatim logic, context already scaled by zoom; `width/height` passed in scene units = canvas CSS size / zoom):
```
offsetX = (scrollX % gridSize) - gridSize;  offsetY likewise
actualGridSize = gridSize * zoom;  spaceWidth = 1/zoom;  devicePixels = zoom*scale
snap(position, maxCssWidth): w = min(scale, maxCssWidth*devicePixels)
   if w < 1: {position, lineWidth: w/devicePixels}
   else W = round(w); center = W odd ? 0.5 : 0
        {position: (round(position*devicePixels - center) + center)/devicePixels, lineWidth: W/devicePixels}
for x = offsetX; x < offsetX + width + 2*gridSize; x += gridSize:
   isBold = gridStep > 1 && round(x - scrollX) % (gridStep*gridSize) == 0
   if !isBold && actualGridSize < 10: skip          # minor lines hidden when < 10 screen px apart
   {position, lineWidth} = snap(x, isBold ? 4 : 1)
   dash = isBold ? [] : [lineWidth*3, spaceWidth + (lineWidth + spaceWidth)]
   stroke from (position, offsetY - gridSize) to (position, ceil(offsetY + height + 2*gridSize))
(same for y)
```
Note: bold lines are solid, minor lines are dashed. Grid is drawn when `renderConfig.renderGrid` (defaults true in `_renderStaticScene`; App passes it based on grid mode — the exact App expression is UNVERIFIED, App.tsx too large to fetch). Grid is never rendered in exports.

### 2c.4 Static scene render order (`_renderStaticScene`)

1. `bootstrapCanvas` (background) → `scale(zoom)` → grid.
2. Pre-pass: computes groups that should be added to `frameToHighlight` (for clip preview while dragging into a frame).
3. For every visible element that is **not** iframe-like, in z-order (fractional-index order of `visibleElements`):
   - skip text elements whose `containerId` exists in the map (bound text is drawn right after its container);
   - `save()`; `clipElementToFrame` (see below); `renderElement(element)`; if it has bound text, `renderElement(boundText)` (same clip); `restore()`;
   - if not exporting and `renderLinks !== false`: `renderLinkIcon`.
4. Then all iframe-like elements (embeddable/iframe) in a second pass (always on top), with placeholder label when exporting or not validated.
5. `pendingFlowchartNodes` (flowchart preview).

Frame clipping (`frameClip`): translate to frame, `roundRect(0,0,w,h, FRAME_STYLE.radius / zoom)` (i.e. 8 screen px radius), `clip()`. Applied only if `appState.frameRendering.enabled && frameRendering.clip` and the element has a `frameId` (or a `frameToHighlight` exists) and `shouldApplyFrameClip(...)` or element/frame is being translated. Default `frameRendering: { enabled: true, clip: true, name: true, outline: true }` (not saved).

Frame outline (in `renderElement.ts`, "frame"/"magicframe" case, when `frameRendering.outline`): `lineWidth = FRAME_STYLE.strokeWidth / zoom` (2 screen px, constant thickness regardless of zoom), `strokeStyle = applyDarkModeFilter("#bbb")`, `roundRect(0,0,w,h, 8/zoom)`, stroke only. Magic frame: light `#7affd7`, dark `applyDarkModeFilter("#1d8264")`. In SVG export: `<rect rx=8 ry=8 fill="none" stroke=...>` (radius in scene units there) (`staticSvgScene.ts`).

`FRAME_STYLE` (`common/constants.ts`), verbatim: `strokeColor "#bbb"`, `strokeWidth 2`, `strokeStyle "solid"`, `fillStyle "solid"`, `roughness 0`, `roundness null`, `backgroundColor "transparent"`, `radius 8`, `nameOffsetY 3`, `nameColorLightTheme "#999999"`, `nameColorDarkTheme "#7a7a7a"`, `nameFontSize 14`, `nameLineHeight 1.25`.

Frame names: on the live canvas they are **DOM elements** (`App.tsx renderFrameNames`: fontSize `FRAME_STYLE.nameFontSize` (screen px, not scaled with zoom), color by theme, edit padding 6px, max width clipped to viewport; exact CSS UNVERIFIED — App.tsx truncated by fetcher). In export (`scene/export.ts addFrameLabelsAsTextElements`) a text element is synthesized per frame: `x = frame.x`, `y = frame.y - 3 - textHeight`, `fontFamily FONT_FAMILY.Helvetica`, `fontSize 14`, `lineHeight 1.25`, `strokeColor` `#7a7a7a` if `exportWithDarkMode` else `#999999`, `text = getFrameLikeTitle(frame)`; truncated with `"..."` suffix to `frame.width` (`truncateText`). When exporting a single frame: `getFrameRenderingConfig` → `{enabled:true, outline:false, name:false, clip:true}`.

Eraser preview: elements pending erase render at `ELEMENT_READY_TO_ERASE_OPACITY = 20` (%) (`common/constants.ts`; applied in element render-state, exact site UNVERIFIED).

### 2c.5 Element link icon (`staticScene.ts renderLinkIcon`, `components/hyperlink/helpers.ts`)

- Drawn only if `element.link` and the element is **not selected**.
- `DEFAULT_LINK_SIZE = 12`. `getLinkHandleFromCoords([x1,y1,x2,y2], angle, appState)`:
  ```
  size = 12; zoom = max(zoom.value, 1)          # icon shrinks in scene units only when zoomed in
  w = h = size/zoom; marginY = size/zoom
  centeringOffset = (size - 8) / (2*zoom); dashedLineMargin = 4/zoom
  x = x2 + dashedLineMargin - centeringOffset
  y = y1 - dashedLineMargin - marginY + centeringOffset
  rotate (x+w/2, y+h/2) around element center by angle; return [cx-w/2, cy-h/2, w, h]
  ```
  (top-right corner, just outside the element; when zoom < 1 the icon is 12 scene units, so it scales down on screen).
- Icon background: filled rect with `appState.viewBackgroundColor || "#ffffff"` then the SVG icon. External link: feather "external-link" icon, stroke `#1971c2`, stroke-width 1.75, 24×24 viewBox. Element link (`isElementLink(link)`): tabler "arrow-big-right-line", stroke `#1971c2`, width 2. Drawn rotated with the element, alpha = element opacity.
- Hit test (`isPointHittingLinkIcon`): icon rect expanded by `4/zoom`. In view mode on desktop, clicking anywhere on the bounding box follows the link (`isPointHittingLink`).

### 2c.6 Selection color & selection box (marquee)

| Item | Value | Source |
|---|---|---|
| selection color | CSS `--color-selection`: light `#6965db`, dark `#b4b0ff`; fallback `DEFAULT_SELECTION_COLOR` `#6965db` | `renderer/helpers.ts getSelectionColor`, `css/theme.scss` |
| fallback when renderConfig has none | `getThemedColor("#000")` | `interactiveScene.ts` |
| locked element selection | `getThemedColor("#ced4da")`, dashed | `interactiveScene.ts` |
| group selection box | `getThemedColor("#000")` (black), dashed | `interactiveScene.ts` |
| marquee fill | `applyDarkModeFilter("rgba(0, 0, 200, 0.04)")` | `element/src/renderElement.ts renderSelectionElement` |
| marquee stroke | selection color, `lineWidth 1/zoom`, offset `0.5/zoom` | same |

Note: the dark selection color `#b4b0ff` comes from CSS and is used as-is (not passed through `applyDarkModeFilter`).

### 2c.7 Selection borders (`renderSelectionBorder`)

```
padding    = elementProperties.padding ?? DEFAULT_TRANSFORM_HANDLE_SPACING*2   # = 4 screen px; 0 for images and the element being cropped
linePadding = padding/zoom
dash: lineWidth = 8/zoom, spaceWidth = 4/zoom
context.lineWidth = (activeEmbeddable ? 4 : 1)/zoom
for each color i of selectionColors (count n):
   if dashed: setLineDash([8/zoom, 4/zoom + (8/zoom + 4/zoom)*(n-1)])
   lineDashOffset = (8/zoom + 4/zoom)*i
   strokeRect (rotated by element angle around (cx,cy)) of
     (x1 - linePadding, y1 - linePadding, w + 2*linePadding, h + 2*linePadding)
```
- `x1,y1,x2,y2,cx,cy` from `getElementAbsoluteCoords(element, elementsMap, true)` (unrotated box; the rect is rotated with the element).
- Single element selected (not via group): solid 1px border in selection color.
- Elements selected via a group (`isSelectedViaGroup`) get **no** individual border; instead each selected group gets one **dashed black** (`#000` themed) unrotated box around `getCommonBounds(groupElements)` (angle 0). If any group member is locked → `#ced4da`. `editingGroupId` (entered group) also gets a dashed box.
- Locked selected element: dashed `#ced4da`.
- A single selected elbow arrow that is bound at either end gets no border.
- Border boxes are only drawn if `hasBoundingBox(selectedElements)` (`element/src/transformHandles.ts`): false while a linear element is being edited/dragged; true for >1 elements; false for an elbow arrow; true for non-linear; for a single line/arrow only if `points.length > 2` and not a mobile device (2-point lines/arrows show only their endpoint handles, no box).
- Nothing selection-related is drawn while `appState.multiElement`, `appState.newElement`, or linear editing is active.

**Multi-selection box** (selectedElements.length > 1, not rotating, none locked): unrotated rect around `getCommonBounds(selected)` padded by `(DEFAULT_TRANSFORM_HANDLE_SPACING*2)/zoom` = 4 screen px, `setLineDash([2/zoom])`, `lineWidth 1/zoom`, selection color; this is drawn **in addition to** each element's own solid border. Then transform handles from `getTransformHandlesFromCoords(commonBounds, angle 0, ...)` with default margin 4 (rotation handle omitted if any frame is selected).

Other boxes:
- `renderElementsBoxHighlight` (e.g. `elementsToHighlight`, elements about to be framed): solid `rgb(0,118,255)` (themed) per ungrouped element and per fully-selected group; used also for `activeLockedId` (locked element under pointer) with dashed `#ced4da`.
- `renderFrameHighlight` (`frameToHighlight`, frame an element is being dragged into): stroke `rgb(0,118,255)` themed, `lineWidth 2/zoom`, `roundRect radius 8/zoom`, rotated by frame angle.
- `renderTextBox` (wrapped text, `autoResize === false`, being edited or hovered with text tool): rect padded by `getTextBoxPadding(zoom) = (2*2)/zoom` = 4 screen px, `lineWidth 1/zoom`, selection color, `globalAlpha 0.5`, `setLineDash([6/zoom, 4/zoom])`, rotated with the text.
- Text auto-resize reset handle (`textAutoResizeHandle.ts`, only for a selected text with `autoResize=false`, desktop only): a vertical bar right of the text box: `TEXT_AUTO_RESIZE_HANDLE_GAP = 12`, `LENGTH = 16`, hitbox `10 × 18`, hidden if `16 > (textHeight + 2*padding)*zoom*0.8` (`MAX_HANDLE_HEIGHT_RATIO`). Center = `(textCenterX + width/2 + padding + 12/zoom, textCenterY)` rotated by text angle. Drawn with `globalAlpha 0.5`, `lineWidth 1.5/zoom`, `lineCap round`, selection color.
- Search matches: fill `rgba(255, 226, 0, 0.4)` (match) / `rgba(255, 124, 0, 0.4)` (focused); dark `rgba(221, 181, 136, 0.4)` / `rgba(250, 123, 53, 0.4)` (`SEARCH_MATCH_COLOR`).

### 2c.8 Transform handles (`element/src/transformHandles.ts`)

| Constant | Value |
|---|---|
| `transformHandleSizes` | `{ mouse: 8, pen: 16, touch: 28 }` (screen px) |
| `ROTATION_RESIZE_HANDLE_GAP` | `16` |
| `DEFAULT_TRANSFORM_HANDLE_SPACING` | `2` (`common/constants.ts`) |
| `SIDE_RESIZING_THRESHOLD` | `2 * 2 = 4` |
| `DEFAULT_COLLISION_THRESHOLD` | `2*SIDE_RESIZING_THRESHOLD - EPSILON` = `8 - 0.00001` |
| `EPSILON` | `0.00001` |

`getTransformHandlesFromCoords([x1,y1,x2,y2,cx,cy], angle, zoom, pointerType, omitSides = {}, margin = 4, spacing = 2)`:
```
size = transformHandleSizes[pointerType]
handleW = handleH = size/zoom ; handleMarginX = handleMarginY = size/zoom
dashedLineMargin = margin/zoom
centeringOffset  = (size - 2*spacing) / (2*zoom)
nw: (x1 - dLM - hMX + cO,  y1 - dLM - hMY + cO)
ne: (x2 + dLM - cO,        y1 - dLM - hMY + cO)
sw: (x1 - dLM - hMX + cO,  y2 + dLM - cO)
se: (x2 + dLM - cO,        y2 + dLM - cO)
rotation: (x1 + w/2 - handleW/2,  y1 - dLM - hMY + cO - 16/zoom)
minimumSizeForEightHandles = 5*8/zoom   (= 40 screen px)
if |w| > min: n = (x1 + w/2 - hW/2, y1 - dLM - hMY + cO), s = (x1 + w/2 - hW/2, y2 + dLM - cO)
if |h| > min: w = (x1 - dLM - hMX + cO, y1 + h/2 - hH/2), e = (x2 + dLM - cO, y1 + h/2 - hH/2)
each handle = [x, y, handleW, handleH], where its center is rotated around (cx,cy) by angle (generateTransformHandle)
```
Worked example (mouse, zoom 1, spacing 2 → centeringOffset 2): with the default margin 4 (multi-selection) the nw handle spans x ∈ `[x1-10, x1-2]`; with margin 2 (single non-linear, non-image element, see below) it spans `[x1-8, x1]`; the rotation handle is 16 px higher than the n handle row.

`getTransformHandles(element, zoom, elementsMap, pointerType="mouse", omitSides=DEFAULT_OMIT_SIDES)`:
- returns `{}` if `element.locked` or elbow arrow (elbow arrows: no handles, no rotation);
- freedraw / linear with exactly 2 points: omit sides based on direction of `points[1]`: `dx==0||dy==0` → BACKSLASH set; `dx>0,dy<0` → SLASH; `dx>0,dy>0` → BACKSLASH; `dx<0,dy>0` → SLASH; `dx<0,dy<0` → BACKSLASH. `OMIT_SIDES_FOR_LINE_SLASH = {e,s,n,w,nw,se}`, `OMIT_SIDES_FOR_LINE_BACKSLASH = {e,s,n,w}`. (Note: for linear elements with 2 points `hasBoundingBox` is false so these handles are computed but not drawn; they are still used for hit-testing — UNVERIFIED whether App uses them.)
- frame-like: add `rotation: true` (frames cannot rotate).
- margin: linear → `2 + 8 = 10`; image → `0`; else `2`. spacing: image → `0`, else default 2.
- coords from `getElementAbsoluteCoords(element, elementsMap, true)`.
- Omit-sides constants: `DEFAULT_OMIT_SIDES = OMIT_SIDES_FOR_MULTIPLE_ELEMENTS = {e,s,n,w: true}`; `OMIT_SIDES_FOR_FRAME = {e,s,n,w,rotation: true}`.

**SURPRISE — side handles are not drawn on desktop.** Both rendering and hit-testing pass `getOmitSidesForEditorInterface(editorInterface)`, which returns `DEFAULT_OMIT_SIDES` (omit n/s/e/w) whenever `canResizeFromSides(editorInterface)` is true, i.e. everywhere except a phone-form-factor mobile device (where it returns `{}` and n/s/e/w squares are drawn). On desktop, side resizing is done by grabbing the **selection border edge** (`resizeTest` in `element/src/resizeTest.ts`):
```
if not (linear && points.length <= 2):
  SPACING = image ? 0 : 4/zoom ; threshold = 4/zoom
  sides = rotated edges of rect (x1-SPACING, y1-SPACING)-(x2+SPACING, y2+SPACING)
  n: TL→TR, e: TR→BR, s: BR→BL, w: BL→TL
  if pointOnLineSegment(pointer, side, threshold): return that side
```
Test order in `resizeTest`: rotation handle first, then corner handles (point inside rect, inclusive), then sides. Only for selected elements. Multi-selection: `getTransformHandleTypeFromCoords` does the same on the common bounds (angle 0).

Handle drawing (`renderTransformHandles`): `lineWidth 1/zoom`, `strokeStyle = selection color`, `fillStyle = getThemedColor("#fff")`; rotation handle = filled+stroked **circle** of radius `width/2` (4 screen px) at the handle center; other handles = `roundRect(x,y,w,h, 2/zoom)` fill then stroke (rect axis-aligned in the handle's own box — handle position is rotated, handle square itself is not rotated when `roundRect` is available). Not drawn in view mode, while editing text, or when cropping. Pointer type used for drawing is always `"mouse"` (8px).

Cursors (`getCursorForResizingElement`): n/s → `ns-resize`, e/w → `ew-resize`, nw/se → `nwse` (swapped to `nesw` when `sign(w)*sign(h) === -1`), ne/sw → `nesw` (swap to `nwse`), rotation → `grab`; cursor index rotated by `round(angle/(π/4))` through `["ns","nesw","ew","nwse"]`.

Crop handles (`renderCropHandles`, image in crop mode): four L-shaped corner marks, `LINE_WIDTH 3`, `LINE_LENGTH 20` (both /zoom; length capped at half width/height + line width), color = selection color, rotated with the image.

### 2c.9 Linear element point handles (`renderLinearPointHandles`, `renderSingleLinearPoint`)

| Item | Value |
|---|---|
| `LinearElementEditor.POINT_HANDLE_SIZE` | `10` (`element/src/linearElementEditor.ts`) |
| radius when editing (`isEditing`) | `10` (screen px) |
| radius when just selected | `10/2 = 5` |
| midpoint ("phantom") radius | `5` |
| stroke | `getThemedColor("#5e5ad8")`, `lineWidth 1/zoom`, no dash |
| fill normal | `rgba(255, 255, 255, 0.9)` |
| fill selected point | `rgba(134, 131, 226, 0.9)` |
| fill phantom/midpoint | `rgba(177, 151, 252, 0.7)`, **no stroke** |
| hover highlight (`highlightPoint`) | filled circle `rgba(105, 101, 219, 0.4)`, radius `10/zoom`, no stroke |
| overlapping point (equal to previous point within `2/zoom`, or last==first) | radius ×1.5 (editing) or ×2 (not editing), drawn **unfilled** unless selected |

- Points are shown for a selected (non-locked) single linear element even without editing (radius 5), and for the element under edit (radius 10).
- Elbow arrows: only first and last points get handles; segment midpoints (radius 5) drawn for each segment unless too short (`dist*zoom < 5`); midpoint is "phantom"-styled (no stroke) unless the segment is in `fixedSegments` (then it is drawn as a normal white stroked point).
- Other lines: segment midpoints from `getEditorMidPoints` (skips segments with length*zoom < `10*4 = 40`; for curved lines with >2 points uses the curve length), drawn only while editing or when the line has exactly 2 points.
- Polygon lines: when point 0 is selected, the last point (which overlaps it) is highlighted too.
- Hover: when not dragging, hovering a segment midpoint (`segmentMidPointHoveredCoords`) or a point (elbow: only endpoints) draws `highlightPoint`.

### 2c.10 Binding highlight & focus point (`interactiveScene.ts`, `element/src/binding.ts`, `element/src/arrows/focus.ts`)

| Constant | Value |
|---|---|
| `BINDING_HIGHLIGHT_RGB` | light `"106, 189, 252"`, dark `"104, 182, 240"` (used as `rgba(<rgb>, 1)`) |
| `BINDING_MIDPOINT_COLOR` | light `rgba(65, 65, 65, 0.5)`, dark `rgba(237, 237, 237, 0.8)` |
| `BASE_BINDING_GAP` | `5`; `getBindingGap(el) = 5 + el.strokeWidth/2` |
| `maxBindingDistance_simple(zoom)` | `B = max(5,15) = 15; z = zoom<1 ? zoom : 1; clamp(B/(z*1.5), 15, 30)` |
| `FOCUS_POINT_SIZE` | `10/1.5` |
| `BASE_ARROW_MIN_LENGTH` | `10` |
| `BIND_MODE_TIMEOUT` | `700` ms |
| feature flag `COMPLEX_BINDINGS` | default `false` (localStorage `excalidraw-feature-flags`; `common/src/utils.ts`) — so the **simple** renderer is the default |

Simple highlight (`renderBindingHighlightForBindableElement_simple`), drawn when `appState.isBindingEnabled && appState.suggestedBinding` (default `isBindingEnabled: true`):
- If the target is inside a frame, clip to the frame's rounded rect first.
- Frame target: stroke frame outline, `lineWidth 2/zoom`, `roundRect radius 8/zoom`, color `rgba(BINDING_HIGHLIGHT_RGB,1)`.
- Other shapes: rotate around element center by its angle; `lineWidth = clamp(1.75, strokeWidth, 4) / max(0.25, zoom)` — NOTE the argument order `clamp(1.75, strokeWidth, 4)`; with excalidraw's `clamp(value, min, max)` this is `clamp(value=1.75, min=strokeWidth, max=4)` = `max(strokeWidth, 1.75)` capped at 4 (UNVERIFIED which signature `clamp` has in `@excalidraw/math`; it is `clamp(value, min, max)` in normalize.ts usage). The outline traced is the element's own geometry: ellipse via `ctx.ellipse(w/2,h/2,w/2,h/2)`; diamond via `deconstructDiamondElement` segments + bezier corner curves; everything else via `deconstructRectanguloidElement` (respects roundness). No offset/gap: the highlight traces the element edge itself.
- Midpoint indicators (only if `isMidpointSnappingEnabled` (default true) && !gridModeEnabled && !shift-angle-lock): radius `4/zoom`; elbow arrows show all 4 edge midpoints, simple arrows only the one nearest the pointer if within `2*(maxBindingDistance_simple + strokeWidth/2)`, and only when the pointer is outside the element. Fill `BINDING_MIDPOINT_COLOR`, the snapped midpoint (`suggestedBinding.midPoint`) filled with highlight color.
- Complex mode (flag on): animated; opacity follows `BIND_MODE_TIMEOUT` countdown, `lineWidth` clamped 2.5–4, dashed circle for center snap area, clearRect cutouts around midpoints (radius + 5). Exact code not captured (UNVERIFIED details; not default).

Focus point indicator (selected arrow with a binding, only for non-elbow 2-point arrows and when the focus point is ≥ `1.5*FOCUS_POINT_SIZE/zoom` = 10 screen px from the arrow endpoint and lies within the bindable element (+binding gap)): dashed line `[4/zoom, 4/zoom]`, `lineWidth 1/zoom`, `rgba(134, 131, 226, 0.6)` from arrow endpoint to the focus point (`getGlobalFixedPointForBindableElement(binding.fixedPoint, ...)`), plus a circle radius `FOCUS_POINT_SIZE/1.5 / zoom` (= 10/2.25 ≈ 4.44 px), stroke `rgba(134,131,226,0.6)`, fill `rgba(255,255,255,0.9)` (hovered: `rgba(134,131,226,0.9)`).

Text tool hover (`renderTextToolHover`): hovering a text → text box; a bindable container → the simple binding highlight; an arrow → `highlightPoint` at label center / start / end.

### 2c.11 Snap lines (`renderer/renderSnaps.ts`)

| Constant | Value |
|---|---|
| `SNAP_COLOR_LIGHT` | `#ff6b6b` |
| `SNAP_COLOR_DARK` | `#ff9090` |
| `SNAP_COLOR_DARK_ZEN` | `#da5b5b` |
| `SNAP_WIDTH` | `1` (×1.5 in zen mode), /zoom |
| `SNAP_CROSS_SIZE` | `2` (×1.5 in zen), /zoom |

- `points` snap line: line from first to last point (omitted in zen mode) + an `×` cross (half-size 2/zoom) at every point.
- `pointer` snap line: cross at point 0 + line to point 1 (no line in zen).
- `gap` snap line (equal spacing): `FULL = 8/zoom, HALF = 4/zoom, QUARTER = 2/zoom`; horizontal: perpendicular end ticks of length 2·FULL at both ends (non-zen), two short parallel ticks (length 2·HALF) at `mid ± QUARTER`, and the main line (non-zen). Vertical analogous.
- Colors are hard-coded per theme (not via applyDarkModeFilter).

### 2c.12 Scrollbars (`scene/scrollbars.ts`, interactiveScene)

`SCROLLBAR_MARGIN = 4`, `SCROLLBAR_WIDTH = 6`, `SCROLLBAR_COLOR = "rgba(0,0,0,0.3)"` (themed via applyDarkModeFilter), stroke `rgba(255,255,255,0.8)` themed, rounded rect radius `3`. Rendered only when `renderConfig.renderScrollbars`. Geometry: scene extent = union(elements bounds, viewport); bar length = `viewportPx * (viewportScene/extendedScene) - (2*max(margin, safeArea) + 2*width)`; horizontal bar at `y = viewportH - 6 - 4`, vertical at `x = viewportW - 6 - 4` (left side in RTL). Hidden if viewport covers the whole extent on that axis. Brief; low priority for the port.

### 2c.13 Colors / palette (`packages/common/src/colors.ts`)

`COLOR_PALETTE` (shade index 0..4):

| name | 0 | 1 | 2 | 3 | 4 |
|---|---|---|---|---|---|
| transparent | `transparent` | | | | |
| black | `#1e1e1e` | | | | |
| white | `#ffffff` | | | | |
| gray | `#f8f9fa` | `#e9ecef` | `#ced4da` | `#868e96` | `#343a40` |
| red | `#fff5f5` | `#ffc9c9` | `#ff8787` | `#fa5252` | `#e03131` |
| pink | `#fff0f6` | `#fcc2d7` | `#f783ac` | `#e64980` | `#c2255c` |
| grape | `#f8f0fc` | `#eebefa` | `#da77f2` | `#be4bdb` | `#9c36b5` |
| violet | `#f3f0ff` | `#d0bfff` | `#9775fa` | `#7950f2` | `#6741d9` |
| blue | `#e7f5ff` | `#a5d8ff` | `#4dabf7` | `#228be6` | `#1971c2` |
| cyan | `#e3fafc` | `#99e9f2` | `#3bc9db` | `#15aabf` | `#0c8599` |
| teal | `#e6fcf5` | `#96f2d7` | `#38d9a9` | `#12b886` | `#099268` |
| green | `#ebfbee` | `#b2f2bb` | `#69db7c` | `#40c057` | `#2f9e44` |
| yellow | `#fff9db` | `#ffec99` | `#ffd43b` | `#fab005` | `#f08c00` |
| orange | `#fff4e6` | `#ffd8a8` | `#ffa94d` | `#fd7e14` | `#e8590c` |
| bronze | `#f8f1ee` | `#eaddd7` | `#d2bab0` | `#a18072` | `#846358` |

- `COLOR_WHITE = "#ffffff"`, `COLOR_CHARCOAL_BLACK = "#1e1e1e"` (`constants.ts`).
- `DEFAULT_ELEMENT_STROKE_COLOR_INDEX = 4`, `DEFAULT_ELEMENT_BACKGROUND_COLOR_INDEX = 1`, `DEFAULT_CHART_COLOR_INDEX = 4`, `MAX_CUSTOM_COLORS_USED_IN_CANVAS = 5`, `COLOR_TOP_PICKS_SLOTS = 5`, `COLORS_PER_ROW = 5`.
- Stroke quick picks `DEFAULT_ELEMENT_STROKE_PICKS` = `["#1e1e1e", "#e03131", "#2f9e44", "#1971c2", "#f08c00"]`.
- Background quick picks `DEFAULT_ELEMENT_BACKGROUND_PICKS` = `["transparent", "#ffc9c9", "#b2f2bb", "#a5d8ff", "#ffec99"]`.
- Full pickers `DEFAULT_ELEMENT_STROKE_COLOR_PALETTE` and `DEFAULT_ELEMENT_BACKGROUND_COLOR_PALETTE` are identical: `transparent, white, gray[5], black, bronze[5]` + `COMMON_ELEMENT_SHADES = pick(COLOR_PALETTE, ["cyan","blue","violet","grape","pink","green","teal","yellow","orange","red"])`.
- `getAllColorsSpecificShade(i)` row order: cyan, blue, violet, grape, pink / green, teal, yellow, orange, red.
- `isColorDark(color, threshold = 160)`: `""` or invalid → true, transparent → false, else YIQ contrast `< 160`.
- `DEFAULT_ELEMENT_PROPS` (`constants.ts`): `strokeColor #1e1e1e`, `backgroundColor transparent`, `fillStyle "solid"`, `strokeWidth STROKE_WIDTH[DEFAULT_ELEMENT_STROKE_WIDTH_KEY]`, `strokeStyle "solid"`, `roughness ROUGHNESS.artist`, `opacity 100`, `locked false` (see style-properties section for the currentItem* defaults used by the UI).

### 2c.14 Zoom

| Item | Value | Source |
|---|---|---|
| `MIN_ZOOM` | `0.1` | `common/constants.ts` |
| `MAX_ZOOM` | `30` | same |
| `ZOOM_STEP` | `0.1` (additive) | same |
| normalize | `getNormalizedZoom(z) = clamp(round(z, 6), 0.1, 30)` | `scene/normalize.ts` |
| zoom in | `zoom + 0.1`, anchored at viewport center; keys `Ctrl/Cmd` or `Shift` + `=` / numpad `+` | `actions/actionCanvas.tsx actionZoomIn` |
| zoom out | `zoom - 0.1`; `Ctrl/Cmd|Shift` + `-` / numpad `-` | `actionZoomOut` |
| reset | `1`; `Ctrl/Cmd|Shift` + `0` | `actionResetZoom` |
| zoom to fit all | `Shift+1`, fit `"scale-down"` (never zooms above 1) | `actionZoomToFit` |
| fit selection in viewport | `Shift+2`, `"scale-down"` | `actionZoomToFitSelectionInViewport` |
| fit selection | `Shift+3`, `"contain"` (may zoom in) | `actionZoomToFitSelection` |

Anchored zoom (`viewport.ts getViewportForZoom`), for a viewport point `(vx, vy)` (relative to the canvas: `appLayerX = vx - offsetLeft`):
```
baseScrollX = scrollX + (appLayerX - appLayerX/currentZoom)
scrollX'    = baseScrollX - (appLayerX - appLayerX/nextZoom)
```
(same for Y). `zoomValueToFitBoundsOnViewport = min(vw/bw, vh/bh, 1)` for scale-down. `DEFAULT_OVERSCROLL = 150` (scroll constraints, not needed). Zoom actions record history as `CaptureUpdateAction.EVENTUALLY` (not their own undo step).

Wheel zoom (Ctrl/Cmd + wheel / pinch) formula lives in `App.tsx handleWheel` — could not fetch (file truncated by fetcher): UNVERIFIED. Historical implementation (from memory of older versions, verify before relying): `sign = sign(deltaY); MAX_STEP = ZOOM_STEP*100; delta = |deltaY| > MAX_STEP ? MAX_STEP*sign : deltaY; newZoom = zoom - delta/100; newZoom += log10(max(1, zoom)) * -sign * min(1, |deltaY|/20)`, anchored at the cursor; plain wheel pans `scroll -= delta/zoom`, Shift+wheel pans horizontally.

### 2c.15 Laser pointer (brief; `packages/excalidraw/laserTrails.ts`)

`DEFAULT_LASER_COLOR = "red"`. Local trail = `AnimatedTrail` with `{ simplify: 0, streamline: 0.4, sizeMapping: c => min(easeOut(l), easeOut(t)), fill: () => "red" }`, where `DECAY_TIME = 1000` ms and `DECAY_LENGTH = 50` points: `t = max(0, 1 - (now - c.pressure)/1000)` (pressure slot stores timestamp), `l = (50 - min(50, totalLength - currentIndex))/50`. Trail is ephemeral (never an element, never saved). Uses `packages/laser-pointer`. Low priority.

---

# 3. Interaction behavior

## 3.A Tools, keyboard shortcuts, style panel, history

Source: excalidraw `master` as fetched 2026-09-27 (WebFetch of raw.githubusercontent.com + Sourcegraph code search for line-level context in `App.tsx`). Line numbers for `App.tsx` are from Sourcegraph's index at fetch time and will drift.

### 3.A.1 Tool table (`packages/excalidraw/components/Tools.tsx`, `TOOLS` via `defineTools`)

NOTE: the old `components/shapes.tsx` / `SHAPES` array no longer exists (404). Tools now live in `Tools.tsx` as an object `TOOLS`; `App.tsx` imports `{ findShapeByKey, TOGGLE_TOOLS } from "./Tools"`.

| tool (`activeTool.type`) | letterKey | numericKey | other flags |
|---|---|---|---|
| `hand` | `h` | – | `toggle: true` |
| `selection` | `v` | `1` | `fillable: true` |
| `rectangle` | `r` | `2` | `fillable: true` |
| `diamond` | `d` | `3` | `fillable: true` |
| `ellipse` | `o` | `4` | `fillable: true` |
| `arrow` | `a` | `5` | `fillable: true` |
| `line` | `l` | `6` | `fillable: true` |
| `freedraw` | `[p, x]` | `7` | |
| `text` | `t` | `8` | |
| `stickynote` | `n` | – | NEW tool (sticky note) |
| `image` | – (no letter!) | `9` | help dialog lists only `9` |
| `eraser` | `e` | `0` | `toggle: true` |
| `frame` | `f` | – | |
| `autoshape` | `x` **with `shiftKey: true`** (Shift+X) | – | `fillable: false`; NEW (draw-a-shape recognition, see `element/src/convertToShape.ts`) |
| `embeddable` | – | – | out of scope |
| `laser` | `k` | – | |
| `bucketfill` | `b` | – | NEW; pressing `b` again while active cycles background color (`this.bucketFill.cycleBackgroundColor()`) |
| `lasso` | – | – | `fillable: false`; reached via `preferredSelectionTool` |

`TOOL_TYPE` (common/src/constants.ts): `selection, lasso, rectangle, diamond, ellipse, arrow, line, freedraw, text, image, eraser, hand, frame, magicframe, stickynote, embeddable, laser, autoshape, bucketfill`.

`TOGGLE_TOOLS` = tools with `toggle: true` (`hand`, `eraser`): "tools that, when activated while already active, switch back to the previously active tool".

`findShapeByKey(key, app, shiftKey=false)` (Tools.tsx), verbatim logic:
```ts
const lowerKey = key.toLowerCase();
for (const type of Object.keys(TOOLS)) {
  const { letterKey, numericKey, shiftKey: requiresShift } = TOOLS[type];
  if (shiftKey !== Boolean(requiresShift)) continue;
  if ((numericKey != null && key === numericKey) ||
      (letterKey && (typeof letterKey === "string"
                      ? letterKey === lowerKey
                      : letterKey.includes(lowerKey)))) {
    return type === "selection" ? app.state.preferredSelectionTool.type : type;
  }
}
return null;
```
So: tool keys match only when Shift state equals the tool's `shiftKey` flag (plain `x` = freedraw, `Shift+X` = autoshape). `v`/`1` returns the *preferred* selection tool (`selection` or `lasso`).

Tool-key dispatch in `App.tsx` `onKeyDown` (~line 5766–5845), verbatim essentials:
```ts
if (!shouldPreventToolSwitching && !event.ctrlKey && !event.altKey && !event.metaKey &&
    !this.state.newElement && !this.state.selectionElement &&
    !this.state.selectedElementsAreBeingDragged) {
  const shape = findShapeByKey(event.key, this, event.shiftKey);
  if (this.state.viewModeEnabled && !oneOf(shape, ["laser", "hand"])) return;
  if (shape) {
    if (shape === "arrow" && this.state.activeTool.type === "arrow") {
      const nextArrowType =
        currentItemArrowType === sharp ? round : currentItemArrowType === round ? elbow : sharp;
      this.setState({ currentItemArrowType: nextArrowType });
    }
    if (shape === "bucketfill" && activeTool.type === "bucketfill") bucketFill.cycleBackgroundColor();
    else if (shape === "lasso" && activeTool.type === "laser") setActiveTool({type: preferredSelectionTool.type});
    else this.setActiveTool({ type: shape }, { toggle: true });
    return;
  } else if (event.key === KEYS.Q) {
    this.toggleLock("keyboard");   // toggles appState.activeTool.locked
    return;
  }
}
```
- Pressing `A` while the arrow tool is active **cycles `currentItemArrowType` sharp → round → elbow → sharp**.
- Escape in view mode sets tool to `selection`.
- Tool keys are ignored while any of Ctrl/Alt/Meta is held, or while creating/box-selecting/dragging.
- `Q` = tool lock (`activeTool.locked`; default `DEFAULT_ELEMENT_PROPS.locked` = false). When not locked, the tool returns to selection after creating an element (actionFinalize / pointer-up paths).

### 3.A.2 Key constants (`packages/common/src/keys.ts`)

- `KEYS.CTRL_OR_CMD = isDarwin ? "metaKey" : "ctrlKey"` — every "CtrlOrCmd" means Cmd on macOS, Ctrl elsewhere.
- `CODES` (physical `event.code`) used for layout-independent shortcuts: `EQUAL "Equal"`, `MINUS "Minus"`, `NUM_ADD "NumpadAdd"`, `NUM_SUBTRACT "NumpadSubtract"`, `NUM_ZERO "Numpad0"`, `BRACKET_RIGHT/LEFT`, `ONE "Digit1"`, `TWO`, `THREE`, `NINE`, `QUOTE "Quote"`, `ZERO "Digit0"`, `SLASH "Slash"`, `C D H V Z Y R S` = `"KeyC"` etc.
- Modifier helpers (verbatim semantics):
  - `shouldResizeFromCenter(e) = e.altKey`
  - `shouldMaintainAspectRatio(e) = e.shiftKey`
  - `shouldRotateWithDiscreteAngle(e) = e.shiftKey`
  - `isArrowKey(k)` = ArrowLeft/Right/Up/Down.

### 3.A.3 Complete shortcut list

Authoritative user-facing list = `packages/excalidraw/components/HelpDialog.tsx`; exact predicates = each action's `keyTest` (`packages/excalidraw/actions/*.ts(x)`); labels = `actions/shortcuts.ts` `shortcutMap`. "Mod" = Cmd (mac) / Ctrl (others).

#### Tools section (HelpDialog)
| Label | Keys |
|---|---|
| hand | `H` |
| selection | `V`, `1` |
| rectangle | `R`, `2` |
| diamond | `D`, `3` |
| ellipse | `O`, `4` |
| arrow | `A`, `5` |
| line | `L`, `6` |
| freedraw | `P`, `7` (also `X`, not listed in help) |
| text | `T`, `8` |
| stickynote | `N` |
| image | `9` |
| eraser | `E`, `0` |
| frame | `F` |
| laser | `K` |
| bucketfill | `B` |
| eyeDropper | `I`, `Shift+S` (pick stroke), `Shift+G` (pick background) |
| editLineArrowPoints | `Mod+Enter` |
| editText | `Enter` |
| textNewLine | `Enter`, `Shift+Enter` (while editing) |
| textFinish | `Esc`, `Mod+Enter` |
| curvedArrow | `A`, click, click, click (not "or") |
| curvedLine | `L`, click, click, click |
| cropStart | double-click or `Enter` (image selected) |
| cropFinish | `Enter` or `Escape` |
| tool lock | `Q` |
| preventBinding | hold `Mod` |
| link | `Mod+K` |
| convertElementType | `Tab` / `Shift+Tab` |

#### View section
| Label | Keys | keyTest (verbatim, file) |
|---|---|---|
| zoomIn | `Mod++` | `(event.code === CODES.EQUAL \|\| event.code === CODES.NUM_ADD) && (event[KEYS.CTRL_OR_CMD] \|\| event.shiftKey)` (actionCanvas.tsx `actionZoomIn`) — note Shift+= also works |
| zoomOut | `Mod+-` | `(CODES.MINUS \|\| NUM_SUBTRACT) && (Mod \|\| shift)` |
| resetZoom | `Mod+0` | `(CODES.ZERO \|\| NUM_ZERO) && (Mod \|\| shift)` |
| zoomToFit (all) | `Shift+1` | `event.code === CODES.ONE && event.shiftKey && !event.altKey && !event[KEYS.CTRL_OR_CMD]`, fit mode `"scale-down"` |
| zoomToFitSelectionInViewport | `Shift+2` | `CODES.TWO` + shift, `"scale-down"` |
| zoomToFitSelection | `Shift+3` | `CODES.THREE` + shift, `"contain"` |
| page up/down | `PgUp`/`PgDn` | scroll by `state.height / zoom` (vertical) |
| page left/right | `Shift+PgUp/PgDn` | scroll by `state.width / zoom` (horizontal); PgDn = negative offset (`App.tsx maybeHandlePageScrollKeyDown`) |
| zenMode | `Alt+Z` | `!Mod && event.altKey && event.code === CODES.Z` |
| objectsSnapMode | `Alt+S` | `!Mod && altKey && code === CODES.S` |
| toggleGrid | `Mod+'` | `event[KEYS.CTRL_OR_CMD] && event.code === CODES.QUOTE` |
| viewMode | `Alt+R` | `!Mod && altKey && code === CODES.R` |
| toggleTheme | `Alt+Shift+D` | `!Mod && altKey && shiftKey && code === CODES.D` |
| stats | `Alt+/` | `!Mod && altKey && code === CODES.SLASH` |
| search | `Mod+F` | (shortcutMap `searchMenu`) |
| commandPalette | `Mod+/`, `Mod+Shift+P` | (out of scope for port) |

#### Editor section
| Label | Keys | keyTest / notes |
|---|---|---|
| createFlowchart | `Mod+Arrow` | new node of same type, `VERTICAL_OFFSET = 100`, `HORIZONTAL_OFFSET = 100`, connected by `elbowed: true` arrow; repeated presses in same direction add siblings (`element/src/flowchart.ts`) |
| navigateFlowchart | `Alt+Arrow` | cycles same-level nodes |
| moveCanvas | `Space+drag`, `Wheel(middle button)+drag` | Space sets `pan.setSpaceHeld(true)`, cursor GRAB |
| clearReset | `Mod+Delete` (also Mod+Backspace) | opens confirm dialog `clearCanvas` |
| delete | `Delete` | `(event.key === KEYS.BACKSPACE \|\| event.key === KEYS.DELETE) && !event[KEYS.CTRL_OR_CMD]` (actionDeleteSelected) |
| cut | `Mod+X` | `event[KEYS.CTRL_OR_CMD] && event.key === KEYS.X` |
| copy / paste | `Mod+C` / `Mod+V` | no keyTest; handled by DOM copy/paste events |
| pasteAsPlaintext | `Mod+Shift+V` | `IS_PLAIN_PASTE = event.shiftKey` flag for 100 ms before paste event |
| selectAll | `Mod+A` | `event[KEYS.CTRL_OR_CMD] && event.key === KEYS.A` |
| multiSelect | `Shift+click` | |
| deepSelect | `Mod+click` | select inside group |
| deepBoxSelect | `Mod+drag` | box-select inside groups |
| copyAsPng | `Shift+Alt+C` | `event.code === CODES.C && event.altKey && event.shiftKey` |
| copyStyles | `Mod+Alt+C` | `Mod && altKey && code === CODES.C` |
| pasteStyles | `Mod+Alt+V` | `Mod && altKey && code === CODES.V` |
| sendToBack | mac `Mod+Alt+[`, else `Mod+Shift+[` | uses `event.code === CODES.BRACKET_LEFT` |
| bringToFront | mac `Mod+Alt+]`, else `Mod+Shift+]` | `CODES.BRACKET_RIGHT` |
| sendBackward | `Mod+[` | `Mod && !shiftKey && code === BRACKET_LEFT` |
| bringForward | `Mod+]` | `Mod && !shiftKey && code === BRACKET_RIGHT` |
| alignTop/Bottom/Left/Right | `Mod+Shift+Up/Down/Left/Right` | `Mod && shiftKey && event.key === KEYS.ARROW_*` (actionAlign.tsx); align center H/V have no shortcut |
| distributeHorizontally / Vertically | `Alt+H` / `Alt+V` | `!Mod && altKey && code === CODES.H / CODES.V` (actionDistribute.tsx; not in help dialog) |
| duplicateSelection | `Mod+D`, `Alt+drag` | `event[KEYS.CTRL_OR_CMD] && event.key === KEYS.D` |
| toggleElementLock | `Mod+Shift+L` | `event.key.toLocaleLowerCase() === KEYS.L && Mod && shiftKey` |
| undo | `Mod+Z` | `event[KEYS.CTRL_OR_CMD] && matchKey(event, KEYS.Z) && !event.shiftKey` |
| redo | `Mod+Shift+Z`; Windows also `Mod+Y` | `(Mod && shiftKey && matchKey(event, KEYS.Z)) \|\| (Mod && !shiftKey && matchKey(event, KEYS.Y))` (Y accepted on all platforms in keyTest) |
| group | `Mod+G` | `!event.shiftKey && Mod && event.key === KEYS.G` |
| ungroup | `Mod+Shift+G` | `event.shiftKey && Mod && event.key === KEYS.G.toUpperCase()` |
| flipHorizontal | `Shift+H` | `event.shiftKey && event.code === CODES.H` |
| flipVertical | `Shift+V` | `event.shiftKey && event.code === CODES.V && !Mod` |
| showStroke (popup) | `S` | opens `openPopup: "elementStroke"` (App.tsx ~6002) |
| showBackground (popup) | `G` | opens `"elementBackground"` only if tool/selection hasBackground |
| showFonts | `Shift+F` | opens `"fontFamily"` if text tool or text/bound-text selected |
| decrease/increaseFontSize | `Mod+Shift+<` / `Mod+Shift+>` | step ×(1/1.1) / ×1.1, rounded (`FONT_SIZE_RELATIVE_INCREASE_STEP = 0.1`) |

`S`/`G` popups are skipped if tool is `selection` and nothing is selected; also require `!altKey && !Mod`.

#### Other shortcuts (shortcutMap / actions, not in help editor list)
| Action | Keys |
|---|---|
| saveToActiveFile / saveScene | `Mod+S` |
| saveFileToDisk ("save as") | `Mod+Shift+S` (actionExport.tsx keyTest; shortcutMap text says Mod+S) |
| loadScene | `Mod+O` |
| imageExport dialog | `Mod+Shift+E` (App.tsx) |
| help dialog | `?` (`event.key === KEYS.QUESTION_MARK`, actionMenu.tsx `actionShortcuts`) |
| deselect | `Escape` (actionDeselect.ts: only when not creating/multi-point/line-editing and something selected / non-default tool / inside group; inside group → exits to parent group) |
| finalize | `Escape` while line editor active, or `Escape`/`Enter` while `multiElement !== null` (actionFinalize.tsx) |
| Enter on single selection | line element or `Mod+Enter` on any linear (non-elbow) → toggle line editor; text or valid text container → start text editing at container center; frame → `editingFrame` (rename) (App.tsx ~5942) |
| Enter with image selected | start crop; Enter/Escape while cropping → finish |
| Tab / Shift+Tab | convert element type (generic: `rectangle, diamond, ellipse`; linear: `line, sharpArrow, curvedArrow, elbowArrow`) — `ConvertElementTypePopup.tsx` |
| holding `Mod` | temporarily inverts binding: `isBindingEnabled: bindingPreference !== "enabled"` |
| `Alt` with bucketfill | temporary eye dropper |

#### Arrow-key nudge (App.tsx ~5901, verbatim)
```ts
const step =
  (this.getEffectiveGridSize() &&
    (event.shiftKey ? ELEMENT_TRANSLATE_AMOUNT : this.getEffectiveGridSize())) ||
  (event.shiftKey ? ELEMENT_SHIFT_TRANSLATE_AMOUNT : ELEMENT_TRANSLATE_AMOUNT);
```
`ELEMENT_TRANSLATE_AMOUNT = 1`, `ELEMENT_SHIFT_TRANSLATE_AMOUNT = 5` (common/src/constants.ts). Grid on: arrow = gridSize, Shift+arrow = 1. Grid off: arrow = 1, Shift+arrow = 5. Selected set includes bound text and frame children (`includeBoundTextElement: true, includeElementsInFrames: true`); bound arrows whose bound target is **not** selected are removed from the moved set; after moving, `updateBoundElements(element, scene, {simultaneouslyUpdated})`.

`getEffectiveGridSize()` (App.tsx line 1521): grid size when grid mode on, else null (UNVERIFIED body). While dragging, holding `Mod` disables grid snapping (`event[KEYS.CTRL_OR_CMD] ? null : this.getEffectiveGridSize()` appears ~12 times).

#### Wheel / zoom (`components/App.wheel.ts`)
- Ctrl/Meta+wheel (and trackpad pinch): `sign = Math.sign(deltaY)`, `MAX_STEP = ZOOM_STEP * 100` (=10); `|delta|` clamped to MAX_STEP; `newZoom = zoom - delta/100; newZoom += Math.log10(Math.max(1, zoom)) * -sign * Math.min(1, absDelta/20)`; zoom anchored at pointer.
- Shift+wheel: horizontal pan `scrollX -= (deltaY || deltaX) / zoom`.
- Plain wheel: `scrollX -= deltaX/zoom; scrollY -= deltaY/zoom`.
- Zoom limits `MIN_ZOOM = 0.1`, `MAX_ZOOM = 30`, `ZOOM_STEP = 0.1` (keyboard zoom in/out adds/subtracts 0.1 around viewport center).

### 3.A.4 Tool behaviour notes (brief)
- **Eraser** (`packages/excalidraw/eraser/index.ts`): uses the last segment of the pointer trail; hit tolerance: freedraw `15` (outline distance `Math.max(2.25, 5 / zoom)`), linear `Math.max(strokeWidth, strokeWidth*2/zoom)`, others `strokeWidth/2`. Hitting one group member erases the whole (shallowest-selected) group; container ↔ bound text erased together; locked elements excluded. Elements pending erase render at opacity `ELEMENT_READY_TO_ERASE_OPACITY = 20`. Alt while erasing = restore (un-mark). Toggle tool: pressing E again returns to previous tool.
- **Hand**: toggle tool, pans only; allowed in view mode.
- **Laser** (`K`): transient trail, no elements (just note; `laser-pointer` package).
- **Lasso**: free-form selection; is a `preferredSelectionTool` variant (`preferredSelectionTool: { type: "selection" | "lasso", initialized }`).
- **Lock** (`Q`): see above.
- **Element links**: `Mod+K` opens link editor on exactly one selected element (`actionLink.tsx` predicate `selectedElements.length === 1`); element-to-element links are URLs with search param `ELEMENT_LINK_KEY` = element id (`element/src/elementLink.ts`, `defaultGetElementLinkFromSelection`). Value of `ELEMENT_LINK_KEY` UNVERIFIED (believed `"element"`).

### 3.A.5 Default appState (`packages/excalidraw/appState.ts`, `getDefaultAppState`)

Relevant constants (common/src/constants.ts, common/src/colors.ts):
`COLOR_PALETTE.black = "#1e1e1e"`, `white = "#ffffff"`, `transparent = "transparent"`; `DEFAULT_ELEMENT_PROPS = { strokeColor: "#1e1e1e", backgroundColor: "transparent", fillStyle: "solid", strokeWidth: 2, strokeStyle: "solid", roughness: 1, opacity: 100, locked: false }`; `DEFAULT_FONT_FAMILY = FONT_FAMILY.Excalifont (5)`; `DEFAULT_FONT_SIZE = 20`; `DEFAULT_TEXT_ALIGN = "left"`; `DEFAULT_VERTICAL_ALIGN = "top"`; `DEFAULT_GRID_SIZE = 20`; `DEFAULT_GRID_STEP = 5`; `DEFAULT_STICKY_NOTE_BG = "#ffdf6b"`; `EXPORT_SCALES = [1, 2, 3]`.

| key | default | saved to .excalidraw (`APP_STATE_STORAGE_CONF.export`) |
|---|---|---|
| currentItemStrokeColor | `"#1e1e1e"` | no (localStorage only) |
| currentItemBackgroundColor | `"transparent"` | no |
| currentItemFillStyle | `"solid"` | no |
| currentItemStrokeWidthKey | `"medium"` (**replaces old `currentItemStrokeWidth` number**) | no |
| currentItemStrokeStyle | `"solid"` | no |
| currentItemRoughness | `1` | no |
| currentItemOpacity | `100` | no |
| currentItemFontFamily | `5` (Excalifont) | no |
| currentItemFontSize | `20` | no |
| currentItemTextAlign | `"left"` | no |
| currentItemStartArrowhead | `null` | no |
| currentItemEndArrowhead | `"arrow"` | no |
| currentItemRoundness | `"round"` (`"sharp"` in test env) | no |
| currentItemArrowType | `"round"` | no |
| currentItemStrokeVariability | `"constant"` (freedraw pressure mode; NEW) | no |
| currentItemStickynoteStrokeColor | `"#1e1e1e"` | no |
| currentItemStickynoteBackgroundColor | `"#ffdf6b"` | no |
| viewBackgroundColor | `"#ffffff"` | **yes** |
| gridSize | `20` | **yes** |
| gridStep | `5` | **yes** |
| gridModeEnabled | `false` | **yes** |
| lockedMultiSelections | `{}` | **yes** |
| theme | `"light"` | no |
| zoom | `{ value: 1 }` | no |
| scrollX / scrollY | `0` / `0` | no |
| activeTool | `{ type: "selection", customType: null, locked: false, fromSelection: false, lastActiveTool: null }` | no |
| preferredSelectionTool | `{ type: "selection", initialized: false }` | no |
| exportBackground | `true` | no |
| exportScale | `defaultExportScale` = `devicePixelRatio` if in `EXPORT_SCALES` else `1` | no |
| exportEmbedScene | `false` | no |
| exportWithDarkMode | `false` | no |
| isBindingEnabled | `true` | no |
| bindingPreference | `"enabled"` | no |
| bindMode | `"orbit"` | no |
| isMidpointSnappingEnabled | `true` | no |
| objectsSnapModeEnabled | `false` | no |
| boxSelectionMode | `"contain"` | no |
| zenModeEnabled / viewModeEnabled | `false` / `false` | no |
| frameRendering | `{ enabled: true, clip: true, name: true, outline: true }` | no |
| selectedElementIds / selectedGroupIds / editingGroupId | `{}` / `{}` / `null` | no (browser only) |
| name | `null` | no (browser) |
| stats | `{ open: false, panels: generalStats \| elementProperties }` | no |
| penMode / penDetected | `false` | no |
| showHints | `true` | no |
| colorTopPicks | `{ elementStroke: null, elementBackground: null, bucketFill: null, stickyNoteStroke: null, stickyNoteBackground: null }` | no |

Other defaults (all ephemeral, `null`/`false`/`{}`): `newElement, editingTextElement, multiElement, resizingElement, selectionElement, selectedLinearElement, suggestedBinding, frameToHighlight, editingFrame, elementsToHighlight, croppingElementId, isCropping, searchMatches, activeLockedId, hoveredElementIds, snapLines: [], originSnapOffset: {x:0,y:0}, scrollConstraints, contextMenu, openMenu, openPopup, openSidebar, openDialog, toast, errorMessage, fileHandle, showWelcomeScreen:false, collaborators: new Map(), cursorButton:"up", inputDevice:"auto", lastPointerDownWith:"mouse", previousSelectedElementIds:{}, shouldCacheIgnoreZoom:false, textToolHover:null, fontTopPicks:null, defaultSidebarDockedPreference:false, isLoading/isResizing/isRotating:false, scrolledOutside:false, showHyperlinkPopup:false, activeEmbeddable:null`.

Only `viewBackgroundColor, gridSize, gridStep, gridModeEnabled, lockedMultiSelections` have `export: true` (and `server: true`) in `APP_STATE_STORAGE_CONF`. `cleanAppStateForExport` filters by that flag.

### 3.A.6 Style properties panel

#### Value sets (common/src/constants.ts, actions/actionProperties.tsx)
| Property (action) | UI options → stored value | Notes |
|---|---|---|
| strokeColor (`changeStrokeColor`) | color picker; quick picks `black #1e1e1e, red[4] #e03131, green[4] #2f9e44, blue[4] #1971c2, yellow[4] #f08c00` | `DEFAULT_ELEMENT_STROKE_COLOR_INDEX = 4` (darkest shade) |
| backgroundColor (`changeBackgroundColor`) | quick picks `transparent, red[1] #ffc9c9, green[1] #b2f2bb, blue[1] #a5d8ff, yellow[1] #ffec99` | `DEFAULT_ELEMENT_BACKGROUND_COLOR_INDEX = 1`. Setting non-transparent bg on lines that `canBecomePolygon(points)` **forces them to polygon** (`shouldEnablePolygon = !isTransparent(color) && selectedElements.every(el => isLineElement(el) && canBecomePolygon(el.points))`) |
| fillStyle (`changeFillStyle`) | `hachure`, `cross-hatch`, `solid`, `zigzag` (zigzag reachable via UI — UNVERIFIED whether shown only on alt-click) | |
| strokeWidth (`changeStrokeWidth`) | keys `thin`, `medium`, `bold` → `STROKE_WIDTH = { thin: 1, medium: 2, bold: 4, extraBold: 8 /* unused */ }`; **freedraw uses `FREEDRAW_STROKE_WIDTH = { thin: 0.5, medium: 1, bold: 2, extraBold: 4 /* legacy */ }`** via `getStrokeWidthByKey(type, key)` | source comment: "freedraw schema 2.0 uses thinner stroke … we scale the stroke width by 1/2 (previous, thin was 1, medium 2 etc.)". So old files' freedraw strokeWidth values of 1/2/4 would render thicker than new ones — check restore.ts for migration (UNVERIFIED here). |
| freedraw pressure (`changeFreedrawMode`, label "pressure") | `constant` / `variable` → `element.strokeOptions.variability` | NEW; appState `currentItemStrokeVariability` |
| strokeStyle (`changeStrokeStyle`) | `solid`, `dashed`, `dotted` | |
| roughness (`changeSloppiness`) | `0` architect, `1` artist, `2` cartoonist | `ROUGHNESS = { architect: 0, artist: 1, cartoonist: 2 }` |
| roundness (`changeRoundness`, "edges") | `sharp` → `roundness: null`; `round` → `{ type: isUsingAdaptiveRadius(type) ? ROUNDNESS.ADAPTIVE_RADIUS (3) : ROUNDNESS.PROPORTIONAL_RADIUS (2) }` | `ROUNDNESS = { LEGACY: 1, PROPORTIONAL_RADIUS: 2, ADAPTIVE_RADIUS: 3 }` |
| arrowType (`changeArrowType`) | `sharp` → `roundness:null, elbowed:false`; `round` → `roundness:{type:2}, elbowed:false`; `elbow` → `elbowed:true, roundness:null`, points recomputed (2-point route) | `ARROW_TYPE = { sharp, round, elbow }` |
| arrowheads (`changeArrowhead`, start & end) | `null, arrow, triangle, triangle_outline, circle, circle_outline, diamond, diamond_outline, bar, cardinality_one, cardinality_many, cardinality_one_or_many, cardinality_exactly_one, cardinality_zero_or_one, cardinality_zero_or_many` | cardinality (crow's foot) heads are NEW |
| opacity (`changeOpacity`) | slider `min=0 max=100 step=10` | |
| fontSize (`changeFontSize`) | `FONT_SIZES = { sm: 16, md: 20, lg: 28, xl: 36 }` labels small/medium/large/veryLarge | bound text: `redrawTextBoundingBox(text, container)` after change (container grows) |
| fontSize step (`increaseFontSize`/`decreaseFontSize`) | ×1.1 / ÷1.1, `Math.round` | `MIN_FONT_SIZE = 1` |
| fontFamily (`changeFontFamily`) | quick picks `DEFAULT_FONTS` (FontPicker.tsx): `Excalifont (5) "Hand Drawn"`, `Nunito (6) "Normal"`, `Comic Shanns (8) "Code"`; full list in dropdown | `FONT_FAMILY = { Virgil: 1, Helvetica: 2, Cascadia: 3, Excalifont: 5, Nunito: 6, "Lilita One": 7, "Comic Shanns": 8, "Liberation Sans": 9, Assistant: 10 }` (note id 4 unused; **Assistant = 10** new) |
| textAlign (`changeTextAlign`) | `left`, `center`, `right` | |
| verticalAlign (`changeVerticalAlign`) | `top`, `middle`, `bottom` | only for bound text (`shouldAllowVerticalAlign`) |
| canvas background (`changeViewBackgroundColor`) | picks `#ffffff, #f8f9fa, #f5faff, #fffce8, #fdf8f6` | |
| sticky note bg | picks `#ffdf6b, #fcc2d7, #b2f2bb, #a5d8ff, #ffd8a8` | |

Full `COLOR_PALETTE` (common/src/colors.ts; open-color shades 50/200/400/600/800 = index 0..4):
| family | 0 | 1 | 2 | 3 | 4 |
|---|---|---|---|---|---|
| gray | #f8f9fa | #e9ecef | #ced4da | #868e96 | #343a40 |
| red | #fff5f5 | #ffc9c9 | #ff8787 | #fa5252 | #e03131 |
| pink | #fff0f6 | #fcc2d7 | #f783ac | #e64980 | #c2255c |
| grape | #f8f0fc | #eebefa | #da77f2 | #be4bdb | #9c36b5 |
| violet | #f3f0ff | #d0bfff | #9775fa | #7950f2 | #6741d9 |
| blue | #e7f5ff | #a5d8ff | #4dabf7 | #228be6 | #1971c2 |
| cyan | #e3fafc | #99e9f2 | #3bc9db | #15aabf | #0c8599 |
| teal | #e6fcf5 | #96f2d7 | #38d9a9 | #12b886 | #099268 |
| green | #ebfbee | #b2f2bb | #69db7c | #40c057 | #2f9e44 |
| yellow | #fff9db | #ffec99 | #ffd43b | #fab005 | #f08c00 |
| orange | #fff4e6 | #ffd8a8 | #ffa94d | #fd7e14 | #e8590c |
| bronze | #f8f1ee | #eaddd7 | #d2bab0 | #a18072 | #846358 |

Dark mode colors: `applyDarkModeFilter(color)` in colors.ts = CSS `invert(93%)` then `hue-rotate(180deg)` applied per color (invert: `c*(1-p) + (255-c)*p`, p=0.93; then 3×3 hue-rotate matrix), cached in a Map; `removeDarkModeFilter` inverts it. (Exact matrix coefficients: standard CSS hue-rotate, UNVERIFIED verbatim.)

#### Which properties apply to which element type (`packages/element/src/comparisons.ts`)
| predicate | element types |
|---|---|
| `hasBackground` | rectangle, stickynote, iframe, embeddable, ellipse, diamond, line, freedraw, autoshape, bucketfill |
| `hasFillStyle` | `hasBackground(type) && type !== "stickynote"` |
| `hasStrokeColor` | rectangle, stickynote, ellipse, diamond, freedraw, arrow, line, text, embeddable, autoshape |
| `hasStrokeWidth` | rectangle, iframe, embeddable, ellipse, diamond, freedraw, arrow, line, autoshape |
| `hasStrokeStyle` | rectangle, iframe, embeddable, ellipse, diamond, arrow, line, autoshape (NOT freedraw) |
| `hasRoughness` | `hasStrokeStyle(type) \|\| type === "stickynote"` |
| `hasFreedrawMode` | freedraw |
| `canChangeRoundness` | rectangle, iframe, embeddable, line, diamond, stickynote, image (NOT ellipse, NOT arrow — arrow uses arrowType) |
| `toolIsArrow` / `canHaveArrowheads` | arrow |

Roundness type per element (`packages/element/src/typeChecks.ts`):
- `isUsingAdaptiveRadius`: rectangle, embeddable, iframe, image → `ADAPTIVE_RADIUS`
- `isUsingProportionalRadius`: line, arrow, diamond, stickynote → `PROPORTIONAL_RADIUS`
- `canApplyRoundnessTypeToElement`: ADAPTIVE or LEGACY on adaptive types; PROPORTIONAL on proportional types; else false.
- `getDefaultRoundnessTypeForElement`: proportional types → `{type:2}`, adaptive → `{type:3}`, else `null`.

Panel visibility (`packages/excalidraw/components/shapeActionPredicates.ts` `getShapeActionPredicates`; `forToolOrSelection(p)` = p(activeTool) or any selected element satisfies p):
| panel | predicate |
|---|---|
| strokeColor | `canChangeStrokeColor(appState, targetElements)` |
| backgroundColor | `canChangeBackgroundColor(...)` |
| fill | `activeTool === "bucketfill" \|\| (hasFillStyle(activeTool) && bg not transparent) \|\| some selected …` |
| strokeWidth | `forToolOrSelection(hasStrokeWidth)` |
| freedrawMode | `forToolOrSelection(hasFreedrawMode)` |
| strokeStyle | `forToolOrSelection(hasStrokeStyle)` |
| sloppiness | `forToolOrSelection(hasRoughness)` |
| roundness | `forToolOrSelection(canChangeRoundness)` |
| arrowType / arrowheads | `forToolOrSelection(toolIsArrow / canHaveArrowheads)` |
| fontFamily, fontSize | `activeTool === "text" \|\| targetElements.some(isTextElement)` |
| textAlign | text tool or `suppportsHorizontalAlign(...)` |
| verticalAlign | `shouldAllowVerticalAlign(targetElements, elementsMap)` |
| opacity | `activeTool !== "autoshape" \|\| hasSelection` |
| layers (z-order) | not freedraw tool … or selection |
| align | `!isSingleElementBoundContainer && alignActionsPredicate` (≥2 group-units selected, no frames) |
| distribute | `targetElements.length > 2` |
| duplicate/delete/group/ungroup | `hasSelection && !isEditingTextOrNewElement` |
| link | `singleSelected \|\| isSingleElementBoundContainer` |
| cropEditor | `!croppingElementId && singleSelected && isImageElement` |
| lineEditor | `!selectedLinearElement?.isEditing && singleSelected && isLinearElement && !isElbowArrow` |

Order in panel (Actions.tsx `SelectedShapeActions`): stroke, background, fill, strokeWidth, strokeStyle, freedrawMode, sloppiness, roundness, arrowType, fontFamily, fontSize, textAlign, verticalAlign, arrowheads, opacity, layers, align/distribute, duplicate/delete/group/ungroup, link, crop, lineEditor.

The panel is shown when (`element/src/showSelectedShapeActions.ts`): not view mode, not in element-link dialog, and (editing text, or active tool is a drawing tool — not selection/lasso/eraser/hand/laser — or some element is selected). When shown for an active tool with no selection, changes go to `currentItem*` only.

#### Copy/paste styles (`actions/actionStyles.ts`)
copyStyles `Mod+Alt+C`, pasteStyles `Mod+Alt+V`. Pasted onto all: `backgroundColor, strokeWidth, strokeColor, strokeStyle, fillStyle, opacity, roughness, roundness`; text only: `fontSize, fontFamily, textAlign, lineHeight`; arrows only: `startArrowhead, endArrowhead`; frame-like targets force `roundness: null, backgroundColor: "transparent"`.

### 3.A.7 Action semantics worth porting
| Action | Behaviour (source) |
|---|---|
| duplicate `Mod+D` | offset `x + DEFAULT_GRID_SIZE/2, y + DEFAULT_GRID_SIZE/2` (= +10,+10); includes bound text; duplicates stay in duplicated frame if frame also duplicated; duplicates become selection (actionDuplicateSelection.tsx) |
| delete | container → bound text deleted too; frame deleted → **children kept, `frameId: null`, become selected**; elbow arrows bound to deleted element get binding nulled; in line editor deletes selected points (all points → element deleted) (actionDeleteSelected.tsx) |
| selectAll | excludes deleted, bound text (`containerId`), and `locked` elements (actionSelectAll.ts) |
| group | ≥2 elements not already one group; new groupId **appended** to `groupIds` (last = outermost) via `addToGroup(groupIds, newId, editingGroupId)`; cannot group frame with its children; grouping elements from different frames removes them from frames (actionGroup.tsx) |
| ungroup | removes only the selected group ids (`removeFromSelectedGroups`) |
| flip H/V | pivot = common bbox center; if *all* selected are bound arrows, only swap start/end arrowheads; elbow arrows re-routed and re-centered (actionFlip.ts) |
| lock `Mod+Shift+L` | toggles `locked`; locking a multi-selection adds a temp groupId recorded in `appState.lockedMultiSelections`; `unlockAllElements` action (no key) (actionElementLock.ts) |
| wrapSelectionInFrame | new frame around selection with **16 px** padding (actionFrame.ts) |
| selectAllElementsInFrame / removeAllElementsFromFrame | single frame selected |
| bindText / unbindText / wrapTextInContainer | bind: exactly 1 text + 1 bindable container w/o text → text `textAlign center, verticalAlign middle, autoResize true`, text placed above container in z-order; wrap: creates **rectangle** container around text using `BOUND_TEXT_PADDING` (actionBoundText.tsx) |
| togglePolygon | lines with ≥4 points; entering polygon sets a background color, leaving sets `"transparent"`; point closing in `toggleLinePolygonState` (actionLinearEditor.tsx) |
| autoResize | text with `autoResize:false` → measure text, set width/height, `autoResize:true`, keep anchor by alignment (actionTextAutoResize.ts) |
| cropEditor | sets `croppingElementId` (actionCropEditor.tsx) |
| arrowBinding toggle | flips `bindingPreference` enabled/disabled and `isBindingEnabled` (actionToggleArrowBinding.tsx, no key) |
| midpointSnapping | toggles `isMidpointSnappingEnabled` (no key) |
| clearCanvas | marks all deleted; resets appState preserving theme, penMode, grid, export settings |

### 3.A.8 History / undo (`packages/element/src/store.ts`, `packages/excalidraw/history.ts`, `packages/element/src/delta.ts`)

`CaptureUpdateAction` (store.ts, verbatim values & doc):
- `IMMEDIATELY` — "Immediately undoable … Should be used for most of the local updates, except ephemerals such as dragging or resizing. These updates will _immediately_ make it to the local undo / redo stacks."
- `NEVER` — "Never undoable … remote updates or scene initialization."
- `EVENTUALLY` — "Eventually undoable … not captured immediately … all such updates would end up being captured with the next `CaptureUpdateAction.IMMEDIATELY`."

Mechanics:
- Every action returns `captureUpdate`. The Store keeps a snapshot (elements + *observed* appState). On commit, `IMMEDIATELY` computes `StoreDelta.calculate(prevSnapshot, nextSnapshot)` → `ElementsDelta` + `AppStateDelta`, emits a **durable increment** (→ history entry) and updates the snapshot. `NEVER` updates the snapshot without a history entry (so the change is never undoable and not folded into later entries). `EVENTUALLY` does NOT update the snapshot, so the change gets folded into the next IMMEDIATELY capture.
- Consequently one undo step = everything changed between two IMMEDIATELY captures. Pointer gestures (drag, resize, rotate, draw) mutate with EVENTUALLY during the gesture and are captured once on pointer-up (`store.scheduleCapture()`), giving one step per gesture (general pattern; exact per-gesture call sites UNVERIFIED).
- `scheduleMicroAction` lets code queue a capture of an intermediate state before the macro action.
- **appState IS part of history, but only these observed keys** (`getObservedAppState`): `name, editingGroupId, viewBackgroundColor, selectedElementIds, selectedGroupIds, selectedLinearElement, croppingElementId, activeLockedId, lockedMultiSelections`.
- `History.record(delta)`: ignores empty deltas and deltas that are themselves `HistoryDelta` (i.e. undo/redo results). Pushes to `undoStack`; **clears `redoStack` only if the delta has element changes** ("don't reset redo stack on local appState changes, as a simple click (unselect) could lead to losing all the redo entries"). So selection-only changes are their own undo entries but don't kill redo.
- Undo/redo (`perform`): pop entry, apply to current elements/appState, if result `containsVisibleChange` stop, otherwise keep popping (entries with no visible effect, e.g. selection of since-deleted elements, are skipped); the inverse of each applied entry is pushed to the opposite stack. No stack size limit.
- Undo/redo are blocked (return `EVENTUALLY`, do nothing) while editing text, drawing a multi-point element, resizing, dragging, box-selecting, creating a flowchart, or mid draw gesture (actionHistory.tsx); successful undo returns `NEVER`.
- Deltas are per-element property diffs keyed by id with `version`; applying an undo on elements that were changed since is done by merging latest changes (delta.ts; details UNVERIFIED). For a single-user native port a snapshot-diff of elements + the 9 observed appState keys reproduces the semantics.

### 3.A.9 Surprises / items for implementers
1. `SHAPES`/`shapes.tsx` is gone → `Tools.tsx` `TOOLS`. New tools: `stickynote` (N), `autoshape` (Shift+X), `bucketfill` (B). Image has **no letter key** (only `9`); `I` is now the eye dropper.
2. `X` is an alias for freedraw; `Shift+X` is autoshape — tool key matching is Shift-sensitive.
3. Pressing `A` again with arrow tool active cycles arrow type sharp→round→elbow.
4. appState uses `currentItemStrokeWidthKey` ("thin"/"medium"/"bold") not a number; `STROKE_WIDTH` is now `thin 1 / medium 2 / bold 4` and freedraw is halved (`0.5/1/2`) "freedraw schema 2.0".
5. Font id 10 = `Assistant`; picker quick-picks are Excalifont/Nunito/Comic Shanns.
6. Arrowheads include 6 `cardinality_*` crow's-foot variants.
7. Deleting a frame keeps its children.
8. Only 5 appState keys are written to `.excalidraw` files (`viewBackgroundColor, gridSize, gridStep, gridModeEnabled, lockedMultiSelections`).
9. Redo stack survives selection-only changes; undo walks past invisible entries.

## 3b. Interaction behaviors (creation, point editor, selection, transform, binding, snapping, eraser, links, frames, duplicate, tool lock)

Source: excalidraw `master` fetched 2026-09-27 (sourcegraph reported commit `438d898` for App.tsx). Paths are relative to the repo root. `App.tsx` = `packages/excalidraw/components/App.tsx` (~14k lines; line numbers quoted below are approximate and drift).

### 3b.0 Constants used by interaction code

All in `packages/common/src/constants.ts` unless noted.

| Constant | Value | Used for |
|---|---|---|
| `DRAGGING_THRESHOLD` | `10` (px) | Bound single arrow must be dragged >10 scene units before it moves/unbinds (`dragElements.ts` `dragSelectedElements`); sticky-note click-vs-drag; double-tap distance check |
| `LINE_CONFIRM_THRESHOLD` | `8` (px) | Click within 8 px of last committed point finalizes a multi-point line/arrow; `isPathALoop` closes a line when first–last distance `<= 8/zoom` |
| `MINIMUM_ARROW_SIZE` | `20` (px, screen) | A linear drag shorter than 20 screen px counts as a click → enters click-click (multiElement) mode |
| `SHIFT_LOCKING_ANGLE` | `Math.PI / 12` (15°) | Shift-constrained line angles and Shift rotation snapping |
| `TAP_TWICE_TIMEOUT` | `300` ms | double tap (touch) |
| `DOUBLE_TAP_POSITION_THRESHOLD` | `35` | double tap max distance |
| `TOUCH_CTX_MENU_TIMEOUT` | `500` ms | long-press context menu |
| `BIND_MODE_TIMEOUT` | `700` ms | Hovering a dragged arrow endpoint over a shape this long switches `bindMode` to `"inside"` |
| `ELEMENT_TRANSLATE_AMOUNT` | `1` | arrow-key nudge |
| `ELEMENT_SHIFT_TRANSLATE_AMOUNT` | `5` | Shift+arrow nudge |
| `DEFAULT_TRANSFORM_HANDLE_SPACING` | `2` | handle margin |
| `SIDE_RESIZING_THRESHOLD` | `2 * DEFAULT_TRANSFORM_HANDLE_SPACING` = `4` | edge-resize hit band (screen px) |
| `EPSILON` | `0.00001` | |
| `DEFAULT_COLLISION_THRESHOLD` | `2 * SIDE_RESIZING_THRESHOLD - EPSILON` = `7.99999` | base hit threshold |
| `TEXT_AUTOWRAP_THRESHOLD` | `36` (px) | dragging a new text box wider than 36/zoom turns off `autoResize` (fixed-width text) |
| `LINE_POLYGON_POINT_MERGE_DISTANCE` | `20` | converting a line to polygon: first/last points closer than 20 are merged, otherwise a closing point is appended (only if ≥4 points; `shape.ts`) |
| `DEFAULT_GRID_SIZE` / `DEFAULT_GRID_STEP` | `20` / `5` | grid |
| `ZOOM_STEP` / `MIN_ZOOM` / `MAX_ZOOM` | `0.1` / `0.1` / `30` | |
| `HYPERLINK_TOOLTIP_DELAY` | `300` ms | link hover tooltip |
| `ELEMENT_READY_TO_ERASE_OPACITY` | `20` | elements under the eraser trail are drawn at 20% opacity |
| `INVISIBLY_SMALL_ELEMENT_SIZE` | `0.1` (`element/src/sizeHelpers.ts`) | |
| `MIN_FONT_SIZE` | `1` | text resize floor |
| `DEFAULT_STICKY_NOTE_SIZE` / `STICKY_NOTE_MIN_SIZE` / `STICKY_NOTE_MIN_FONT_SIZE` | `250` / `75` / `16` | sticky notes (new element type, see 3b.12) |
| `FRAME_NAME_EDIT_PADDING` (App.tsx) | `6` | frame-name input |
| `DEFAULT_LINK_SIZE` | `12` (`components/hyperlink/helpers.ts`, hit test) / `14` (`element/src/renderElement.ts`, drawing) | link icon |
| `SNAP_DISTANCE` (`packages/excalidraw/snapping.ts`) | `8` (screen px; `getSnapDistance = 8 / zoom`) | object snapping |
| `LinearElementEditor.POINT_HANDLE_SIZE` (`linearElementEditor.ts`) | `10` | point handles |
| `transformHandleSizes` (`transformHandles.ts`) | `{ mouse: 8, pen: 16, touch: 28 }` | resize handle size (screen px) |
| `ROTATION_RESIZE_HANDLE_GAP` (`transformHandles.ts`) | `16` | rotation handle distance above the top handles (screen px) |
| `BASE_BINDING_GAP` (`binding.ts`) | `5` | arrow→shape gap |
| `BASE_ARROW_MIN_LENGTH` (`binding.ts`) | `10` | |
| `FOCUS_POINT_SIZE` (`binding.ts`) | `10 / 1.5` | |
| `MIN_BINDABLE_SIZE` (`binding.ts`) | `1` | |

Modifier helpers (`packages/common/src/keys.ts`):

```ts
export const shouldResizeFromCenter = (event) => event.altKey;
export const shouldMaintainAspectRatio = (event) => event.shiftKey;
export const shouldRotateWithDiscreteAngle = (event) => event.shiftKey;
```

`KEYS.CTRL_OR_CMD` is Cmd on macOS and Ctrl elsewhere. In almost every drag path, holding CTRL_OR_CMD turns grid snapping off for that gesture (`getGridPoint(x, y, event[KEYS.CTRL_OR_CMD] ? null : this.getEffectiveGridSize())`).

Relevant `getDefaultAppState` defaults (`packages/excalidraw/appState.ts`):
`gridModeEnabled: false`, `gridSize: 20`, `gridStep: 5`, `objectsSnapModeEnabled: false`, `isMidpointSnappingEnabled: true`, `bindingPreference: "enabled"`, `isBindingEnabled: true`, `bindMode: "orbit"`, `boxSelectionMode: "contain"`, `preferredSelectionTool: { type: "selection", initialized: false }`, `activeTool: { type: "selection", customType: null, locked: false, fromSelection: false, lastActiveTool: null }`, `editingGroupId: null`, `frameRendering: { enabled: true, clip: true, name: true, outline: true }`, `currentItemArrowType: "round"`, `currentItemRoundness: "round"` (`"sharp"` only in the test env).

`TOOL_TYPE` (constants.ts): `selection, lasso, rectangle, diamond, ellipse, arrow, line, freedraw, text, image, eraser, hand, frame, magicframe, stickynote, embeddable, laser, autoshape, bucketfill`. `stickynote`, `autoshape` and `bucketfill` are new on master (see 3b.12).

---

### 3b.1 Creating elements

#### Pointer-down: creating the element (App.tsx `createGenericElementOnPointerDown`, `handleLinearElementOnPointerDown`, `createFrameElementOnPointerDown`)

- The origin is snapped to the grid first: `[gridX, gridY] = getGridPoint(origin.x, origin.y, ctrlOrCmd ? null : effectiveGridSize)`. `getGridPoint` rounds each coordinate to the nearest multiple: `Math.round(x / gridSize) * gridSize` (`packages/common/src/points.ts`). `getEffectiveGridSize()` returns `gridSize` only when grid mode is on, otherwise `null`.
- `frameId` is set to `getTopLayerFrameAtSceneCoords({x: gridX, y: gridY})`. That function takes the unlocked frames whose bounds contain the point (`isCursorInFrame`) and picks the last one (topmost in z-order), then does a further hit check.
- Generic shapes (rectangle, diamond, ellipse, embeddable, stickynote, and the internal `selection` box) are built from the `currentItem*` style: strokeColor, backgroundColor, fillStyle, strokeWidth (`getCurrentItemStrokeWidth(type)`), strokeStyle, roughness, opacity, `roundness: getCurrentItemRoundness(type)`, `locked: false`. Sticky notes use `currentItemStickynoteStrokeColor` and `currentItemStickynoteBackgroundColor` instead. The element is inserted right away with 0×0 size and `newElement` is set to it.
- Frames use `{x, y, opacity: currentItemOpacity, locked: false, ...FRAME_STYLE}` with `newFrameElement`.
- Arrows use `newArrowElement`:
  - `roundness`: `{type: PROPORTIONAL_RADIUS}` when `currentItemArrowType === "round"`, otherwise `null`.
  - `elbowed`: `currentItemArrowType === "elbow"`.
  - `fixedSegments`: `[]` for elbow arrows, otherwise `null`.
  - `startArrowhead` / `endArrowhead` come from `currentItemStartArrowhead` / `currentItemEndArrowhead`.
- Lines use `newLinearElement` with `roundness` `{type: PROPORTIONAL_RADIUS}` when `currentItemRoundness === "round"`, otherwise `null`. Arrowheads are `[null, null]`.
- Linear elements start with `points: [[0,0],[0,0]]`.
- Holding Ctrl at pointer-down on a linear tool flips `isBindingEnabled` for the gesture: `isBindingEnabled: bindingPreference !== "enabled"`. The same flip happens on keydown of CTRL_OR_CMD while drawing.

#### Pointer-move: drag-to-size (App.tsx `maybeDragNewGenericElement` → `element/src/dragElements.ts` `dragNewElement`)

- The pointer is grid-snapped and then object-snapped (`snapNewElement`).
- Aspect lock: `shouldMaintainAspectRatio` is Shift for ordinary shapes. It is inverted for images and sticky notes, which are proportional by default and freed by Shift.
- From center: `shouldResizeFromCenter` is Alt.
- The `selection` box is never aspect-locked.
- `dragNewElement` logic (verbatim essence):

```ts
if (shouldMaintainAspectRatio && newElement.type !== "selection") {
  if (widthAspectRatio) height = width / widthAspectRatio;          // images
  else if (|y-originY| > |x-originX|)
    ({width,height} = getPerfectElementSize(type, height, x<originX ? -width : width));
  else
    ({width,height} = getPerfectElementSize(type, width,  y<originY ? -height : height));
  if (height < 0) height = -height;
}
let newX = x < originX ? originX - width : originX;
let newY = y < originY ? originY - height : originY;
if (shouldResizeFromCenter) { width += width; height += height;
  newX = originX - width/2; newY = originY - height/2; }
if (width !== 0 && height !== 0) mutate({x:newX, y:newY, width, height});
```

- `getPerfectElementSize(type, w, h)` (`sizeHelpers.ts`):
  - For `line`/`arrow`/`freedraw` it snaps the angle to a multiple of 15° (`lockedAngle = round(atan(|h|/|w|) / SHIFT_LOCKING_ANGLE) * SHIFT_LOCKING_ANGLE`; if 0 then h=0, if π/2 then w=0, else `h = |w| * tan(lockedAngle) * sign(h)`).
  - For other non-selection types it makes a square: `height = |width| * sign(height)`.
- Linear elements being dragged (2nd point follows the pointer) use `getLockedLinearCursorAlignSize(originX, originY, x, y, customAngle?)` under Shift. It locks to the nearest 15° multiple and projects the pointer perpendicularly onto that ray. If `customAngle` is given, e.g. the angle of the previous segment, the lock snaps to it when within `SHIFT_LOCKING_ANGLE / 6` (2.5°).
- Text (`dragNewTextElement`): dragging sets the box width, with a minimum of `getMinTextElementWidth(font, lineHeight)`. The anchor ratio is 0.5 with Alt, 1 when dragging left, 0 when dragging right. Once the reach exceeds `TEXT_AUTOWRAP_THRESHOLD / zoom` (36 px), `autoResize: false` is set, giving fixed-width wrapped text.

#### Pointer-up (App.tsx `handleCanvasPointerUp`, ~L11840–12630)

- **Zero-size shapes are discarded.** For any non-selection tool, if `isInvisiblySmallElement(newElement)`, the element is removed with `CaptureUpdateAction.NEVER`, so no history entry is made. `isInvisiblySmallElement`:
  - Linear/freedraw: `points.length < 2`, or a 2-point *arrow* whose points are equal within 0.1.
  - Everything else: `width === 0 && height === 0`.
  - A plain click with the rectangle/ellipse/diamond tool therefore creates **nothing** (there is no default size).
  - Exception: a sticky-note click (drag `< DRAGGING_THRESHOLD` screen px in both axes) creates a 250×250 note centered on the pointer. A dragged sticky note is clamped to its minimum size.
- New frame: `getElementsInNewFrame(...)` is computed and those elements are added via `addElementsToFrame` (see 3b.9).
- Every new element then gets `getNormalizedDimensions(newElement)`, which makes width/height positive by moving x/y.
- Freedraw pointer-up appends a final point. If that point equals the first, it adds `+0.0001` to dx/dy "to allow dots". It appends the pressure unless `simulatePressure`, then runs `actionFinalize`.
- New text: if width < min width, `autoResize: true` is set, then the WYSIWYG editor opens.
- **Linear elements (line/arrow)**:
  - If there was no drag, or the drag distance × zoom `< MINIMUM_ARROW_SIZE` (20), and no multiElement exists yet:
    - On touch screens, a fixed horizontal line of length `min(0.7*viewportWidth/zoom, 100)` is created centered on the pointer and finalized.
    - Otherwise the app enters **click-click mode**: `multiElement = newElement`. "Movement out of commit area will create the point."
  - If there was a drag and no multiElement: `actionFinalize`. Then, unless the tool is locked, it switches to the preferred selection tool, selects the new element and creates a `LinearElementEditor` for it (not in editing mode).
- **Tool revert after creation**:
  - If `!isToolLocked()` and the tool is not `freedraw`, the new element is selected.
  - On a real `pointerup`, when the tool is not locked, not `freedraw`, not `bucketfill`, and (not lasso, or lasso started from selection), `activeTool` resets to `preferredSelectionTool.type`.
  - **Freedraw stays active** after each stroke even without the lock.
  - `isToolLocked()` = `activeTool.locked || props.activeTool != null`.
- Embeddables open the link editor right after creation (`showHyperlinkPopup: "editor"`).

#### Click-click multi-point lines/arrows (App.tsx `handleLinearElementOnPointerDown`, multiElement branch)

Each pointer-down while `multiElement` is set:
1. **Line loop:** if the multiElement is a `line` and `isPathALoop(points, zoom)` (≥3 points and first–last distance ≤ `8/zoom`), it finalizes as a closed loop.
2. **Elbow arrows** accept only start and end. If `points.length > 1`, the next click finalizes.
3. With binding enabled, it computes the binding strategy for the last point, then finalizes immediately if:
   - the end would orbit-bind to an element other than the start's binding, or
   - both ends bind to the same element and the end point lands outside it, or
   - `points.length > 1` and the click is within `LINE_CONFIRM_THRESHOLD` (8) of `lastCommittedPoint`. In other words, clicking the last point again finishes the line.
4. Otherwise the element stays selected and the cursor becomes a pointer. The point under the pointer is committed and a new floating point follows the mouse.

Other ways to finish: Escape / Enter run `actionFinalize` (keyboard section). Double-clicking with an arrow/line tool while in multiElement mode is ignored (`handleCanvasDoubleClick` returns early), so it does not create text.

---

### 3b.2 Linear element point editor (`packages/element/src/linearElementEditor.ts`, `actions/actionLinearEditor.tsx`)

- **State:** `appState.selectedLinearElement` (a `LinearElementEditor` object) exists whenever a single linear element is selected. `isEditing: true` means full point-editing mode.
- **Entering edit mode** (`actionToggleLinearEditor`, predicate: exactly 1 selected linear element, not elbow, not already editing; no keyTest of its own). Triggers:
  - Double-click a **line**: enters the editor. Double-click an **arrow** only with CTRL_OR_CMD held. This is because a plain double-click on an arrow edits its label (`handleCanvasDoubleClick`).
  - **Enter** with a single selected **line**, or **Ctrl/Cmd+Enter** with any single linear element except elbow arrows (App.tsx keydown).
  - Double-click inside the editor of the same line does nothing more.
  - Clicking elsewhere (a hit on anything other than the edited element) leaves edit mode (`handleSelectionOnPointerDown` sets `isEditing` false).
- **Handles:**
  - `POINT_HANDLE_SIZE = 10`. A point is hit when its distance × zoom `< POINT_HANDLE_SIZE + 1` (11 screen px; "+1px to account for outline stroke").
  - Segment midpoint knob hit radius is `(POINT_HANDLE_SIZE + 1) / zoom`.
  - Midpoints are **not offered on short segments**: `isSegmentTooShort` ⇔ `distance * zoom < POINT_HANDLE_SIZE * 4` (40 screen px).
- **Adding points:**
  - Dragging a segment midpoint inserts a new point there.
  - In edit mode, **Alt+click** appends a point at the pointer (`handlePointerDown`: `event.altKey && isEditing` → `points: [...points, createPointAt(..., ctrlOrCmd ? null : gridSize)]`).
- **Dragging points:** points snap to the grid unless CTRL_OR_CMD is held. Shift with a single point dragged locks the segment angle via `_getShiftLockedDelta(..., customLineAngle)` in 15° steps relative to the pivot (the neighbour point).
- **Selecting several points:** Shift-click toggles points (on pointer-up, "when inside line editor, shift selects points instead"). Box selection inside the editor selects points.
- **Deleting points:**
  - Delete/Backspace with points selected calls `LinearElementEditor.deletePoints` (`actionDeleteSelected`). If all points are selected, the whole element is deleted.
  - For a polygon, when point 0 or the last point is involved, the first point is re-synced to the last (`nextPoints[0] = nextPoints[last]`).
  - UNVERIFIED: the exact rule when exactly 1 point would remain.
- **Duplicating points:** Ctrl/Cmd+D in edit mode calls `LinearElementEditor.duplicateSelectedPoints()` (`actionDuplicateSelection`) instead of duplicating elements.
- **Polygons** (`line.polygon === true`):
  - `actionTogglePolygon` is available when all selected elements are `line`s with **≥4 points** (UNVERIFIED count semantics: includes the closing point).
  - Turning it on runs `toggleLinePolygonState`, which closes the path: first and last points are merged if they are within 20 units, otherwise a closing point is appended (`shape.ts`, `LINE_POLYGON_POINT_MERGE_DISTANCE`).
  - Turning it off sets `backgroundColor: "transparent"`.
  - While editing a polygon, point 0 is always forced equal to the last point.
- **Elbow arrows:**
  - There is no point editor.
  - Dragging a segment midpoint creates or moves a `fixedSegment`.
  - **Double-clicking a segment midpoint deletes that fixed segment** (`LinearElementEditor.deleteFixedSegment`, `handleCanvasDoubleClick`), which returns that part to automatic routing.
  - Elbow arrows show no transform handles.
  - A single selected elbow arrow that is bound cannot be dragged (`dragSelectedElements` returns early). A bound elbow arrow in a multi-selection moves only if both its bound shapes are also selected.

---

### 3b.3 Hit testing (`packages/element/src/collision.ts`, App.tsx)

- **Threshold** (App.tsx `getElementHitThreshold`):

```ts
Math.max(element.strokeWidth / 2 + 0.1,
         0.85 * (DEFAULT_COLLISION_THRESHOLD / zoom))   // 0.85*7.99999 ≈ 6.8 screen px
```

  When several elements overlap, the topmost one is re-tested with half this threshold.
- `hitElementItself({point, element, threshold, frameNameBound})`:
  1. Quick reject by the rotated bounding box, grown by the threshold. Freedraw adds its max stroke radius.
  2. If `shouldTestInside(element)`, the test is point-in-shape OR point-on-outline. Otherwise it is on-outline only.
  3. A hit on the frame's name label also counts, with the label bounds grown by the threshold.
- `shouldTestInside(element)`:

```ts
if (element.type === "arrow") return false;
const isDraggableFromInside =
  (hasBackground(type) && !isTransparent(backgroundColor)) ||
  hasBoundTextElement(element) || isIframeLikeElement(element) || isTextElement(element);
if (type === "line" || type === "freedraw") return isDraggableFromInside && isPathALoop(points);
return isDraggableFromInside || isImageElement(element);
```

  So a **transparent unfilled** rectangle/ellipse/diamond is hit only on its stroke. Filled shapes, shapes with a label, text and images are hit anywhere inside. Lines and freedraw count their inside only when closed (first–last ≤ 8).
- `isPointInElement` uses ray casting: it counts intersections of a ray to an outside point with the element outline, and odd means inside. Freedraw tests against its fill polygon.
- **Bound text:** a hit on a container's bound text counts as a hit on the container (`hitElementBoundText`). Bound text is never selected on its own by click or box.
- **Already-selected elements:** a press inside the *common bounding box* of the current selection (`hasHitCommonBoundingBoxOfSelectedElements`) keeps the selection and starts a drag, even over empty (transparent) area.
- **Locked elements** are hit-tested (`includeLockedElements: true`) but not selected. A locked hit on top with no selected unlocked element under it clears `hit.element`, so the press starts box selection. `activeLockedId` tracks the clicked locked element (for the unlock affordance; UNVERIFIED UI details).

---

### 3b.4 Selection semantics

#### Click (App.tsx `handleSelectionOnPointerDown` and the pointer-up branch)

- Pointer-down on an element:
  - If nothing hit is already selected and **Shift is not held** and the press is outside the selection's common bbox, the selection is cleared first.
  - Then, if the hit element is not selected, it is added (with Shift this adds to the selection; without Shift the old selection is already gone).
  - Invariant: **a frame and its children are never selected at the same time.** Selecting a frame deselects its children. Clicking a child of a selected frame does not select the child. Selecting an element grouped with frames deselects children of those frames.
- **Shift+click on an already-selected element deselects it**. This happens on pointer-up and only if no drag occurred. If it was selected through a group, the whole group and its members are deselected.
- **Groups** (`packages/element/src/groups.ts`):
  - `element.groupIds` is ordered **innermost first, outermost last**. `addToGroup` appends the new (outer) group at the end, or inserts it just before `editingGroupId`. In `actionDeleteSelected` the supergroup of `groupIds[i]` is `groupIds[i+1]`.
  - `selectGroupsForSelectedElements` (`_selectGroups`): for every selected element, take `groupIds`, cut it at `editingGroupId` if present (`groupIds.slice(0, indexOf(editingGroupId))`), and select the **last** id of what remains, i.e. the outermost group not being edited. Then every element whose `groupIds` contains a selected group gets selected. A "group" with fewer than 2 members is never marked selected.
  - **Double-click an element of a selected group** sets `editingGroupId` to that group and selects just the clicked element (`handleCanvasDoubleClick`; `getSelectedGroupIdForElement` returns the first of the element's groupIds that is selected). Repeated double-clicks drill one level deeper each time.
  - **Ctrl/Cmd+click = deep select**: `editGroupForSelectedElement` sets `editingGroupId = groupIds[0]` (innermost) and selects only that element, ignoring groups.
  - Clicking an element outside the `editingGroupId` group exits group editing (`editingGroupId: null`, selection cleared).
- **Ctrl/Cmd+Alt+drag** starts a **lasso** from the selection tool (`setActiveTool({type: "lasso", fromSelection: true})`).
- **Double-click on empty canvas or on a shape** (selection tool) creates or edits text:
  - A single selected text-bindable container (rectangle, diamond, ellipse, arrow, …) gets its label edited wherever the double-click lands.
  - Otherwise the container under the pointer is used. A **transparent** container only binds if it already has text or its stroke was hit.
  - Ctrl/Cmd skips container binding, giving free text. Alt means "don't insert at parent center".
  - The text is centered in the container (`getContainerCenter`).
- **Double-click an image** starts cropping. Enter on a selected image also starts cropping; Enter/Escape finishes.
- **Enter** on a selected text or text container starts editing it. Enter on a frame starts editing its name (`editingFrame`).

#### Box selection (`element/src/selection.ts` `getElementsWithinSelection` → `element/src/bounds.ts` `elementsOverlappingBBox`)

- Mode is `appState.boxSelectionMode`, `"contain"` (default) or `"overlap"`, a user preference in the main menu (`components/main-menu/DefaultItems.tsx`). There is no drag-direction rule.
- **Ignored:** `element.locked || isBoundToContainer(element)` (`shouldIgnoreElementFromSelection`).
- Per element:
  - The AABB is grown by `strokeWidth/2`. An arrow's label AABB is unioned in.
  - If the element is inside a frame it overlaps, its AABB is **clipped to the frame**.
  - Rules:
    1. If the box fully contains the (element ∪ label) AABB, the element is selected in either mode.
    2. `overlap` only: the element is selected if the box intersects the label AABB.
    3. `overlap` only: if the box intersects the element AABB, test real geometry. For linear/freedraw, is any vertex inside the box. For other shapes, is any of the 4 rotated edge midpoints inside. Otherwise, does any box edge intersect the element outline (tolerance `strokeWidth/2`).
- Frames: when a frame is selected, its children are dropped from the result (`excludeElementsInFrames` defaults to true).
- Groups (top-level only, `groupIds.at(-1)`):
  - In `overlap`, hitting one member selects the whole outermost group.
  - In `contain`, a member is dropped unless **every** selectable member of its outermost group is inside.
- The result keeps scene order.
- In the app, the dragged `selection` element is sized via `dragNewElement` with Shift = square.
- **Lasso** (`packages/excalidraw/lasso/`): the same `boxSelectionMode` is used. `contain` requires every outline segment enclosed and `overlap` requires some (`enclosureTest`: `mode === "contain" ? "every" : "some"`). The path is simplified with `simplifyDistance: 5 / zoom`.

---

### 3b.5 Transform: resize, rotate, flip

#### Handles (`element/src/transformHandles.ts`, `element/src/resizeTest.ts`)

- **Only 4 corner handles plus a rotation handle are drawn.** `DEFAULT_OMIT_SIDES = { e: true, s: true, n: true, w: true }` is the default. Side resizing happens by **grabbing the selection border** instead: `resizeTest` checks whether the pointer is within `SIDE_RESIZING_THRESHOLD / zoom` (4 screen px) of the rotated bbox edges, which are pushed out by the same spacing (0 for images). Edge grabbing is disabled for linear elements with ≤2 points and gated by `canResizeFromSides(editorInterface)`, presumably desktop only (UNVERIFIED exact rule).
- Handle geometry (`getTransformHandlesFromCoords`):
  - `size = 8/zoom` (mouse), `margin = 4` for the dashed box (non-linear default).
  - Corners sit outside the bbox by `margin/zoom + size/zoom - centeringOffset`, with `centeringOffset = (size - 2*spacing) / (2*zoom)`.
  - The rotation handle is centered horizontally, above the NW handle row by `ROTATION_RESIZE_HANDLE_GAP / zoom` (16).
  - Side handles (when not omitted) appear only if the bbox side is longer than `5 * 8 / zoom`.
  - All handle positions are rotated about the element center.
  - Margin: linear elements use `DEFAULT_TRANSFORM_HANDLE_SPACING + 8` (10). Images use 0 margin and 0 spacing. Others use 2.
- Per element type (`getTransformHandles`):
  - Locked or elbow arrow: no handles.
  - Frames: no rotation handle.
  - A 2-point line/arrow/freedraw omits sides by diagonal direction. `OMIT_SIDES_FOR_LINE_SLASH` adds `nw, se`; the backslash variant as fetched is `{e,s,n,w}` only.
  - A single selected **2-point** linear element does not get resize handles in `handleSelectionOnPointerDown`. Its endpoints are dragged through the linear editor. Resizing via `getResizeArrowDirection` still exists for the transform path.
  - **Multi-selection** uses one common bbox (`getTransformHandleTypeFromCoords`, angle 0) with the same corners, rotation handle and edge band.

#### Resize (`element/src/resizeElements.ts`)

- Dispatch (`transformElements`):
  - With one element, the `"rotation"` handle calls `rotateSingleElement` (skipped for elbow arrows) and then `updateBoundElements`. Other handles call `getNextSingleWidthAndHeightFromPointer` then `resizeSingleElement`. Text also triggers `updateBoundElements`.
  - With more than one element, the rotation handle calls `rotateMultipleElements` and other handles call `resizeMultipleElements`.
- Modifiers, set in App.tsx before calling:
  - Shift toggles aspect ratio. It is **inverted** (proportional by default, Shift frees) when any image is selected, or for a single sticky note's corner handle.
  - Alt resizes from center.
  - Shift snaps rotation.
- Object snapping applies to resizing too (`snapResizingElements`). Grid snapping of the pointer applies unless CTRL_OR_CMD is held.
- Single element:
  - Aspect: `widthRatio = |nextW| / orig.width`, `heightRatio = |nextH| / orig.height`, then the larger ratio is used (per handle).
  - From center: `nextWidth = 2*nextWidth - orig.width` (same for height).
  - **Flip by dragging past the opposite edge:** negative width/height is allowed. `if (nextWidth < 0) newOrigin.x += nextWidth` (same for y). Linear/freedraw points are mirrored accordingly.
- **Text** (`resizeSingleTextElement`):
  - Any handle containing `n` or `s` (**all corners** and top/bottom edges) **scales the font**. `metricsWidth = width * (nextHeight/height)`, `fontSize` comes from `measureFontSizeFromWidth`, and width/height are set proportionally. Aspect is always kept.
  - `e`/`w` edges **re-wrap**: `newWidth = max(minWidth, nextWidth)`, the text is re-wrapped with `wrapText(originalText, font, newWidth)`, height is re-measured, and `autoResize: false` is set.
- **Multi-element** (`resizeMultipleElements`):
  - `scaleX = |nextW|/w` for e/w handles, otherwise 1. `scaleY` works likewise for n/s.
  - Single-direction handles use that axis's scale. Corners use `max(|nextW|/w, |nextH|/h)`.
  - **Aspect ratio is forced** if Shift is held or any target has `angle !== 0`, is text, or is in a group.
  - Flip factors are −1 when dragged past the opposite side. Text font sizes scale with the group.

#### Rotation

- Single element: `angle = 5π/2 + atan2(py - cy, px - cx)`, with 0 meaning the handle straight up. With Shift: `angle += SHIFT_LOCKING_ANGLE/2; angle -= angle % SHIFT_LOCKING_ANGLE`, i.e. round to the nearest 15°.
- Multiple elements: they rotate about the common bbox center (`centerX, centerY` from pointer-down). Each element's own center orbits and its angle is incremented. Shift snaps the delta angle the same way.
- Bound text rotates with its container. Arrows bound to rotated shapes update via `updateBoundElements`.

#### Flip

`actionFlip.ts` (Shift+H / Shift+V per the keyboard section) mirrors the selection about its bbox center. Details are in the actions section; this fork did not deep-dive it (UNVERIFIED specifics).

---

### 3b.6 Moving / nudging / duplicating

- **Drag** (`element/src/dragElements.ts` `dragSelectedElements`):
  - The offset is `pointer − origin`, plus the object-snap offset. On any axis with no object snap, the **top-left of the common bounds is snapped to the grid** (`calculateOffset` → `getGridPoint(x + dx, y + dy, gridSize)`).
  - If any frame is selected, **all its children are moved too**.
  - Each moved non-arrow element also moves its bound text and calls `updateBoundElements`, so bound arrows re-route.
  - Arrows:
    - A selected arrow moves if more than one element is being dragged, or the offset is greater than `DRAGGING_THRESHOLD` (10) on some axis, or it has no bindings.
    - This avoids accidental unbinding when the user only means to select the arrow.
    - When it moves, **each end whose bound element is not also being dragged is unbound** (`unbindBindingElement`).
- **Arrow-key nudge** (App.tsx keydown):
  - Step without grid: 1, or 5 with Shift. With grid on: `gridSize`, or 1 with Shift.
  - Selected arrows with an end bound to an element **outside** the selection are removed from the nudge set, so they stay attached and are re-routed instead.
  - `updateBoundElements` runs for each moved element.
- **Alt+drag duplicates**: on the first pointer-move with Alt held (`!hit.hasBeenDuplicated`), `duplicate.duplicateDraggedSelection` clones the selection and the drag continues on the clones (originals stay put; UNVERIFIED which copy remains selected: the dragged one).
- **Ctrl/Cmd+D** (`actions/actionDuplicateSelection.tsx`):
  - keyTest `event[CTRL_OR_CMD] && event.key === "d"`.
  - Duplicates are offset by `DEFAULT_GRID_SIZE / 2` = **+10, +10**.
  - Bound text and frame children are included. `frameId` is kept, or points to the duplicated frame.
  - Groups get fresh ids (`getNewGroupIdsForDuplication` maps ids up to `editingGroupId`).
  - In linear edit mode it duplicates the selected points instead.
- Library drop / paste goes through `duplicateElements({type: "everything", randomizeSeed: true, ...})`, which gives new ids and seeds.

---

### 3b.7 Binding (arrows ↔ shapes) (`packages/element/src/binding.ts`, `collision.ts`)

**Feature flag:** `getFeatureFlag("COMPLEX_BINDINGS")` defaults to **false** (`packages/common/src/utils.ts`, stored in localStorage `excalidraw-feature-flags`). Port the `_simple` code paths.

- **Which elements:** arrows (not lines) bind to bindable elements: rectangle, diamond, ellipse, text, image, frame, embeddable, sticky note (per `isBindableElement`; UNVERIFIED exact list on master). Locked elements cannot be bound to but still occlude.
- **Enabling:**
  - `appState.isBindingEnabled` is the effective state. `bindingPreference: "enabled" | "disabled"` is the user setting (`actionToggleArrowBinding`, a menu toggle with no key).
  - Holding **Ctrl/Cmd** while drawing or dragging an endpoint temporarily inverts it (`isBindingEnabled: bindingPreference !== "enabled"`).
  - With binding disabled, dragging an endpoint **breaks** that end's binding.
- **Candidate search** (`getBindingCandidates`):
  - `maxDistance = maxBindingDistance_simple(zoom)`:

    ```ts
    const BASE_BINDING_DISTANCE = Math.max(BASE_BINDING_GAP, 15);          // 15
    const zoomValue = zoom?.value && zoom.value < 1 ? zoom.value : 1;
    return clamp(BASE_BINDING_DISTANCE / (zoomValue * 1.5), 15, 30);       // scene units
    ```

    So the distance is 15 scene units at zoom ≥ 1 and grows up to 30 when zoomed out below 1/2.
  - The search runs front-to-back. An element is a candidate if its signed border distance is `> -maxDistance`, meaning within 15 outside or inside near the border (UNVERIFIED sign convention of `bindableElementBorderDistanceIfClose`).
  - The search stops at the first **opaque** element (image, or has-background and non-transparent) that contains the point. An opaque frame containing the point hides only non-children.
- `getHoveredElementForBinding` picks the candidate with the smallest |distance|. A smaller element overlapping it wins only if the point is inside that smaller element.
- **Gap:** `getBindingGap(el) = BASE_BINDING_GAP + el.strokeWidth / 2` (5 + sw/2).
- **Bind modes** (`FixedPointBinding.mode`):
  - `"orbit"`: the endpoint sits on the outline (offset by the gap), aimed at the fixed point.
  - `"inside"`: the endpoint sits exactly at the fixed point inside the shape.
  - `appState.bindMode` is `"orbit"` (default), `"inside"` or `"skip"`.
- **Strategy while dragging an endpoint** (`getBindingStrategyForDraggingBindingElementEndpoints_simple`):
  - Both ends dragged (whole arrow): no binding, and existing bindings break.
  - Elbow arrows use `bindingStrategyForElbowArrowEndpointDragging`.
  - If the other end is already bound to the hovered element, both ends become `"inside"` bindings of that element.
  - **Alt held:** bind `"inside"` to whatever is hovered, at the exact pointer point.
  - Otherwise, if the point is inside the hovered shape the binding is `"inside"`. If it is only near the outline, it is `"orbit"`, with the focus point projected by `projectFixedPointOntoDiagonal` (midpoint snapping when `isMidpointSnappingEnabled`, not angle-locked and grid off; grid mode snaps the bound point to the grid).
  - No hovered element: `mode: null` (unbind).
- **Delayed "inside" mode:** keeping a dragged endpoint over a bindable element for `BIND_MODE_TIMEOUT` (700 ms) switches `bindMode` to `"inside"` for the rest of the gesture (App.tsx `handleDelayedBindModeChange`). Not for elbow arrows, not when `bindMode === "skip"`.
- **Recomputing on shape move/resize** (`updateBoundElements(changedElement, scene, {simultaneouslyUpdated})`):
  - For each arrow in `changedElement.boundElements`, skipped if the arrow itself is being moved at the same time: new endpoint = `updateBoundPoint(...)`, then `LinearElementEditor.movePoints`, then the arrow label is re-laid out.
  - `updateBoundPoint`:
    - `"inside"`: the endpoint is the global fixed point (`fixedPoint` ratio × element box).
    - `"orbit"`: build a segment from the fixed point toward the other end (the other binding's focus point for 2-point arrows, otherwise the adjacent point). Intersect it with the element outline offset by the gap and take the intersection nearest the other end. If the arrow is too short (≤ `BASE_ARROW_MIN_LENGTH` 10) or would invert into the other shape (area heuristic ×2), fall back to the focus point.
    - Ends without an arrowhead, when the other end has one, use the focus point.
  - For multi-point arrows (>2 points) only the end bound to the changed element is updated.
- **Delete:** deleting a shape nulls the start/end binding of bound **elbow** arrows (`actionDeleteSelected`). For simple arrows, binding cleanup on delete is UNVERIFIED, likely in scene/restore logic.
- Bound text follows its container on move (`dragSelectedElements` moves `getBoundTextElement`) and on resize (`handleBindTextResize`).

---

### 3b.8 Snapping

- **Object snapping** (`packages/excalidraw/snapping.ts`):
  - Threshold is `SNAP_DISTANCE / zoom` = 8 screen px.
  - Enabled when (`objectsSnapModeEnabled` && !CTRL_OR_CMD) or (!`objectsSnapModeEnabled` && CTRL_OR_CMD && !gridMode). So Ctrl/Cmd **inverts** the setting while held, and it cannot be enabled that way while grid mode is on.
  - A lone selected arrow is never snapped. Lasso only snaps while dragging.
  - Default `objectsSnapModeEnabled: false`, toggled by `actionToggleObjectsSnapMode` (key in the keyboard section).
- **Snap points** (`getElementsCorners`):
  - Rectangles etc: 4 rotated corners plus center.
  - Diamond/ellipse: 4 edge midpoints plus center.
  - Multi-selection: the common bbox corners plus center.
- **Snap kinds:**
  - point-to-point alignment (x and y independently)
  - **gap** snapping: equal spacing, center-in-gap and side gaps (`VISIBLE_GAPS_LIMIT_PER_AXIS = 99999`)
  - resize snapping (`snapResizingElements`)
  - new-element snapping (`snapNewElement`)
  - linear point snapping
- Reference points are cached per gesture (`SnapCache`, `maybeCacheReferenceSnapPoints`) and include only visible, non-selected elements (`getVisibleAndNonSelectedElements`). `appState.snapLines` are drawn.
- **Grid snapping:**
  - `getGridPoint` rounds to the nearest `gridSize` (20).
  - Active only when `gridModeEnabled`. The toggle is `actionToggleGridMode`; the `gridStep` 5 major-line interval is a rendering matter.
  - Applied to new element origin and size, drag offset (via the common-bounds top-left), resize pointer, linear points and arrow-key steps.
  - Holding CTRL_OR_CMD suppresses it per gesture.
  - Text creation uses its own floor-based variant (`getTextCreationGridPoint`).

---

### 3b.9 Frames (`packages/element/src/frame.ts`)

- **Creating a frame** (drag with the frame tool): on pointer-up, `getElementsInNewFrame` = `omitPartialGroups(omitGroupsContainingFrameLikes(getElementsCompletelyInFrame(...)))`.
  - `getElementsCompletelyInFrame` runs the **contain** box selection against the frame (no frame exclusion) and keeps only non-frame elements with no `frameId`, or already in this frame.
  - Groups only partly inside are omitted, and groups that contain frames are omitted.
  - `addElementsToFrame` then sets `frameId` (bound text included) and **reorders** the children to sit just below the frame in z-order. Frames cannot be nested.
- **Drawing a new element** inside a frame: `frameId` is the topmost unlocked frame under the pointer-down point.
- **Dragging elements into or out of frames** (App pointer-up after a drag):
  - The target is `topLayerFrame` under the pointer (excluding selected elements).
  - If there is a target frame and it is not itself selected, the selected elements satisfying `isElementInFrame` (overlap test, group-aware) are added. When `editingGroupId` is set, group ids are adjusted first.
  - If there is no frame under the pointer, elements that are no longer `isElementInFrame` are removed from their frame (`frameId: null`).
  - Linear elements edited in the point editor are removed from their frame (and their `groupIds` cleared) if they no longer overlap it.
- **Overlap test** (`elementOverlapsWithFrame`): the element is inside the frame bounds, OR intersects the frame outline, OR contains the frame.
  - `isElementInFrame` is group-aware: an element outside the frame whose group has members in the frame stays a member and is clipped (`shouldApplyFrameClip`).
- **Resizing a frame** (`getElementsInResizingFrame`), highlighted live:
  - Kept: previous children still fully inside or containing the frame.
  - Previous ungrouped children that no longer intersect are removed. Grouped ones are kept if any member of their group is still completely inside or intersecting.
  - Added: new ungrouped elements now completely inside, and whole groups whose common bounds are inside the frame.
  - Bound text is excluded from the list, because it follows its container.
- **Moving a frame** moves all its children (`dragSelectedElements`).
- **Deleting a frame** (`actionDeleteSelected`) **does not delete its children**. They are released (`frameId: null`) and become selected. Children only go too if they were themselves selected (UNVERIFIED: in practice selecting a frame never co-selects children, so they survive).
- **Selection:** a frame and its children are never co-selected (3b.4). Box-selecting a frame drops its children. Clicking inside a frame's empty area does not select it (frames are hit on the outline or name label; `isFrameLikeElement` elements are skipped for container hit-testing).
- **Name:**
  - `frame.name`, defaulting to `"Frame"` (`"AI Frame"` for magicframe) via `getFrameLikeTitle`.
  - Edited via Enter on a selected frame (`editingFrame`); double-click on the label is UNVERIFIED but likely.
  - The inline input uses `fontFamily: "Assistant"`, `fontSize: FRAME_STYLE.nameFontSize` (14), padding 6, color `#999999` light / `#7a7a7a` dark. Escape/Enter ends editing.
- Frame style: `FRAME_STYLE = {strokeColor "#bbb", strokeWidth 2, strokeStyle "solid", fillStyle "solid", roughness 0, roundness null, backgroundColor "transparent", radius 8, nameOffsetY 3, nameColorLightTheme "#999999", nameColorDarkTheme "#7a7a7a", nameFontSize 14, nameLineHeight 1.25}`.
- Frames have no rotation handle.

---

### 3b.10 Eraser, hand, laser, lock, links

- **Eraser** (`packages/excalidraw/eraser/index.ts`, an AnimatedTrail):
  - Trail options `size: 5, streamline: 0.2, keepHead: true`, decay `DECAY_TIME = 200`, `DECAY_LENGTH = 10`.
  - Each trail segment is tested against elements (`eraserTest`): a fast bounds check, then the real outline. The threshold is `strokeWidth / 2`, or `15` for freedraw, with freedraw tolerance `max(2.25, 5/zoom)` against outline segments.
  - A hit marks the **whole outermost group** (`groupIds.at(-1)`), plus bound text, or the container when bound text is hit.
  - Locked elements are skipped.
  - Marked elements render at `ELEMENT_READY_TO_ERASE_OPACITY` (20%) and are deleted on pointer-up (one undo step; UNVERIFIED capture detail).
  - Holding **Alt** while erasing un-marks elements (`restore`).
  - The pen's eraser button (`POINTER_BUTTON.ERASER = 5`) activates the eraser.
  - UNVERIFIED: frame-specific eraser rules.
- **Hand tool / panning:**
  - Space held with no active pointers sets `pan.setSpaceHeld(true)` and cursor `grab`. Space-drag pans.
  - The hand tool drags to pan. Wheel pans and Ctrl/Cmd+wheel zooms (viewport section).
- **Laser:** a transient trail only (`laserTrails.ts`). Not ported.
- **Tool lock:**
  - `activeTool.locked`. When locked, the tool stays active after creating an element and the new element is **not** selected. Linear elements are still finalized.
  - Freedraw never auto-reverts; bucketfill never reverts.
- **Element lock** (`element.locked`, `actionElementLock.ts`): locked elements get no transform handles, are skipped by box selection, erasing and binding, and cannot be clicked into selection.
- **Links** (`components/hyperlink/`, `element/src/elementLink.ts`):
  - `element.link` is any URL. An **element link** is the current page URL with `?element=<id>` (`ELEMENT_LINK_KEY = "element"`; `defaultGetElementLinkFromSelection`). It targets one element (id) or a group (the selected group id, or `selectedElements[0].groupIds[0]`) and requires 1 element or elements sharing a group (`canCreateLinkFromElements`).
  - `isElementLink(url)` checks that the host equals the current host and the query has `element`.
  - Link icon hit box (`getLinkHandleFromCoords`): `size = 12 / max(zoom, 1)`, placed at the element's top-right outside the bbox (`x = x2 + 4/z − (12−8)/(2z)`, `y = y1 − 4/z − 12/z + (12−8)/(2z)`, rotated with the element), hit tolerance `4/zoom`.
  - The icon is drawn with `DEFAULT_LINK_SIZE = 14` in `renderElement.ts`.
  - Clicking the icon of a **non-selected** element opens the link. In view mode, a click anywhere on the element opens it.
  - External links open in `_blank`, local ones in `_self`, via the `onLinkOpen` callback.
  - The hover tooltip appears after 300 ms and auto-hides after 500 ms once the pointer is more than `15/zoom` away.

---

### 3b.11 Tool state machine summary (for the port)

| Tool | Press | Drag | Release | After |
|---|---|---|---|---|
| rectangle/diamond/ellipse | create 0×0 at grid-snapped point, frameId = frame under point | size; Shift=square; Alt=from center; grid+object snap | discard if 0×0; normalize | revert to selection and select new, unless locked |
| line/arrow | create 2-point `[[0,0],[0,0]]` | move last point; Shift=15° lock; binding preview for arrows | drag < 20 screen px → click-click mode; else finalize | same revert; creates LinearElementEditor (not editing) |
| line/arrow click-click | each click commits a point | pointer moves floating point | — | finish: click last point again (≤8 px), Enter/Esc, close loop (line), or bind outside an element (arrow); elbow: 2nd click finishes |
| freedraw | start points/pressures | append points | append final point; finalize | tool stays freedraw |
| text | click: new text at point (or into container) | drag sets width; >36 px turns autoResize off | open editor | reverts after editing |
| frame | create with FRAME_STYLE | size (Shift=square, Alt=center) | capture fully-contained elements | revert unless locked |
| selection | hit test → select / box / resize / rotate / link | move, box, transform, Alt=duplicate | Shift-click toggle | — |
| eraser | start trail | mark hits (Alt restores) | delete marked | — |
| hand | — | pan | — | — |

---

### 3b.12 Surprises found while researching (flag to other sections)

- master has **new element/tool types**: `stickynote` (`element/src/stickyNote.ts`, default 250×250, min 75, min font 16, proportional corner resize by default), `autoshape` (double-click-to-type tool), `bucketfill` (paint bucket, `element/src/bucketFill.ts`), plus `convertToShape.ts`, `arrowEndpointText.ts`, `heading.ts`. Decide whether to preserve these on load, like embeddables.
- Side handles are gone; edges are grabbed invisibly (4 px band).
- The binding rewrite: `FixedPointBinding.mode` `"orbit" | "inside"`, the 700 ms hover-to-inside behaviour, Alt forcing inside, and the `COMPLEX_BINDINGS` flag being off by default.
- Box-selection mode is a user preference (`contain` default), not drag-direction based.
- `groupIds` order is innermost→outermost, which is easy to get backwards.
- Freedraw stays active after each stroke. Plain clicks with shape tools create nothing.
- Deleting a frame releases its children rather than deleting them.
- A single bound arrow needs >10 px of drag before it moves and unbinds.

---

# 4. Suggested implementation order for the native port

This order is written for the current spike's architecture: an Elisp document model plus a Cairo/Pango module rasterising into a Canvas framebuffer. Each step lists what it depends on (**Deps**) and what "done" means (**Done when**).

| # | Milestone | Deps | Done when | Spec refs |
|---|-----------|------|-----------|-----------|
| 0 | **Lossless document model.** Keep each element's original JSON (alist/hash) and edit fields in place. Unknown types, including embeddable/iframe/magicframe, and unknown fields must round-trip untouched. | – | Loading and saving any upstream `.excalidraw` is byte-for-byte equivalent after JSON normalisation. | 1a.2–1a.3, 1b.2, 1b.11, §0.14 |
| 1 | **Restore on load**: `restoreElementWithProperties` defaults for missing fields, legacy `strokeSharpness`→`roundness`, fontFamily fallback, lineHeight from `FONT_METADATA`, arrowhead renames, `repairBinding` (legacy focus/gap → FixedPoint), and `restoreAppState` (5 saved keys). | 0 | Old v1 files and legacy-binding files open and resave in the current format. | 1b.3, 1b.1, 2b.2 |
| 2 | **Deterministic PRNG and roughjs port**: seeded `Random` (roughjs uses a Park-Miller style LCG seeded from `element.seed`), `generateRoughOptions`, line/ellipse/curve/polygon generators, hachure/cross-hatch/zigzag/solid fills. Match roughjs 4.6.4 exactly; pixel parity depends on consuming random numbers in the same order. | 0 | A test scene renders visually identical to excalidraw.com, compared by screenshot diff. | 2a.1–2a.2 |
| 3 | **Shapes**: rectangle/diamond/ellipse including roundness (`getCornerRadius`) and rounded diamond paths, then line/arrow straight and curved (roundness → curve), then `polygon` lines, then opacity. | 2 | All non-text shapes render. | 2a.3–2a.4, 2a.9 |
| 4 | **Arrowheads**, all 14 values. Direction comes from the last segment of the generated (jittered) curve, so approximate it with the tangent of the rough path. Outline heads are filled with the background colour. | 3 | Arrowhead test sheet matches. | 2a.5 |
| 5 | **Text**: `FONT_FAMILY`, bundled font files (Excalifont, Nunito, Lilita One, Comic Shanns, Liberation Sans, Cascadia, Xiaolai fallback), `FONT_METADATA` line heights, `getVerticalOffset` baseline, the tokenizer-based `wrapText` (do not rely on Pango wrapping), and measuring with the same font metrics. | 0 | Multi-line text lines up with web output. | 2b.1–2b.9 |
| 6 | **Bound text in containers and on arrows**: `BOUND_TEXT_PADDING`, max-width formulas (ellipse √2, diamond ½, arrow label), container auto-grow, vertical alignment, label `labelPosition`, and the arrow "hole" behind labels. | 3, 5 | Labels in all container types match. | 2b.7–2b.8, 2a.10 |
| 7 | **Freedraw**: port perfect-freehand `getStroke` (variable width) and the constant-width path (`strokeOptions.variability`), then fill the outline. | 2 | Pen strokes match. | 2a.6 |
| 8 | **Dark mode and canvas**: `applyDarkModeFilter` per colour (cache it), background, grid (major/minor), render order, frame clipping. | 3 | Theme toggle matches. | 2a.13, 2c.1–2c.4 |
| 9 | **Images and the files map**: decode `dataURL` (PNG/JPEG/SVG/GIF/WebP), `scale` flip, `crop`, placeholder for missing/pending/error, keeping the files map untouched on save. | 0, 3 | Images from upstream files render and survive round-trip. | 1a.3 (image), 1b.2, 2a.7 |
| 10 | **Frames (render)**: border style, name label, clipping children, export label as Helvetica 14px text. | 3, 5, 8 | Frames render. | 2a.8, 2c.4 |
| 11 | **Selection and hit testing**: `collision.ts` hit thresholds, click/shift-click, groups (outermost first, double-click enters), box selection (`boxSelectionMode`), selection borders and handle rendering. | 3 | Select anything, including groups and bound text. | 3b.3–3b.4, 2c.6–2c.8 |
| 12 | **History** before the more complex edits: the store/delta model (element snapshots plus the 9 tracked appState keys), and grouping by `CaptureUpdateAction`. Every later milestone emits history entries. | 0, 11 | Undo/redo for create/move/delete/style. | 3.A.8 |
| 13 | **Creation tools and style panel**: tool keys, drag-to-create with shift/alt, click-click lines, `currentItem*` defaults, the properties panel applying per type (`comparisons.ts`). | 11, 12 | All in-scope tools create correct JSON (compare against an upstream-saved file). | 3.A.1–3.A.6, 3b.1, 1a.5 |
| 14 | **Transform**: move/nudge, resize per handle and edge band (shift/alt), text resize semantics, rotation (shift-snap), multi-element resize/rotate, flip. | 11, 12 | Transforms match upstream geometry. | 3b.5–3b.6, 2b.11 |
| 15 | **Linear point editor**: enter/exit, add/move/delete points, midpoints, polygon closing. | 11, 14 | Line editing parity. | 3b.2, 2c.9 |
| 16 | **Binding (simple path)**: bind on create or drag within the bind distance, `fixedPoint` computation, orbit vs inside, updating bound arrows when shapes move or resize, unbinding, bound text following its container, highlight rendering. | 14, 15 | Moving a shape drags connected arrows correctly. | 3b.7, 2c.10 |
| 17 | **Elbow arrows**: A* routing on the dynamic grid, headings, `fixedSegments`. The largest single algorithm; port `elbowArrow.ts` nearly verbatim. | 16 | Elbow arrows reroute like upstream. | 2a.12 |
| 18 | **Frames (behavior)**: membership on create or drag-in, moving the frame moves its children, deleting releases children, name editing. | 10, 14 | Frame workflows. | 3b.9 |
| 19 | **Clipboard**: `excalidraw/clipboard` JSON through the Emacs kill ring / system clipboard, paste with new ids/seeds, remapped group/bind/frame ids, and positioning. Duplicate (Ctrl-D, Alt-drag) reuses the same id-remapping code. | 13, 16 | Copy/paste between excali and excalidraw.com works both ways. | 1b.9, 3b.6 |
| 20 | **Fractional index**: port `packages/fractional-indexing` and call the equivalent of `syncMovedIndices` on insert/reorder/paste. Array order stays authoritative for z-order. Needed for files to stay valid in upstream (collab merge relies on it). | 0 | z-order actions keep indices valid. | 1b.10 |
| 21 | **Export**: PNG (Cairo surface, padding 10, scale, background, dark mode, tEXt embed), SVG (Cairo SVG surface or a hand-written emitter, with the metadata payload), `.excalidraw` save. Reading embedded scenes back from PNG/SVG is an import path. | 3–10 | Exported PNG/SVG re-import into excalidraw.com with the scene. | 1b.6–1b.8 |
| 22 | **Library**: `.excalidrawlib` v1/v2 load/merge/save, and inserting an item (new ids, placement at the cursor or on a grid). | 19 | Library round-trip with upstream. | 1b.5 |
| 23 | **Snapping**: grid snapping first (simple), then object snapping (points and gaps, `SNAP_DISTANCE`), then snap-line rendering. | 14 | Snap parity. | 3b.8, 2c.11 |
| 24 | **Remaining tools**: eraser, hand, lock, element links, text auto-resize handle, sticky notes (full), autoshape, bucket fill, laser (optional). | as needed | – | 3b.10, 2a.11, 1a.7 |

**Dependency highlights**

- The roughjs port (step 2) is the long pole for visual fidelity. Everything visible depends on it, and exact PRNG call order matters.
- History (step 12) should land before tools (step 13) so that every mutation goes through one "commit" function from the start.
- Binding (step 16) needs bounds and collision (step 11) plus transforms (step 14). Elbow arrows (step 17) need binding.
- Clipboard, duplicate, library insert and paste all share one "clone elements with remapped ids" routine (upstream `duplicateElements` / `App.duplicate.ts`). Write it once.
- Export reuses the renderer. Keep the renderer independent of the view: it should take a scene, a transform and a theme.
- Text measurement must be available to Elisp (for wrap, container growth and bound text) as well as to the renderer. Expose a `measure-text` module function early (step 5).

**Test strategy**

- Save reference `.excalidraw` files from excalidraw.com: one per element type/style, legacy-binding files, and v1 files. Assert that restore → save equals upstream's own re-save, ignoring `version`, `versionNonce` and `updated`.
- Screenshot-diff renders against upstream PNG exports at scale 1 with the same seeds.
