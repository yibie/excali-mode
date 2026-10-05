# Excali DSL

The current language uses **`.excalidsl`**. See the
[language reference](excali-dsl.md) and [examples](../examples/excali-dsl/README.md).

The former `.edsl` / excalidraw-dsl grammar has been replaced. Rename alone is not
a conversion: rewrite nodes as `node id "Label"`, express containment with dotted
identifiers, add relative placements, and put appearance in trailing style blocks.
There is no legacy parser or automatic migration in this release.
