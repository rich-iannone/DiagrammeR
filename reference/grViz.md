# R + viz.js

Make diagrams in R using `viz.js` with infrastructure provided by
htmlwidgets.

## Usage

``` r
grViz(
  diagram = "",
  engine = "dot",
  allow_subst = TRUE,
  options = NULL,
  width = NULL,
  height = NULL,
  envir = parent.frame()
)
```

## Arguments

- diagram:

  Spec for a diagram as either text, filename string, or file
  connection.

- engine:

  String for the Graphviz layout engine; can be `dot` (default),
  `neato`, `circo`, or `twopi`.

- allow_subst:

  A boolean that enables/disables substitution functionality.

- options:

  Parameters supplied to the htmlwidgets framework.

- width:

  An optional parameter for specifying the width of the resulting
  graphic in pixels.

- height:

  An optional parameter for specifying the height of the resulting
  graphic in pixels.

- envir:

  The environment in which substitution functionality takes place.

## Value

An object of class `htmlwidget` that will intelligently print itself
into HTML in a variety of contexts including the R console, within R
Markdown documents, and within Shiny output bindings.
