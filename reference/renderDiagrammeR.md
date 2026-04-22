# Widget render function for use in Shiny

Widget render function for use in Shiny

## Usage

``` r
renderDiagrammeR(expr, env = parent.frame(), quoted = FALSE)
```

## Arguments

- expr:

  An expression that generates a DiagrammeR graph

- env:

  The environment in which to evaluate expr.

- quoted:

  Is expr a quoted expression (with quote())? This is useful if you want
  to save an expression in a variable.
