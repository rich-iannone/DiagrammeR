# Widget render function for use in Shiny

Widget render function for use in Shiny

## Usage

``` r
renderGrViz(expr, env = parent.frame(), quoted = FALSE)
```

## Arguments

- expr:

  an expression that generates a DiagrammeR graph

- env:

  the environment in which to evaluate expr.

- quoted:

  is expr a quoted expression (with quote())? This is useful if you want
  to save an expression in a variable.

## See also

[`grVizOutput()`](https://rich-iannone.github.io/DiagrammeR/reference/grVizOutput.md)
for an example in Shiny.
