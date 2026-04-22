# Razor-like template for diagram specification

Use Razor-like syntax to define a template for use in a `grViz` diagram.

## Usage

``` r
replace_in_spec(spec, envir = parent.frame())
```

## Arguments

- spec:

  String spec to be parsed and evaluated.

- envir:

  The environment in which substitution functionality takes place.

## Examples

``` r
if (FALSE) { # \dontrun{
# a simple example to use a LETTER as a node label
spec <- "
  digraph { '@1' }

 [1]: LETTERS[1]
"
grViz(replace_in_spec(spec))


spec <- "
digraph a_nice_graph {
node [fontname = Helvetica]
a [label = '@1']
b [label = '@2-1']
c [label = '@2-2']
d [label = '@2-3']
e [label = '@2-4']
f [label = '@2-5']
g [label = '@2-6']
h [label = '@2-7']
i [label = '@2-8']
j [label = '@2-9']
a -> { b c d e f g h i j}
}

[1]: 'top'
[2]: 10:20
"
grViz(replace_in_spec(spec))
} # }
```
