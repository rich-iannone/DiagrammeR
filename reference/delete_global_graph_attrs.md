# Delete one of the global graph attributes stored within a graph object

Delete one of the global attributes stored within a graph object of
class `dgr_graph`).

## Usage

``` r
delete_global_graph_attrs(graph, attr = NULL, attr_type = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- attr:

  The name of the attribute to delete for the `type` of global attribute
  specified.

- attr_type:

  The specific type of global graph attribute to delete. The type is
  specified with `graph`, `node`, or `edge`.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a new graph and add
# some extra global graph attrs
graph <-
  create_graph() |>
  add_global_graph_attrs(
    attr = "overlap",
    value = "true",
    attr_type = "graph") |>
  add_global_graph_attrs(
    attr = "penwidth",
    value = 3,
    attr_type = "node") |>
  add_global_graph_attrs(
    attr = "penwidth",
    value = 3,
    attr_type = "edge")

# Inspect the graph's global
# attributes
graph |>
  get_global_graph_attr_info()
#> # A tibble: 20 × 3
#>    attr        value      attr_type
#>    <chr>       <chr>      <chr>    
#>  1 layout      neato      graph    
#>  2 outputorder edgesfirst graph    
#>  3 bgcolor     white      graph    
#>  4 fontname    Helvetica  node     
#>  5 fontsize    10         node     
#>  6 shape       circle     node     
#>  7 fixedsize   true       node     
#>  8 width       0.5        node     
#>  9 style       filled     node     
#> 10 fillcolor   aliceblue  node     
#> 11 color       gray70     node     
#> 12 fontcolor   gray50     node     
#> 13 fontname    Helvetica  edge     
#> 14 fontsize    8          edge     
#> 15 len         1.5        edge     
#> 16 color       gray80     edge     
#> 17 arrowsize   0.5        edge     
#> 18 overlap     true       graph    
#> 19 penwidth    3          node     
#> 20 penwidth    3          edge     

# Delete the `penwidth` attribute
# for the graph's nodes using the
# `delete_global_graph_attrs()` fcn
graph <-
  graph |>
  delete_global_graph_attrs(
    attr = "penwidth",
    attr_type = "node")

# View the remaining set of global
# attributes for the graph
graph |>
  get_global_graph_attr_info()
#> # A tibble: 19 × 3
#>    attr        value      attr_type
#>    <chr>       <chr>      <chr>    
#>  1 layout      neato      graph    
#>  2 outputorder edgesfirst graph    
#>  3 bgcolor     white      graph    
#>  4 fontname    Helvetica  node     
#>  5 fontsize    10         node     
#>  6 shape       circle     node     
#>  7 fixedsize   true       node     
#>  8 width       0.5        node     
#>  9 style       filled     node     
#> 10 fillcolor   aliceblue  node     
#> 11 color       gray70     node     
#> 12 fontcolor   gray50     node     
#> 13 fontname    Helvetica  edge     
#> 14 fontsize    8          edge     
#> 15 len         1.5        edge     
#> 16 color       gray80     edge     
#> 17 arrowsize   0.5        edge     
#> 18 overlap     true       graph    
#> 19 penwidth    3          edge     
```
