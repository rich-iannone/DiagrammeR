# Add one or more global graph attributes

Add global attributes of a specific type (either `graph_attrs`,
`node_attrs`, or `edge_attrs` for a graph object of class `dgr_graph`).

## Usage

``` r
add_global_graph_attrs(graph, attr, value, attr_type)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- attr:

  The name of the attribute to set for the `type` of global attribute
  specified.

- value:

  The value to be set for the chosen attribute specified in the
  `attr_for_type` argument.

- attr_type:

  The specific type of global graph attribute to set. The type is
  specified with `graph`, `node`, or `edge`.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a new graph with no
# global graph attributes and
# add a global graph attribute
graph <-
  create_graph(
    attr_theme = NULL) |>
  add_global_graph_attrs(
    attr = "overlap",
    value = "true",
    attr_type = "graph")

# Verify that the attribute
# addition has been made
graph |>
  get_global_graph_attr_info()
#> # A tibble: 1 × 3
#>   attr    value attr_type
#>   <chr>   <chr> <chr>    
#> 1 overlap true  graph    

# Add another attribute with
# `add_global_graph_attrs()`
graph <-
  graph |>
  add_global_graph_attrs(
    attr = "penwidth",
    value = 12,
    attr_type = "node")

# Verify that the attribute
# addition has been made
graph |>
  get_global_graph_attr_info()
#> # A tibble: 2 × 3
#>   attr     value attr_type
#>   <chr>    <chr> <chr>    
#> 1 overlap  true  graph    
#> 2 penwidth 12    node     

# When adding an attribute where
# `attr` and `attr_type` already
# exists, the value provided will
# serve as an update
graph |>
  add_global_graph_attrs(
    attr = "penwidth",
    value = 15,
    attr_type = "node") |>
  get_global_graph_attr_info()
#> # A tibble: 2 × 3
#>   attr     value attr_type
#>   <chr>    <chr> <chr>    
#> 1 overlap  true  graph    
#> 2 penwidth 15    node     
```
