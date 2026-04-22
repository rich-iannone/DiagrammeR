# Get node attribute values

From a graph object of class `dgr_graph`, get node attribute values for
one or more nodes.

## Usage

``` r
get_node_attrs(graph, node_attr, nodes = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- node_attr:

  The name of the attribute for which to get values.

- nodes:

  An optional vector of node IDs for filtering list of nodes present in
  the graph or node data frame.

## Value

A named vector of node attribute values for the attribute given by
`node_attr` by node ID.

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph() |>
  add_gnm_graph(
    n = 4,
    m = 4,
    set_seed = 23) |>
  set_node_attrs(
    node_attr = value,
    values = c(2.5, 8.2, 4.2, 2.4))

# Get all of the values from
# the `value` node attribute
# as a named vector
graph |>
  get_node_attrs(
    node_attr = value)
#>   1   2   3   4 
#> 2.5 8.2 4.2 2.4 

# To only return node attribute
# values for specified nodes,
# use the `nodes` argument
graph |>
  get_node_attrs(
    node_attr = value,
    nodes = c(1, 3))
#>   1   3 
#> 2.5 4.2 
```
