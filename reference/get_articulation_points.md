# Get articulation points

Get the nodes in the graph that are identified as articulation points.

## Usage

``` r
get_articulation_points(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

a vector of node IDs.

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph() |>
  add_gnm_graph(
    n = 10,
    m = 12,
    set_seed = 23) |>
  set_node_attrs(
    node_attr = shape,
    values = "square")

# Get the articulation points
# in the graph (i.e., those
# nodes that if any were to be
# removed, the graph would
# become disconnected)
graph |>
  get_articulation_points()
#> [1] 2 4

# For the articulation points,
# change the node shape to
# a `circle`
graph <-
  graph |>
  select_nodes_by_id(
    nodes = get_articulation_points(graph)) |>
  set_node_attrs_ws(
    node_attr = shape,
    value = "circle")
```
