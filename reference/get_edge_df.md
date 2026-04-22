# Get an edge data frame from a graph

From a graph, obtain an edge data frame with all current edge
attributes.

## Usage

``` r
get_edge_df(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

An edge data frame.

## Examples

``` r
# Create a graph
graph <-
  create_graph() |>
  add_n_nodes(
    n = 1,
    type = "a") |>
  select_last_nodes_created() |>
  add_n_nodes_ws(
    n = 5,
    direction = "from",
    type = "b") |>
  select_edges_by_node_id(
    nodes = 3:5) |>
  set_edge_attrs_ws(
    edge_attr = color,
    value = "green") |>
  set_edge_attrs_ws(
    edge_attr = rel,
    value = "a") |>
  invert_selection() |>
  set_edge_attrs_ws(
    edge_attr = color,
    value = "blue") |>
  set_edge_attrs_ws(
    edge_attr = rel,
    value = "b") |>
  clear_selection()

# Get the graph's internal
# edge data frame (edf)
graph |> get_edge_df()
#>   id from to rel color
#> 1  1    1  2   b  blue
#> 2  2    1  3   a green
#> 3  3    1  4   a green
#> 4  4    1  5   a green
#> 5  5    1  6   b  blue
```
