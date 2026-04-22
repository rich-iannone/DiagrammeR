# Add a clone of an existing edge to the graph

Add a new edge to a graph object of class `dgr_graph` which is a clone
of an edge already in the graph. All edge attributes are preserved.

## Usage

``` r
add_edge_clone(graph, edge, from, to)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- edge:

  An edge ID corresponding to the graph edge to be cloned.

- from:

  The outgoing node from which the edge is connected.

- to:

  The incoming nodes to which each edge is connected.

## Value

A graph object of class `dgr_graph`.

## See also

Other edge creation and removal:
[`add_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge.md),
[`add_edge_df()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge_df.md),
[`add_edges_from_table()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edges_from_table.md),
[`add_edges_w_string()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edges_w_string.md),
[`add_forward_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/add_forward_edges_ws.md),
[`add_reverse_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/add_reverse_edges_ws.md),
[`copy_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/copy_edge_attrs.md),
[`create_edge_df()`](https://rich-iannone.github.io/DiagrammeR/reference/create_edge_df.md),
[`delete_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_edge.md),
[`delete_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_edges_ws.md),
[`delete_loop_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_loop_edges_ws.md),
[`drop_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/drop_edge_attrs.md),
[`edge_data()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_data.md),
[`join_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/join_edge_attrs.md),
[`mutate_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/mutate_edge_attrs.md),
[`mutate_edge_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/mutate_edge_attrs_ws.md),
[`recode_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/recode_edge_attrs.md),
[`rename_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/rename_edge_attrs.md),
[`rescale_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/rescale_edge_attrs.md),
[`rev_edge_dir()`](https://rich-iannone.github.io/DiagrammeR/reference/rev_edge_dir.md),
[`rev_edge_dir_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/rev_edge_dir_ws.md),
[`set_edge_attr_to_display()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attr_to_display.md),
[`set_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs.md),
[`set_edge_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs_ws.md)

## Examples

``` r
# Create a graph with a path of
# 2 nodes; supply a common `rel`
# edge attribute for all edges
# in this path and then add a
# `color` edge attribute
graph <-
  create_graph() |>
  add_path(
    n = 2,
    rel = "a") |>
  select_last_edges_created() |>
  set_edge_attrs(
    edge_attr = color,
    values = "steelblue") |>
  clear_selection()

# Display the graph's internal
# edge data frame
graph |> get_edge_df()
#>   id from to rel     color
#> 1  1    1  2   a steelblue

# Create a new node (will have
# node ID of `3`) and then
# create an edge between it and
# node `1` while reusing the edge
# attributes of edge `1` -> `2`
# (edge ID `1`)
graph_2 <-
  graph |>
  add_node() |>
  add_edge_clone(
    edge = 1,
    from = 3,
      to = 1)

# Display the graph's internal
# edge data frame
graph_2 |> get_edge_df()
#>   id from to rel     color
#> 1  1    1  2   a steelblue
#> 2  2    3  1   a steelblue

# The same change can be performed
# with some helper functions in the
# `add_edge_clone()` function call
graph_3 <-
  graph |>
    add_node()

graph_3 <-
  graph_3 |>
    add_edge_clone(
      edge = get_last_edges_created(graph_3),
      from = get_last_nodes_created(graph_3),
      to = 1)

# Display the graph's internal
# edge data frame
graph_3 |> get_edge_df()
#>   id from to rel     color
#> 1  1    1  2   a steelblue
#> 2  2    3  1   a steelblue
```
