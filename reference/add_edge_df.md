# Add edges from an edge data frame to an existing graph object

With a graph object of class `dgr_graph`, add edges from an edge data
frame to that graph.

## Usage

``` r
add_edge_df(graph, edge_df)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- edge_df:

  An edge data frame that is created using
  [`create_edge_df()`](https://rich-iannone.github.io/DiagrammeR/reference/create_edge_df.md).

## Value

A graph object of class `dgr_graph`.

## See also

Other edge creation and removal:
[`add_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge.md),
[`add_edge_clone()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge_clone.md),
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
# Create a graph with 4 nodes
# and no edges
graph <-
  create_graph() |>
  add_n_nodes(n = 4)

# Create an edge data frame (edf)
edf <-
  create_edge_df(
    from = c(1, 2, 3),
      to = c(4, 3, 1))

# Add the edge data frame to
# the graph object to create
# a graph with both nodes
# and edges
graph <-
  graph |>
  add_edge_df(
    edge_df = edf)

# Get the graph's edges to
# verify that the edf had
# been added
graph |>
  get_edges(
    return_type = "vector")
#> [1] "1->4" "2->3" "3->1"
```
