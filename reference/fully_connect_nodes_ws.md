# Fully connect all nodes in a selection of nodes

With a selection of nodes in a graph, add any remaining edges required
to fully connect this group of edges to each other.

This function makes use of an active selection of nodes (and the
function ending with `_ws` hints at this).

Selections of nodes can be performed using the following node selection
(`select_*()`) functions:
[`select_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes.md),
[`select_last_nodes_created()`](https://rich-iannone.github.io/DiagrammeR/reference/select_last_nodes_created.md),
[`select_nodes_by_degree()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes_by_degree.md),
[`select_nodes_by_id()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes_by_id.md),
or
[`select_nodes_in_neighborhood()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes_in_neighborhood.md).

Selections of nodes can also be performed using the following traversal
(`trav_*()`) functions:
[`trav_out()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out.md),
[`trav_in()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in.md),
[`trav_both()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_both.md),
[`trav_out_node()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out_node.md),
[`trav_in_node()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_node.md),
[`trav_out_until()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out_until.md),
or
[`trav_in_until()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_until.md).

## Usage

``` r
fully_connect_nodes_ws(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create an empty graph and
# then add a path of 3 nodes
# and two isolated nodes
graph <-
  create_graph() |>
  add_path(n = 3) |>
  add_n_nodes(n = 2)

# Select a node in the path
# of nodes (node `3`) and
# the two isolated nodes (`4`
# and `5`); then, and fully
# connect these nodes together
graph <-
  graph |>
  select_nodes_by_id(
    nodes = 3:5) |>
  fully_connect_nodes_ws()

# Get the graph's edge data frame
graph |> get_edge_df()
#>   id from to  rel
#> 1  1    1  2 <NA>
#> 2  2    2  3 <NA>
#> 3  3    3  4 <NA>
#> 4  4    3  5 <NA>
#> 5  5    4  5 <NA>
#> 6  6    4  3 <NA>
#> 7  7    5  3 <NA>
#> 8  8    5  4 <NA>

# Create an undirected, empty
# graph; add a path of 3 nodes
# and two isolated nodes
graph <-
  create_graph(
    directed = FALSE) |>
  add_path(n = 3) |>
  add_n_nodes(n = 2)

# Select a node in the path
# of nodes (node `3`) and
# the two isolated nodes (`4`
# and `5`); then, and fully
# connect these nodes together
graph <-
  graph |>
  select_nodes_by_id(
    nodes = 3:5) |>
  fully_connect_nodes_ws()

# Get the graph's edge data
# frame; in the undirected
# case, reverse edges aren't
# added
graph |> get_edge_df()
#>   id from to  rel
#> 1  1    1  2 <NA>
#> 2  2    2  3 <NA>
#> 3  3    3  4 <NA>
#> 4  4    3  5 <NA>
#> 5  5    4  5 <NA>
```
