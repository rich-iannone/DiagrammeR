# Create a subgraph using a node or edge selection

Create a subgraph based on a selection of nodes or edges stored in the
graph object.

This function makes use of an active selection of nodes or edges (and
the function ending with `_ws` hints at this).

Selections of nodes can be performed using the following node selection
(`select_*()`) functions:
[`select_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes.md),
[`select_last_nodes_created()`](https://rich-iannone.github.io/DiagrammeR/reference/select_last_nodes_created.md),
[`select_nodes_by_degree()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes_by_degree.md),
[`select_nodes_by_id()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes_by_id.md),
or
[`select_nodes_in_neighborhood()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes_in_neighborhood.md).

Selections of edges can be performed using the following edge selection
(`select_*()`) functions:
[`select_edges()`](https://rich-iannone.github.io/DiagrammeR/reference/select_edges.md),
[`select_last_edges_created()`](https://rich-iannone.github.io/DiagrammeR/reference/select_last_edges_created.md),
[`select_edges_by_edge_id()`](https://rich-iannone.github.io/DiagrammeR/reference/select_edges_by_edge_id.md),
or
[`select_edges_by_node_id()`](https://rich-iannone.github.io/DiagrammeR/reference/select_edges_by_node_id.md).

Selections of nodes or edges can also be performed using the following
traversal (`trav_*()`) functions:
[`trav_out()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out.md),
[`trav_in()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in.md),
[`trav_both()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_both.md),
[`trav_out_node()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out_node.md),
[`trav_in_node()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_node.md),
[`trav_out_until()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out_until.md),
[`trav_in_until()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_until.md),
[`trav_out_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out_edge.md),
[`trav_in_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_edge.md),
[`trav_both_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_both_edge.md),
or
[`trav_reverse_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_reverse_edge.md).

## Usage

``` r
transform_to_subgraph_ws(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a node data frame (ndf)
ndf <-
  create_node_df(
    n = 6,
    value =
      c(3.5, 2.6, 9.4,
        2.7, 5.2, 2.1))

# Create an edge data frame (edf)
edf <-
  create_edge_df(
    from = c(1, 2, 4, 5, 2, 6, 2),
      to = c(2, 4, 1, 3, 5, 5, 4))

# Create a graph
graph <-
  create_graph(
    nodes_df = ndf,
    edges_df = edf)

# Create a selection of nodes, this selects
# nodes `1`, `3`, and `5`
graph <-
  graph |>
  select_nodes(
    conditions = value > 3)

# Create a subgraph based on the selection
subgraph <-
  graph |>
  transform_to_subgraph_ws()

# Display the graph's node data frame
subgraph |> get_node_df()
#>   id type label value
#> 1  1 <NA>  <NA>   3.5
#> 2  3 <NA>  <NA>   9.4
#> 3  5 <NA>  <NA>   5.2

# Display the graph's edge data frame
subgraph |> get_edge_df()
#>   id from to  rel
#> 1  4    5  3 <NA>

# Create a selection of edges, this selects
# edges `1`, `2`
graph <-
  graph |>
  clear_selection() |>
  select_edges(
  edges = c(1,2))

# Create a subgraph based on the selection
subgraph <-
  graph |>
  transform_to_subgraph_ws()

# Display the graph's node data frame
subgraph |> get_node_df()
#>   id type label value
#> 1  1 <NA>  <NA>   3.5
#> 2  2 <NA>  <NA>   2.6
#> 3  4 <NA>  <NA>   2.7

# Display the graph's edge data frame
subgraph |> get_edge_df()
#>   id from to  rel
#> 1  1    1  2 <NA>
#> 2  2    2  4 <NA>
```
