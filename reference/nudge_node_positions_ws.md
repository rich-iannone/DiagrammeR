# Move layout positions of a selection of nodes

With an active selection of nodes, move the position in either the `x`
or `y` directions, or both. Nodes in the selection that do not have
position information (i.e., `NA` values for the `x` or `y` node
attributes) will be ignored.

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
nudge_node_positions_ws(graph, dx, dy)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- dx:

  A single numeric value specifying the amount that selected nodes (with
  non-`NA` values for the `x` and `y` attributes) will be moved in the x
  direction. A positive value will move nodes right, negative left.

- dy:

  A single numeric value specifying the amount that selected nodes (with
  non-`NA` values for the `x` and `y` attributes) will be moved in the y
  direction. A positive value will move nodes up, negative down.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a simple graph with 4 nodes
graph <-
  create_graph() |>
  add_node(
    type = "a",
    label = "one") |>
  add_node(
    type = "a",
    label = "two") |>
  add_node(
    type = "b",
    label = "three") |>
  add_node(
    type = "b",
    label = "four")

# Add position information to each of
# the graph's nodes
graph <-
  graph |>
  set_node_position(
    node = 1, x = 1, y = 1) |>
  set_node_position(
    node = 2, x = 2, y = 2) |>
  set_node_position(
    node = 3, x = 3, y = 3) |>
  set_node_position(
    node = 4, x = 4, y = 4)

# Select all of the graph's nodes using the
# `select_nodes()` function (and only
# specifying the graph object)
graph <- select_nodes(graph)

# Move the selected nodes (all the nodes,
# in this case) 5 units to the right
graph <-
  graph |>
  nudge_node_positions_ws(
    dx = 5, dy = 0)

# View the graph's node data frame
graph |> get_node_df()
#>   id type label x y
#> 1  1    a   one 6 1
#> 2  2    a   two 7 2
#> 3  3    b three 8 3
#> 4  4    b  four 9 4

# Now select nodes that have `type == "b"`
# and move them in the `y` direction 2 units
# (the graph still has an active selection
# and so it must be cleared first)
graph <-
  graph |>
  clear_selection() |>
  select_nodes(
    conditions = type == "b") |>
  nudge_node_positions_ws(
    dx = 0, dy = 2)

# View the graph's node data frame
graph |> get_node_df()
#>   id type label x y
#> 1  1    a   one 6 1
#> 2  2    a   two 7 2
#> 3  3    b three 8 5
#> 4  4    b  four 9 6
```
