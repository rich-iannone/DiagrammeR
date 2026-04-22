# Traverse outward node-by-node until stopping conditions are met

From a graph object of class `dgr_graph`, move along outward edges from
one or more nodes present in a selection to other connected nodes,
replacing the current nodes in the selection with those nodes traversed
to until reaching nodes that satisfy one or more conditions.

This traversal function makes use of an active selection of nodes. After
the traversal, depending on the traversal conditions, there will either
be a selection of nodes or no selection at all.

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
`trav_out_until()`, or
[`trav_in_until()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_until.md).

## Usage

``` r
trav_out_until(
  graph,
  conditions,
  max_steps = 30,
  exclude_unmatched = TRUE,
  add_to_selection = FALSE
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- conditions:

  An option to use a stopping condition for the traversal. If the
  condition is met during the traversal (i.e., the node(s) traversed to
  match the condition), then those traversals will terminate at those
  nodes. Otherwise, traversals with continue and terminate when the
  number of steps provided in `max_steps` is reached.

- max_steps:

  The maximum number of
  [`trav_out()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out.md)
  steps (i.e., node-to-node traversals in the outward direction) to
  allow before stopping.

- exclude_unmatched:

  If `TRUE` (the default value) then any nodes not satisfying the
  conditions provided in `conditions` that are in the ending selection
  are excluded.

- add_to_selection:

  If `TRUE` then every node traversed will be part of the final
  selection of nodes. If `FALSE` (the default value) then only the nodes
  finally traversed to will be part of the final node selection.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a path graph and add
# values of 1 to 10 across the
# nodes from beginning to end;
# select the first path node
graph <-
  create_graph() |>
  add_path(
    n = 10,
    node_data = node_data(
      value = 1:10)) |>
  select_nodes_by_id(
    nodes = 1)

# Traverse outward, node-by-node
# until stopping at a node where
# the `value` attribute is 8
graph <-
  graph |>
  trav_out_until(
    conditions =
      value == 8)

# Get the graph's node selection
graph |> get_selection()
#> [1] 8

# Create two cycles in graph and
# add values of 1 to 6 to the
# first cycle, and values 7 to
# 12 in the second; select nodes
# `1` and `7`
graph <-
  create_graph() |>
  add_cycle(
    n = 6,
    node_data = node_data(
      value = 1:6)) |>
  add_cycle(
    n = 6,
    node_data = node_data(
      value = 7:12)) |>
  select_nodes_by_id(
    nodes = c(1, 7))

# Traverse outward, node-by-node
# from `1` and `7` until stopping
# at the first nodes where the
# `value` attribute is 5, 6, or 15;
# specify that we should only
# keep the finally traversed to
# nodes that satisfy the conditions
graph <-
  graph |>
  trav_out_until(
    conditions =
      value %in% c(5, 6, 9),
    exclude_unmatched = TRUE)

# Get the graph's node selection
graph |> get_selection()
#> [1] 5 9
```
