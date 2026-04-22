# Get the current selection available in a graph object

Get the current selection of node IDs or edge IDs from a graph object of
class `dgr_graph`.

## Usage

``` r
get_selection(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A vector with the current selection of node or edge ID values.

## Examples

``` r
# Create a simple graph
graph <-
  create_graph() |>
  add_path(n = 6)

# Select node `4`, then select
# all nodes a distance of 1 away
# from node `4`, and finally
# return the selection of nodes as
# a vector object
graph |>
  select_nodes(nodes = 4) |>
  select_nodes_in_neighborhood(
    node = 4,
    distance = 1) |>
  get_selection()
#> [1] 3 4 5

# Select edges associated with
# node `4` and return the
# selection of edges
graph |>
  select_edges_by_node_id(
    nodes = 4) |>
  get_selection()
#> [1] 3 4
```
