# Use the breadth-first search (bfs) algorithm

With a chosen or random node serving as the starting point, perform a
breadth-first search of the whole graph and return the node ID values
visited. The bfs algorithm differs from depth-first search (dfs) in that
bfs will follow tree branches branches one level at a time until
terminating at leaf node (dfs traverses branches as far as possible).

## Usage

``` r
do_bfs(graph, node = NULL, direction = "all")
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- node:

  An optional node ID value to specify a single starting point for the
  bfs. If not provided, a random node from the graph will be chosen.

- direction:

  Using `all` (the default), the bfs will ignore edge direction while
  traversing through the graph. With `out` and `in`, traversals between
  adjacent nodes will respect the edge direction.

## Value

A vector containing node ID values for nodes visited during the
breadth-first search. The order of the node IDs corresponds to the order
visited.

## Examples

``` r
# Create a graph containing
# two balanced trees
graph <-
  create_graph() |>
  add_balanced_tree(
    k = 2, h = 2) |>
  add_balanced_tree(
    k = 3, h = 2)

# Perform a breadth-first
# search of the graph,
# beginning at the root node
# `1` (the default
# `direction = "all"` doesn't
# take edge direction into
# account)
graph |>
  do_bfs(node = 1)
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20

# If not specifying a
# starting node, the function
# will begin the search from
# a random node
graph |>
  do_bfs()
#>  [1]  9  8 12 13 14 10 11 15 16 17 18 19 20  1  2  3  4  5  6  7

# It's also possible to
# perform bfs while taking
# into account edge direction;
# using `direction = "in"`
# causes the bfs routine to
# visit nodes along inward edges
graph |>
  do_bfs(
    node = 1,
    direction = "in")
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20

# Using `direction = "out"`
# results in the bfs moving
# along solely outward edges
graph |>
  do_bfs(
    node = 1,
    direction = "out")
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
```
