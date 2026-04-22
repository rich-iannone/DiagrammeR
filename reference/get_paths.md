# Get paths from a specified node in a directed graph

Obtain a list of all possible paths from a given node within a directed
graph.

## Usage

``` r
get_paths(
  graph,
  from = NULL,
  to = NULL,
  shortest_path = FALSE,
  longest_path = FALSE,
  distance = NULL
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- from:

  The node from which all paths will be determined.

- to:

  The node to which all paths will be determined.

- shortest_path:

  An option to return paths that are the shortest in the set of all
  determined paths.

- longest_path:

  An option to return paths that are the longest in the set of all
  determined paths.

- distance:

  A vector of integer values that specify which of the valid paths to
  return when filtering by distance.

## Value

A list of paths, sorted by ascending traversal length, comprising
vectors of node IDs in sequence of traversal through the graph.

## Examples

``` r
# Create a simple graph
graph <-
  create_graph() |>
  add_n_nodes(n = 8) |>
  add_edge(from = 1, to = 2) |>
  add_edge(from = 1, to = 3) |>
  add_edge(from = 3, to = 4) |>
  add_edge(from = 3, to = 5) |>
  add_edge(from = 4, to = 6) |>
  add_edge(from = 2, to = 7) |>
  add_edge(from = 7, to = 5) |>
  add_edge(from = 4, to = 8)

# Get a list of all paths outward from node `1`
graph |>
  get_paths(from = 1)
#> [[1]]
#> [1] 1 3 5
#> 
#> [[2]]
#> [1] 1 2 7 5
#> 
#> [[3]]
#> [1] 1 3 4 6
#> 
#> [[4]]
#> [1] 1 3 4 8
#> 

# Get a list of all paths leading to node `6`
graph |>
  get_paths(to = 6)
#> [[1]]
#> [1] 1 3 4 6
#> 

# Get a list of all paths from `1` to `5`
graph |>
  get_paths(
   from = 1,
   to = 5)
#> [[1]]
#> [1] 1 3 5
#> 
#> [[2]]
#> [1] 1 2 7 5
#> 

# Get a list of all paths from `1` up to a distance
# of 2 node traversals
graph |>
  get_paths(
    from = 1,
    distance = 2)
#> [[1]]
#> [1] 1 3 5
#> 
#> [[2]]
#> [1] 1 2 7
#> 
#> [[3]]
#> [1] 1 3 4
#> 

# Get a list of the shortest paths from `1` to `5`
get_paths(
  graph,
  from = 1,
  to = 5,
  shortest_path = TRUE)
#> [[1]]
#> [1] 1 3 5
#> 

# Get a list of the longest paths from `1` to `5`
get_paths(
  graph,
  from = 1,
  to = 5,
  longest_path = TRUE)
#> [[1]]
#> [1] 1 2 7 5
#> 
```
