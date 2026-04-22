# Get node IDs associated with edges

Obtain a vector, data frame, or list of node IDs associated with edges
in a graph object. An optional filter by edge attribute can limit the
set of edges returned.

## Usage

``` r
get_edges(
  graph,
  conditions = NULL,
  return_type = "vector",
  return_values = "id"
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- conditions:

  an option to use filtering conditions for the retrieval of edges.

- return_type:

  using `vector` (the default), a vector of character objects
  representing the edges is provided. With `list` a list object will be
  provided that contains vectors of outgoing and incoming node IDs
  associated with edges. With `df`, a data frame containing outgoing and
  incoming node IDs associated with edges.

- return_values:

  using `id` (the default) results in node ID values returned in the
  edge definitions. With `label`, the node labels will instead be used
  to define edges.

## Value

A list, data frame, or a vector object, depending on the value given to
`return_type`.

## Examples

``` r
# Create a node data frame (ndf)
ndf <-
  create_node_df(
    n = 4,
    label = c("one", "two", "three", "four"),
    type = "letter",
    color = c("red", "green", "grey", "blue"),
    value = c(3.5, 2.6, 9.4, 2.7))

# Create an edge data frame (edf)
edf <-
  create_edge_df(
    from = c(1, 2, 3),
    to = c(4, 3, 1),
    rel = "leading_to",
    color = c("pink", "blue", "blue"),
    value = c(3.9, 2.5, 7.3))

# Create a graph
graph <-
  create_graph(
    nodes_df = ndf,
    edges_df = edf)

# Get all edges within a graph, returned as a list
graph |>
  get_edges(
    return_type = "vector")
#> [1] "1->4" "2->3" "3->1"

# Get all edges within a graph, returned as a
# data frame
graph |>
  get_edges(
    return_type = "df")
#>   from to
#> 1    1  4
#> 2    2  3
#> 3    3  1

# Get all edges returned as a list
graph |>
  get_edges(
    return_type = "list")
#> [[1]]
#> [1] 1 2 3
#> 
#> [[2]]
#> [1] 4 3 1
#> 

# Get a vector of edges using
# a numeric comparison (i.e.,
# all edges with a `value`
# attribute greater than 3)
graph |>
  get_edges(
    conditions = value > 3,
    return_type = "vector")
#> [1] "1->4" "3->1"

# Get a vector of edges using
# a matching condition
graph |>
  get_edges(
    conditions = color == "pink",
    return_type = "vector")
#> [1] "1->4"

# Use multiple conditions to
# return edges with the
# desired attribute values
graph |>
  get_edges(
    conditions =
      color == "blue" &
      value > 3,
    return_type = "vector")
#> [1] "3->1"

# Use `return_values = "label"`
# to return the labels of the
# connected nodes
graph |>
  get_edges(
    conditions =
      color == "blue" &
      value > 3,
    return_type = "vector",
    return_values = "label")
#> [1] "three->one"
```
