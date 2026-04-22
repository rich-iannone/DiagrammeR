# Get a vector of node ID values

Obtain a vector of node ID values from a graph object. An optional
filter by node attribute can limit the set of node ID values returned.

## Usage

``` r
get_node_ids(graph, conditions = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- conditions:

  An option to use filtering conditions for the retrieval of nodes.

## Value

A vector of node ID values.

## Examples

``` r
# Create a node data
# frame (ndf)
ndf <-
  create_node_df(
    n = 4,
    type = "letter",
    color = c(
      "red", "green",
      "blue", "blue"),
    value = c(
      3.5, 2.6, 9.4, 2.7))

# Create a graph using
# the ndf
graph <-
  create_graph(
    nodes_df = ndf)

# Get a vector of all nodes in a graph
graph |> get_node_ids()
#> [1] 1 2 3 4

# Get a vector of node ID values using a
# numeric comparison (i.e., all nodes with
# `value` attribute greater than 3)
graph |>
  get_node_ids(
    conditions = value > 3)
#> [1] 1 3

# Get a vector of node ID values using
# a match pattern (i.e., all nodes with
# `color` attribute of `green`)
graph |>
  get_node_ids(
    conditions = color == "green")
#> [1] 2

# Use multiple conditions to return nodes
# with the desired attribute values
graph |>
  get_node_ids(
    conditions =
      color == "blue" &
      value > 5)
#> [1] 3
```
