# Get an aggregate value from the indegree of nodes

Get a single, aggregate value from the indegree values for all nodes in
a graph, or, a subset of graph nodes.

## Usage

``` r
get_agg_degree_in(graph, agg, conditions = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- agg:

  The aggregation function to use for summarizing indegree values from
  graph nodes. The following aggregation functions can be used: `sum`,
  `min`, `max`, `mean`, or `median`.

- conditions:

  An option to use filtering conditions for the nodes to consider.

## Value

A vector with an aggregate indegree value.

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph() |>
  add_gnm_graph(
    n = 20,
    m = 35,
    set_seed = 23)

graph <-
  graph |>
  set_node_attrs(
    node_attr = value,
    values = rnorm(
      n = 20,
      mean = 5,
      sd = 1) |> round(1))

# Get the mean indegree value
# from all nodes in the graph
graph |>
  get_agg_degree_in(
    agg = "mean")
#> [1] 1.75

# Other aggregation functions
# can be used (`min`, `max`,
# `median`, `sum`); let's get
# the median in this example
graph |>
  get_agg_degree_in(
    agg = "median")
#> [1] 1.5

# The aggregation of indegree
# can occur for a subset of the
# graph nodes and this is made
# possible by specifying
# `conditions` for the nodes
graph |>
  get_agg_degree_in(
    agg = "mean",
    conditions = value > 5.0)
#> [1] 1.5
```
