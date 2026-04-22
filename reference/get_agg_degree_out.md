# Get an aggregate value from the outdegree of nodes

Get a single, aggregate value from the outdegree values for all nodes in
a graph, or, a subset of graph nodes.

## Usage

``` r
get_agg_degree_out(graph, agg, conditions = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- agg:

  The aggregation function to use for summarizing outdegree values from
  graph nodes. The following aggregation functions can be used: `sum`,
  `min`, `max`, `mean`, or `median`.

- conditions:

  An option to use filtering conditions for the nodes to consider.

## Value

A vector with an aggregate outdegree value.

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph() |>
  add_gnm_graph(
    n = 20,
    m = 35,
    set_seed = 23) |>
  set_node_attrs(
    node_attr = value,
    values = rnorm(
      n = 20,
      mean = 5,
      sd = 1) |> round(1))

# Get the mean outdegree value from all
# nodes in the graph
graph |>
  get_agg_degree_out(
    agg = "mean")
#> [1] 1.75

# Other aggregation functions can be used
# (`min`, `max`, `median`, `sum`); let's
# get the median in this example
graph |>
  get_agg_degree_out(
    agg = "median")
#> [1] 1

# The aggregation of outdegree can occur
# for a subset of the graph nodes and this
# is made possible by specifying `conditions`
# for the nodes
graph |>
  get_agg_degree_out(
    agg = "mean",
    conditions = value < 5.0)
#> [1] 2.555556
```
