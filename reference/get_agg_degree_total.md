# Get an aggregate value from the total degree of nodes

Get a single, aggregate value from the total degree values for all nodes
in a graph, or, a subset of graph nodes.

## Usage

``` r
get_agg_degree_total(graph, agg, conditions = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- agg:

  the aggregation function to use for summarizing total degree values
  from graph nodes. The following aggregation functions can be used:
  `sum`, `min`, `max`, `mean`, or `median`.

- conditions:

  an option to use filtering conditions for the nodes to consider.

## Value

A vector with an aggregate total degree value.

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

# Get the mean total degree
# value from all nodes in
# the graph
graph |>
  get_agg_degree_total(
    agg = "mean")
#> [1] 3.5

# Other aggregation functions
# can be used (`min`, `max`,
# `median`, `sum`); let's get
# the median in this example
graph |>
  get_agg_degree_total(
    agg = "median")
#> [1] 3

# The aggregation of total
# degree can occur for a
# subset of the graph nodes
# and this is made possible
# by specifying `conditions`
# for the nodes
graph |>
  get_agg_degree_total(
    agg = "mean",
    conditions = value < 5.0)
#> [1] 4.666667
```
