# Cache a vector in the graph

Place any vector in the cache of a graph object of class `dgr_graph`.

## Usage

``` r
set_cache(graph, to_cache, name = NULL, col = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- to_cache:

  Any vector or data frame. If a data frame is supplied then a single
  column for the vector to pull must be provided in the `col` argument.

- name:

  An optional name for the cached vector.

- col:

  If a data frame is provided in `to_cache` then a column name from that
  data frame must provided here.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph() |>
  add_gnm_graph(
    n = 10,
    m = 22,
    set_seed = 23
  )

# Get the closeness values for
# all nodes from `1` to `10` and
# store in the graph's cache
graph <-
  graph |>
  set_cache(
    name = "closeness_vector",
    to_cache = get_closeness(graph),
    col = "closeness"
  )

# Get the graph's cache
graph |>
  get_cache(name = "closeness_vector")
#>  [1] 0.07692308 0.08333333 0.07692308 0.06666667 0.05555556 0.06250000
#>  [7] 0.06666667 0.05000000 0.06666667 0.04761905

# Get the difference of betweenness
# and closeness values for nodes in
# the graph and store the vector in
# the graph's cache
graph <-
  graph |>
  set_cache(
    name = "difference",
    to_cache =
      get_betweenness(graph)$betweenness -
        get_closeness(graph)$closeness
  )

# Get the graph's cache
graph |>
  get_cache(name = "difference")
#>  [1]  9.25641026 28.91666667 19.08974359  2.60000000  0.44444444 17.93750000
#>  [7] 11.93333333 -0.05000000 10.26666667 -0.04761905
```
