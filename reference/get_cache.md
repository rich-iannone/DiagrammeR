# Get a cached vector from a graph object

Get the vector cached in a graph object of class `dgr_graph`.

## Usage

``` r
get_cache(graph, name = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- name:

  the name of the object to extract from the cache. If none supplied,
  the most recent object added to the cache will be returned.

## Value

A vector.

## Examples

``` r
# Set a seed
suppressWarnings(RNGversion("3.5.0"))
set.seed(23)

# Create a graph with 5 nodes and 5 edges
graph <-
  create_graph() |>
  add_n_nodes(n = 5) |>
  set_node_attrs(
    node_attr = value,
    values = rnorm(
      n = 5,
      mean = 8,
      sd = 2)) |>
  add_edges_w_string(
    edges = "1->2 1->3 2->4 2->5 3->2")

# Cache all values from the node attribute `value`
# as a numeric vector
graph <-
  graph |>
  set_cache(
    name = "value",
    to_cache = get_node_attrs(
      graph = graph,
      node_attr = value))

# Return the cached vector
graph |> get_cache()
#>         1         2         3         4         5 
#>  9.993210 10.214981  7.443827 10.038411  8.090874 
```
