# Delete vectors cached in a graph object

Delete vectors cached in a graph object of class `dgr_graph`.

## Usage

``` r
delete_cache(graph, name = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- name:

  One or more name of vector objects to delete from the cache. If none
  supplied, all cached vectors available in the graph will be deleted.

## Value

A vector.

## Examples

``` r
# Create an empty graph
graph <-
  create_graph()

# Cache 3 different vectors inside
# the graph object
graph <-
  graph |>
  set_cache(
    name = "a",
    to_cache = 1:4) |>
  set_cache(
    name = "b",
    to_cache = 5:9) |>
  set_cache(
    name = "c",
    to_cache = 10:14)

# Delete cache `b`
graph <-
  graph |>
  delete_cache(name = "b")

# Delete remaining cached vectors
graph <-
  graph |>
  delete_cache()
```
