# Get the number of asymmetrically-connected node pairs

Get the number of asymmetrically-connected node pairs. This works for
directed graphs.

## Usage

``` r
count_asymmetric_node_pairs(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A single numeric value representing the number of
asymmetrically-connected node pairs.

## Examples

``` r
# Create a cycle graph
graph <-
  create_graph() |>
  add_cycle(n = 5)

# Get a count of asymmetrically-
# connected node pairs
graph |>
  count_asymmetric_node_pairs()
#> [1] 5

# Create a full graph and then
# count the asymmetrically-
# connected node pairs
create_graph() |>
  add_full_graph(n = 10) |>
  count_asymmetric_node_pairs()
#> [1] 0
```
