# Get the number of mutually-connected node pairs

Get the number of mutually-connected node pairs. This works for directed
graphs.

## Usage

``` r
count_mutual_node_pairs(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A single numeric value representing the number of mutually-connected
node pairs.

## Examples

``` r
# Create a cycle graph
graph <-
  create_graph() |>
  add_cycle(n = 5)

# Get a count of mutually-connected
# node pairs
graph |> count_mutual_node_pairs()
#> [1] 0

# Create a full graph and then
# count the mutually-connected
# node pairs
create_graph() |>
  add_full_graph(n = 10) |>
  count_mutual_node_pairs()
#> [1] 45
```
