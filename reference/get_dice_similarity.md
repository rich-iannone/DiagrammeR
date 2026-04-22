# Get Dice similarity coefficient scores

Get the Dice similarity coefficient scores for one or more nodes in a
graph.

## Usage

``` r
get_dice_similarity(graph, nodes = NULL, direction = "all", round_to = 3)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- nodes:

  an optional vector of node IDs to consider for Dice similarity scores.
  If not supplied, then similarity scores will be provided for every
  pair of nodes in the graph.

- direction:

  using `all` (the default), the function will ignore edge direction
  when determining scores for neighboring nodes. With `out` and `in`,
  edge direction for neighboring nodes will be considered.

- round_to:

  the maximum number of decimal places to retain for the Dice similarity
  coefficient scores. The default value is `3`.

## Value

A matrix with Dice similarity values for each pair of nodes considered.

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph(
    directed = FALSE) |>
  add_gnm_graph(
    n = 10,
    m = 15,
    set_seed = 23)

# Get the Dice similarity
# values for nodes `5`, `6`,
# and `7`
graph |>
  get_dice_similarity(
    nodes = 5:7)
#>       5     6     7
#> 5 1.000 0.444 0.571
#> 6 0.444 1.000 0.500
#> 7 0.571 0.500 1.000
```
