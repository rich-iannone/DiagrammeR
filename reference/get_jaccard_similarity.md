# Get Jaccard similarity coefficient scores

Get the Jaccard similarity coefficient scores for one or more nodes in a
graph.

## Usage

``` r
get_jaccard_similarity(graph, nodes = NULL, direction = "all", round_to = 3)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- nodes:

  An optional vector of node IDs to consider for Jaccard similarity
  scores. If not supplied, then similarity scores will be provided for
  every pair of nodes in the graph.

- direction:

  Using `all` (the default), the function will ignore edge direction
  when determining scores for neighboring nodes. With `out` and `in`,
  edge direction for neighboring nodes will be considered.

- round_to:

  The maximum number of decimal places to retain for the Jaccard
  similarity coefficient scores. The default value is `3`.

## Value

A matrix with Jaccard similarity values for each pair of nodes
considered.

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

# Get the Jaccard similarity
# values for nodes `5`, `6`,
# and `7`
graph |>
  get_jaccard_similarity(
    nodes = 5:7)
#>       5     6     7
#> 5 1.000 0.286 0.400
#> 6 0.286 1.000 0.333
#> 7 0.400 0.333 1.000
```
