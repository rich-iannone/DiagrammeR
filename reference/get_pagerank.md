# Get the PageRank values for all nodes

Get the PageRank values for all nodes in the graph.

## Usage

``` r
get_pagerank(graph, directed = TRUE, damping = 0.85)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- directed:

  If `TRUE` (the default) then directed paths will be considered for
  directed graphs. This is ignored for undirected graphs.

- damping:

  The damping factor. The default value is set to `0.85`.

## Value

A data frame with PageRank values for each of the nodes.

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph() |>
  add_gnm_graph(
    n = 10,
    m = 15,
    set_seed = 23)

# Get the PageRank scores
# for all nodes in the graph
graph |>
  get_pagerank()
#>    id pagerank
#> 1   1   0.1302
#> 2   2   0.1037
#> 3   3   0.0450
#> 4   4   0.0450
#> 5   5   0.1501
#> 6   6   0.0578
#> 7   7   0.0871
#> 8   8   0.1780
#> 9   9   0.0744
#> 10 10   0.1287

# Colorize nodes according to their
# PageRank scores
graph <-
  graph |>
  join_node_attrs(
    df = get_pagerank(graph)) |>
  colorize_node_attrs(
    node_attr_from = pagerank,
    node_attr_to = fillcolor,
    palette = "RdYlGn")
```
