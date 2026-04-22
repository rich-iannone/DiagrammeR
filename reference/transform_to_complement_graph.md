# Create a complement of a graph

Create a complement graph which contains only edges not present in the
input graph. It's important to nodes that any edge attributes in the
input graph's edges will be lost. Node attributes will be retained,
since they are not affected by this transformation.

## Usage

``` r
transform_to_complement_graph(graph, loops = FALSE)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- loops:

  An option for whether loops should be generated in the complement
  graph.

## Value

a graph object of class `dgr_graph`.

## Examples

``` r
# Create a simple graph
# with a single cycle
graph <-
  create_graph() |>
  add_cycle(n = 4)

# Get the graph's edge
# data frame
graph |> get_edge_df()
#>   id from to  rel
#> 1  1    1  2 <NA>
#> 2  2    2  3 <NA>
#> 3  3    3  4 <NA>
#> 4  4    4  1 <NA>

# Create the complement
# of the graph
graph_c <-
  graph |>
    transform_to_complement_graph()

# Get the edge data frame
# for the complement graph
graph_c |> get_edge_df()
#>   id from to  rel
#> 1  1    1  4 <NA>
#> 2  2    1  3 <NA>
#> 3  3    2  4 <NA>
#> 4  4    2  1 <NA>
#> 5  5    3  2 <NA>
#> 6  6    3  1 <NA>
#> 7  7    4  3 <NA>
#> 8  8    4  2 <NA>
```
