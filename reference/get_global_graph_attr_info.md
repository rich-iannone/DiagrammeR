# Get global graph attributes

Get the available global attributes for a graph object of class
`dgr_graph`.

## Usage

``` r
get_global_graph_attr_info(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A data frame containing global attributes for the graph.

## Examples

``` r
# Create a new, empty graph
graph <- create_graph()

# View the graph's set of
# global attributes
graph |>
  get_global_graph_attr_info()
#> # A tibble: 17 × 3
#>    attr        value      attr_type
#>    <chr>       <chr>      <chr>    
#>  1 layout      neato      graph    
#>  2 outputorder edgesfirst graph    
#>  3 bgcolor     white      graph    
#>  4 fontname    Helvetica  node     
#>  5 fontsize    10         node     
#>  6 shape       circle     node     
#>  7 fixedsize   true       node     
#>  8 width       0.5        node     
#>  9 style       filled     node     
#> 10 fillcolor   aliceblue  node     
#> 11 color       gray70     node     
#> 12 fontcolor   gray50     node     
#> 13 fontname    Helvetica  edge     
#> 14 fontsize    8          edge     
#> 15 len         1.5        edge     
#> 16 color       gray80     edge     
#> 17 arrowsize   0.5        edge     
```
