# Combine multiple edge data frames into a single edge data frame

Combine several edge data frames in the style of
[`rbind()`](https://rdrr.io/r/base/cbind.html), except, it works
regardless of the number and ordering of the columns.

## Usage

``` r
combine_edfs(...)
```

## Arguments

- ...:

  Two or more edge data frames, which contain edge IDs and associated
  attributes.

## Value

A combined edge data frame.

## Examples

``` r
# Create an edge data frame (edf)
edf_1 <-
  create_edge_df(
    from = c(1, 1, 2, 3),
    to = c(2, 4, 4, 1),
    rel = "requires",
    color = "green",
    data = c(2.7, 8.9, 2.6, 0.6))

# Create a second edge data frame
edf_2 <-
  create_edge_df(
    from = c(5, 7, 8, 8),
    to = c(7, 8, 6, 5),
    rel = "receives",
    arrowhead = "dot",
    color = "red")

# Combine the two edge data frames
all_edges <- combine_edfs(edf_1, edf_2)

# View the combined edge data frame
all_edges
#>   id from to      rel color data arrowhead
#> 1  1    1  2 requires green  2.7      <NA>
#> 2  2    1  4 requires green  8.9      <NA>
#> 3  3    2  4 requires green  2.6      <NA>
#> 4  4    3  1 requires green  0.6      <NA>
#> 5  5    5  7 receives   red   NA       dot
#> 6  6    7  8 receives   red   NA       dot
#> 7  7    8  6 receives   red   NA       dot
#> 8  8    8  5 receives   red   NA       dot
```
