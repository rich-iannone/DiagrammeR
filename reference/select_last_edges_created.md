# Select the last set of edges created in a graph

Select the last edges that were created in a graph object of class
`dgr_graph`. This function should ideally be used just after creating
the edges to be selected.

## Usage

``` r
select_last_edges_created(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a graph and add a cycle and then
# a tree in 2 separate function calls
graph <-
  create_graph() |>
  add_cycle(
    n = 3,
    rel = "a") |>
  add_balanced_tree(
    k = 2, h = 2,
    rel = "b")

# Select the last edges created (all edges
# from the tree) and then set their edge
# color to be `red`
graph <-
  graph |>
  select_last_edges_created() |>
  set_edge_attrs_ws(
    edge_attr = color,
    value = "red") |>
  clear_selection()

# Display the graph's internal edge
# data frame to verify the change
graph |> get_edge_df()
#>   id from to rel color
#> 1  1    1  2   a  <NA>
#> 2  2    2  3   a  <NA>
#> 3  3    3  1   a  <NA>
#> 4  4    4  5   b   red
#> 5  5    4  6   b   red
#> 6  6    5  7   b   red
#> 7  7    5  8   b   red
#> 8  8    6  9   b   red
#> 9  9    6 10   b   red
```
