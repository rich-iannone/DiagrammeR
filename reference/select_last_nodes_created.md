# Select the last set of nodes created in a graph

Select the last nodes that were created in a graph object of class
`dgr_graph`. This function should ideally be used just after creating
the nodes to be selected.

## Usage

``` r
select_last_nodes_created(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a graph and add 4 nodes
# in 2 separate function calls
graph <-
  create_graph() |>
  add_n_nodes(
    n = 2,
    type = "a",
    label = c("a_1", "a_2")) |>
  add_n_nodes(
    n = 2,
    type = "b",
    label = c("b_1", "b_2"))

# Select the last nodes created (2 nodes
# from the last function call) and then
# set their color to be `red`
graph <-
  graph |>
  select_last_nodes_created() |>
  set_node_attrs_ws(
    node_attr = color,
    value = "red") |>
  clear_selection()

# Display the graph's internal node
# data frame to verify the change
graph |> get_node_df()
#>   id type label color
#> 1  1    a   a_1  <NA>
#> 2  2    a   a_2  <NA>
#> 3  3    b   b_1   red
#> 4  4    b   b_2   red
```
