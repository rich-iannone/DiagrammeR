# Get information on any available graph actions

Get a tibble of the available graph actions, which contains information
on function invocations to be called on the graph at every
transformation step, or, when manually invoked with the
[`trigger_graph_actions()`](https://rich-iannone.github.io/DiagrammeR/reference/trigger_graph_actions.md)
function.

## Usage

``` r
get_graph_actions(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A `df_tbl` object.

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

# Add a graph action that sets a node
# attr column with a function; the
# main function `set_node_attr_w_fcn()`
# uses the `get_betweenness()` function
# to provide betweenness values in the
# `btwns` column
graph <-
  graph |>
  add_graph_action(
    fcn = "set_node_attr_w_fcn",
    node_attr_fcn = "get_betweenness",
    column_name = "btwns",
    action_name = "get_btwns")

# To ensure that the action is
# available in the graph, use the
# `get_graph_actions()` function
graph |> get_graph_actions()
#> # A tibble: 1 × 3
#>   action_index action_name expression                                           
#>          <int> <chr>       <chr>                                                
#> 1            1 get_btwns   set_node_attr_w_fcn(graph = graph, node_attr_fcn = '…
```
