# Reorder the stored series of graph actions

Reorder the graph actions stored in the graph through the use of the
[`add_graph_action()`](https://rich-iannone.github.io/DiagrammeR/reference/add_graph_action.md)
function. These actions are be invoked in a specified order via the
[`trigger_graph_actions()`](https://rich-iannone.github.io/DiagrammeR/reference/trigger_graph_actions.md)
function.

## Usage

``` r
reorder_graph_actions(graph, indices)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- indices:

  A numeric vector that provides the new ordering of graph actions. This
  vector can be the same length as the number of graph actions, or, of
  shorter length. In the latter case, the ordering places the given
  items first and the remaining actions will follow.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph() |>
  add_gnm_graph(
    n = 4,
    m = 4,
    set_seed = 23)

# Add three graph actions to the
# graph
graph <-
  graph |>
  add_graph_action(
    fcn = "rescale_node_attrs",
    node_attr_from = "pagerank",
    node_attr_to = "width",
    action_name = "pgrnk_to_width") |>
  add_graph_action(
    fcn = "set_node_attr_w_fcn",
    node_attr_fcn = "get_pagerank",
    column_name = "pagerank",
    action_name = "get_pagerank") |>
  add_graph_action(
    fcn = "colorize_node_attrs",
    node_attr_from = "width",
    node_attr_to = "fillcolor",
    action_name = "pgrnk_fillcolor")

# View the graph actions for the graph
# object by using the function called
# `get_graph_actions()`
graph |> get_graph_actions()
#> # A tibble: 3 × 3
#>   action_index action_name     expression                                       
#>          <int> <chr>           <chr>                                            
#> 1            1 pgrnk_to_width  rescale_node_attrs(graph = graph, node_attr_from…
#> 2            2 get_pagerank    set_node_attr_w_fcn(graph = graph, node_attr_fcn…
#> 3            3 pgrnk_fillcolor colorize_node_attrs(graph = graph, node_attr_fro…

# We note that the order isn't
# correct and that the `get_pagerank`
# action should be the 1st action
# and `pgrnk_to_width` should go
# in 2nd place; to fix this, use the
# function `reorder_graph_actions()`
# and specify the reordering with a
# numeric vector
graph <-
  graph |>
  reorder_graph_actions(
    indices = c(2, 1, 3))

# View the graph actions for the graph
# object once again to verify that
# we have the desired order of actions
graph |> get_graph_actions()
#> # A tibble: 3 × 3
#>   action_index action_name     expression                                       
#>          <int> <chr>           <chr>                                            
#> 1            1 get_pagerank    set_node_attr_w_fcn(graph = graph, node_attr_fcn…
#> 2            2 pgrnk_to_width  rescale_node_attrs(graph = graph, node_attr_from…
#> 3            3 pgrnk_fillcolor colorize_node_attrs(graph = graph, node_attr_fro…
```
