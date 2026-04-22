# Delete one or more graph actions stored within a graph object

Delete one or more graph actions stored within a graph object of class
`dgr_graph`).

## Usage

``` r
delete_graph_actions(graph, actions)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- actions:

  Either a vector of integer numbers indicating which actions to delete
  (based on `action_index` values), or, a character vector corresponding
  to `action_name` values.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph() |>
  add_gnm_graph(
    n = 5,
    m = 8,
    set_seed = 23)

# Add three graph actions to the
# graph
graph <-
  graph |>
  add_graph_action(
    fcn = "set_node_attr_w_fcn",
    node_attr_fcn = "get_pagerank",
    column_name = "pagerank",
    action_name = "get_pagerank") |>
  add_graph_action(
    fcn = "rescale_node_attrs",
    node_attr_from = "pagerank",
    node_attr_to = "width",
    action_name = "pagerank_to_width") |>
  add_graph_action(
    fcn = "colorize_node_attrs",
    node_attr_from = "width",
    node_attr_to = "fillcolor",
    action_name = "pagerank_fillcolor")

# View the graph actions for the graph
# object by using the `get_graph_actions()`
# function
graph |> get_graph_actions()
#> # A tibble: 3 × 3
#>   action_index action_name        expression                                    
#>          <int> <chr>              <chr>                                         
#> 1            1 get_pagerank       set_node_attr_w_fcn(graph = graph, node_attr_…
#> 2            2 pagerank_to_width  rescale_node_attrs(graph = graph, node_attr_f…
#> 3            3 pagerank_fillcolor colorize_node_attrs(graph = graph, node_attr_…

# Delete the second and third graph
# actions using `delete_graph_actions()`
graph <-
  graph |>
  delete_graph_actions(
    actions = c(2, 3))

# Verify that these last two graph
# actions were deleted by again using
# the `get_graph_actions()` function
graph |> get_graph_actions()
#> # A tibble: 1 × 3
#>   action_index action_name  expression                                          
#>          <int> <chr>        <chr>                                               
#> 1            1 get_pagerank set_node_attr_w_fcn(graph = graph, node_attr_fcn = …
```
