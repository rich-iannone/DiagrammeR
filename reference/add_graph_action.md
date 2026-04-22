# Add a graph action for execution at every transform

Add a graph function along with its arguments to be run at every graph
transformation step.

## Usage

``` r
add_graph_action(graph, fcn, ..., action_name = NULL)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- fcn:

  The name of the function to use.

- ...:

  Arguments and values to pass to the named function in `fcn`, if
  necessary.

- action_name:

  An optional name for labeling the action.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph() |>
  add_gnm_graph(
    n = 10,
    m = 22,
    set_seed = 23)

# Add a graph action that sets a node
# attr column with a function; the
# main function `set_node_attr_w_fcn()`
# uses the `get_betweenness()` function
# to provide betweenness values in the
# `btwns` column; this action will
# occur whenever there is a function
# called on the graph that modifies it
# (e.g., `add_n_nodes()`)
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
