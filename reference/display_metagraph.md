# Display a property graph's underlying model

With a graph object of class `dgr_graph` that is also a property graph
(i.e., all nodes have an assigned `type` value and all edges have an
assigned `rel` value), display its metagraph in the RStudio Viewer. This
representation provides all combinations of edges of different `rel`
values to all nodes with distinct `type` values, including any edges to
nodes of the same `type` (shown as loops). The precondition of the graph
being a property graph can be verified by using the
[`is_property_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/is_property_graph.md)
function.

## Usage

``` r
display_metagraph(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`. This graph must fulfill the
  condition of being a property graph, otherwise the function yields an
  error.

## Examples

``` r
# Create a randomized property
# graph with 1000 nodes and 1350 edges
property_graph <-
  create_graph() |>
  add_gnm_graph(
    n = 1000,
    m = 1350,
    set_seed = 23) |>
  select_nodes_by_degree(
    expressions = "deg >= 3") |>
  set_node_attrs_ws(
    node_attr = type,
    value = "a") |>
  clear_selection() |>
  select_nodes_by_degree(
    expressions = "deg < 3") |>
  set_node_attrs_ws(
    node_attr = type,
    value = "b") |>
  clear_selection() |>
  select_nodes_by_degree(
    expressions = "deg == 0") |>
  set_node_attrs_ws(
    node_attr = type,
    value = "c") |>
  set_node_attr_to_display(
    attr = type)

# Select a random subset of edges
# for setting edge attributes
node_ids <- get_node_ids(property_graph)
sampled_nodes <- sample(
  node_ids,
  size = floor(0.15 * length(node_ids)))

property_graph <-
  property_graph |>
  select_edges_by_node_id(
    nodes = sampled_nodes) |>
  set_edge_attrs_ws(
    edge_attr = rel,
    value = "r_1") |>
  invert_selection() |>
  set_edge_attrs_ws(
    edge_attr = rel,
    value = "r_2") |>
  clear_selection() |>
  copy_edge_attrs(
    edge_attr_from = rel,
    edge_attr_to = label) |>
  add_global_graph_attrs(
    attr = "fontname",
    value = "Helvetica",
    attr_type = "edge") |>
  add_global_graph_attrs(
    attr = "fontcolor",
    value = "gray50",
    attr_type = "edge") |>
  add_global_graph_attrs(
    attr = "fontsize",
    value = 10,
    attr_type = "edge")

# Display this graph's
# metagraph, or, the underlying
# graph model for a property graph
# display_metagraph(property_graph)
```
