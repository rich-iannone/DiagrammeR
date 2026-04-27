# Changelog

## DiagrammeR (development version)

## DiagrammeR 1.0.12

CRAN release: 2026-04-27

- The minimum R version is now 4.1 to reflect usage of the native pipe
  operator `|>`.

- Fixed
  [`get_leverage_centrality()`](https://rich-iannone.github.io/DiagrammeR/reference/get_leverage_centrality.md)
  which erroneously passed the full degree vector instead of a single
  vertex index to
  [`igraph::neighbors()`](https://r.igraph.org/reference/neighbors.html)
  ([\#541](https://github.com/rich-iannone/DiagrammeR/issues/541)).

- Fixed
  [`get_authority_centrality()`](https://rich-iannone.github.io/DiagrammeR/reference/get_authority_centrality.md)
  and `get_eigenvector_centrality()` to safely call
  [`igraph::hits_scores()`](https://r.igraph.org/reference/hits_scores.html)
  and handle the igraph v2+ authority/hub API
  ([\#542](https://github.com/rich-iannone/DiagrammeR/issues/542)).

- Fixed GML import to correctly apply edge labels
  ([\#543](https://github.com/rich-iannone/DiagrammeR/issues/543)).

- Fixed
  [`grViz()`](https://rich-iannone.github.io/DiagrammeR/reference/grViz.md)
  to correctly pass options to `Viz()`
  ([@cderv](https://github.com/cderv),
  [\#530](https://github.com/rich-iannone/DiagrammeR/issues/530)).

- Fixed edge attribute generation logic when edge attributes are empty
  strings
  ([\#521](https://github.com/rich-iannone/DiagrammeR/issues/521)).

- Migrated all examples and internal code from `%>%` to the native pipe
  `|>` ([\#540](https://github.com/rich-iannone/DiagrammeR/issues/540)).

## DiagrammeR 1.0.11

CRAN release: 2024-02-02

- DiagrammeR now has a dependency on viridisLite instead of viridis
  ([@olivroy](https://github.com/olivroy),
  [\#511](https://github.com/rich-iannone/DiagrammeR/issues/511))

- DiagrammeR nows uses testthat 3rd edition
  ([@olivroy](https://github.com/olivroy),
  [\#498](https://github.com/rich-iannone/DiagrammeR/issues/498))

- No longer use deprecated features from tibble, igraph and tidyselect
  (\>= 1.2.0) ([@olivroy](https://github.com/olivroy),
  [\#497](https://github.com/rich-iannone/DiagrammeR/issues/497),
  [\#507](https://github.com/rich-iannone/DiagrammeR/issues/507))

- Error messages have been reviewed and now use **cli**
  ([@olivroy](https://github.com/olivroy),
  [\#499](https://github.com/rich-iannone/DiagrammeR/issues/499),
  [\#502](https://github.com/rich-iannone/DiagrammeR/issues/502))

- It is now easier to install suggested packages on the fly. DiagrammeR
  now uses
  [`rlang::check_installed()`](https://rlang.r-lib.org/reference/is_installed.html)
  internally. ([@olivroy](https://github.com/olivroy),
  [\#499](https://github.com/rich-iannone/DiagrammeR/issues/499))

- [`DiagrammeR()`](https://rich-iannone.github.io/DiagrammeR/reference/DiagrammeR.md)
  checks the `type` argument more strictly.
  ([@olivroy](https://github.com/olivroy),
  [\#506](https://github.com/rich-iannone/DiagrammeR/issues/506))

- DiagrammeR is compatible with igraph (\>= 2.0.0)
  ([@maelle](https://github.com/maelle),
  [\#500](https://github.com/rich-iannone/DiagrammeR/issues/500))

## DiagrammeR 1.0.10

CRAN release: 2023-05-18

- Remove dependency on the **influenceR** package, which also means
  removing the `get_constraint()` and `get_bridging()` graph inspection
  functions.
  ([\#486](https://github.com/rich-iannone/DiagrammeR/issues/486))

## DiagrammeR 1.0.9

CRAN release: 2022-03-05

- More safely check inputs to
  [`grViz()`](https://rich-iannone.github.io/DiagrammeR/reference/grViz.md)
  and
  [`mermaid()`](https://rich-iannone.github.io/DiagrammeR/reference/mermaid.md)

## DiagrammeR 1.0.8

CRAN release: 2022-01-24

- Reduce minimum R version requirement.

## DiagrammeR 1.0.7

CRAN release: 2022-01-15

- Fix malformed CSS selector in `htmlwidgets/grViz.js`

- Fixes for dev tidyr. Only call `replace_na(replace = "")` on character
  columns. ([@DavisVaughan](https://github.com/DavisVaughan),
  [\#448](https://github.com/rich-iannone/DiagrammeR/issues/448))

- Added the `envir` argument to the
  [`grViz()`](https://rich-iannone.github.io/DiagrammeR/reference/grViz.md)
  and
  [`replace_in_spec()`](https://rich-iannone.github.io/DiagrammeR/reference/replace_in_spec.md)
  functions. ([@atusy](https://github.com/atusy),
  [\#408](https://github.com/rich-iannone/DiagrammeR/issues/408))

## DiagrammeR 1.0.6.1

CRAN release: 2020-05-08

- Removed the `set_df_as_node_attr()`, `set_df_as_edge_attr()`, and
  `get_attr_dfs()` functions.

## DiagrammeR 1.0.5

CRAN release: 2020-01-16

- Removes the ability to save a DiagrammeR graph object as a Gephi file
  (.gexf) since the **rgexf** package is no longer maintained.

## DiagrammeR 1.0.0

CRAN release: 2018-03-01

- Added the helper functions
  [`node_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/node_aes.md),
  [`node_data()`](https://rich-iannone.github.io/DiagrammeR/reference/node_data.md),
  [`edge_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_aes.md),
  and
  [`edge_data()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_data.md)
  to facilitate the binding of node and edge aesthetic and data
  attribute values; several functions now have namesake arguments that
  accept these functions’ output.

- Added traversal functions
  [`trav_in_until()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_until.md)
  and
  [`trav_out_until()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out_until.md).

- Information about the graph is now displayed in the console when the
  graph object is called

- Error messages are now more helpful and try to provide pointers for
  the more common errors.

### Deprecated

- `create_complement_graph()` is deprecated in favor of
  [`create_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/create_graph.md) +
  [`transform_to_complement_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/transform_to_complement_graph.md).

- `create_subgraph_ws()` is deprecated in favor of
  [`create_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/create_graph.md) +
  [`transform_to_subgraph_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/transform_to_subgraph_ws.md).

- `edge_rel()` is deprecated in favor of `graph$edges_df$rel` for edge
  relationships.

- `create_random_graph()` is deprecated in favor of
  [`create_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/create_graph.md) +
  [`add_gnm_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/add_gnm_graph.md),
  [`add_growing_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/add_growing_graph.md),
  [`add_islands_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/add_islands_graph.md),
  [`add_smallworld_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/add_smallworld_graph.md)
  for the creation of randomized graphs.

### Functions renamed: (old functions were removed)

- `add_to_series()` -\>
  [`add_graph_to_graph_series()`](https://rich-iannone.github.io/DiagrammeR/reference/add_graph_to_graph_series.md)
- `remove_from_series()` -\>
  [`remove_graph_from_graph_series()`](https://rich-iannone.github.io/DiagrammeR/reference/remove_graph_from_graph_series.md)
- `get_graph_from_series()` -\>
  [`get_graph_from_graph_series()`](https://rich-iannone.github.io/DiagrammeR/reference/get_graph_from_graph_series.md)
- `create_series()` -\>
  [`create_graph_series()`](https://rich-iannone.github.io/DiagrammeR/reference/create_graph_series.md)
- `subset_series()` -\>
  [`filter_graph_series()`](https://rich-iannone.github.io/DiagrammeR/reference/filter_graph_series.md)
- `render_graph_from_series()` -\>
  [`render_graph_from_graph_series()`](https://rich-iannone.github.io/DiagrammeR/reference/render_graph_from_graph_series.md)
- `series_info()` -\>
  [`get_graph_series_info()`](https://rich-iannone.github.io/DiagrammeR/reference/get_graph_series_info.md)
- `graph_count()` -\>
  [`count_graphs_in_graph_series()`](https://rich-iannone.github.io/DiagrammeR/reference/count_graphs_in_graph_series.md)
- `graph_info()` -\>
  [`get_graph_info()`](https://rich-iannone.github.io/DiagrammeR/reference/get_graph_info.md)
- `node_present()` -\>
  [`is_node_present()`](https://rich-iannone.github.io/DiagrammeR/reference/is_node_present.md)
- `edge_present()` -\>
  [`is_edge_present()`](https://rich-iannone.github.io/DiagrammeR/reference/is_edge_present.md)
- `edge_count()` -\>
  [`count_edges()`](https://rich-iannone.github.io/DiagrammeR/reference/count_edges.md)
- `node_count()` -\>
  [`count_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/count_nodes.md)
- `edge_info()` -\>
  [`get_edge_info()`](https://rich-iannone.github.io/DiagrammeR/reference/get_edge_info.md)
- `node_info()` -\>
  [`get_node_info()`](https://rich-iannone.github.io/DiagrammeR/reference/get_node_info.md)
- `node_type()` -\>
  [`get_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/get_node_attrs.md)
- `get_global_graph_attrs()` -\>
  [`get_global_graph_attr_info()`](https://rich-iannone.github.io/DiagrammeR/reference/get_global_graph_attr_info.md)
- `get_min_spanning_tree()` -\>
  [`transform_to_min_spanning_tree()`](https://rich-iannone.github.io/DiagrammeR/reference/transform_to_min_spanning_tree.md)

### Functions removed:

`cache_edge_attrs()`, `cache_edge_attrs_ws()`, `cache_edge_count_ws()`,
`cache_node_attrs()`, `cache_node_attrs_ws()`, `cache_node_count_ws()`,
`clear_global_graph_attrs()`, `image_icon()`,
`set_global_graph_attrs()`.

### Functions added

[`add_node_clones_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/add_node_clones_ws.md),
[`count_loop_edges()`](https://rich-iannone.github.io/DiagrammeR/reference/count_loop_edges.md),
[`count_s_connected_cmpts()`](https://rich-iannone.github.io/DiagrammeR/reference/count_s_connected_cmpts.md),
[`count_unconnected_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/count_unconnected_nodes.md),
[`count_w_connected_cmpts()`](https://rich-iannone.github.io/DiagrammeR/reference/count_w_connected_cmpts.md),
[`delete_cache()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_cache.md),
[`delete_loop_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_loop_edges_ws.md),
[`deselect_edges()`](https://rich-iannone.github.io/DiagrammeR/reference/deselect_edges.md),
[`deselect_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/deselect_nodes.md),
[`fully_connect_nodes_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/fully_connect_nodes_ws.md),
[`fully_disconnect_nodes_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/fully_disconnect_nodes_ws.md),
[`get_edge_df_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/get_edge_df_ws.md),
[`get_node_df_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/get_node_df_ws.md),
[`is_graph_undirected()`](https://rich-iannone.github.io/DiagrammeR/reference/is_graph_undirected.md),
[`set_graph_directed()`](https://rich-iannone.github.io/DiagrammeR/reference/set_graph_directed.md),
[`set_edge_attr_to_display()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attr_to_display.md)

## DiagrammeR 0.9.2

CRAN release: 2017-09-06

- Added functions to generate 2D and 3D grid graphs
  ([`add_grid_2d()`](https://rich-iannone.github.io/DiagrammeR/reference/add_grid_2d.md)
  and
  [`add_grid_3d()`](https://rich-iannone.github.io/DiagrammeR/reference/add_grid_3d.md))

- Added `_ws()` (with selection) variants of the
  `mutate_[node/edge]_attrs()` functions for mutating node or edge
  attributes for only those nodes/edges in an active selection

- Incorporated an `edges` argument into the
  [`select_edges()`](https://rich-iannone.github.io/DiagrammeR/reference/select_edges.md)
  function in order to filter the selection of edges to a set of edge ID
  values

- Reduced the dependency on R to version \>= 3.2.0

## DiagrammeR 0.9.1

CRAN release: 2017-08-21

- Simplified many functions internally

- Added a default print method for graph objects

- Allowed use of bare node or edge attribute names in many functions

- Implemented graph actions as a means to run one or more functions at
  every graph transformation step; for example, this can be used to
  automatically update a node attribute such as `betweenness` whenever
  modifications to the graph are made (e.g., adding nodes, removing
  edges, etc.)

- Data frames can be set as node or edge attributes with the
  `set_df_as_node_attr()` and `set_df_as_edge_attr()` functions; the
  `get_attr_dfs()` function allows for retrieval of stored data frame
  data

- Added two new graph-generating functions
  ([`add_gnp_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/add_gnp_graph.md),
  [`add_pa_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/add_pa_graph.md),
  and `create_complement_graph()`)

- Added functions to clone existing nodes and edges
  ([`add_n_node_clones()`](https://rich-iannone.github.io/DiagrammeR/reference/add_n_node_clones.md)
  and
  [`add_edge_clone()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge_clone.md))

- Added several `count_*` functions
  ([`count_asymmetric_node_pairs()`](https://rich-iannone.github.io/DiagrammeR/reference/count_asymmetric_node_pairs.md),
  [`count_automorphisms()`](https://rich-iannone.github.io/DiagrammeR/reference/count_automorphisms.md),
  etc.)

- Added new functions to obtain graph properties
  ([`get_adhesion()`](https://rich-iannone.github.io/DiagrammeR/reference/get_adhesion.md),
  [`get_girth()`](https://rich-iannone.github.io/DiagrammeR/reference/get_girth.md),
  [`get_reciprocity()`](https://rich-iannone.github.io/DiagrammeR/reference/get_reciprocity.md),
  etc.)

- Added several `is_*` functions for graph and edge properties (e.g.,
  [`is_edge_loop()`](https://rich-iannone.github.io/DiagrammeR/reference/is_edge_loop.md),
  [`is_graph_dag()`](https://rich-iannone.github.io/DiagrammeR/reference/is_graph_dag.md),
  etc.)

- The
  [`mutate_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/mutate_node_attrs.md)
  and
  [`mutate_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/mutate_edge_attrs.md)
  functions now have simpler and more powerful interfaces for mutating
  node and edge attributes

- Graphs can be easily saved to disk (and read from disk) using the
  [`save_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/save_graph.md)
  and
  [`open_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/open_graph.md)
  functions

## DiagrammeR 0.9.0

CRAN release: 2017-01-03

- Modified basic structure of node and edge data frames such that ID
  values are automatically set as integer values

- Just as nodes do, edges now have edge ID values (they can be obtained
  using
  [`get_edge_ids()`](https://rich-iannone.github.io/DiagrammeR/reference/get_edge_ids.md)
  and they can be used directly in the
  [`select_edges_by_edge_id()`](https://rich-iannone.github.io/DiagrammeR/reference/select_edges_by_edge_id.md)
  function)

- When created, a graph object automatically generates a graph ID and
  graph name (which can be modified using
  [`set_graph_name()`](https://rich-iannone.github.io/DiagrammeR/reference/set_graph_name.md))

- So long as node `label` values are unique, they may now be used to
  compose edges using the
  [`add_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge.md)
  function with `use_labels = TRUE`

- Quickly and flexibly add color to nodes and edges using the
  [`colorize_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/colorize_node_attrs.md)
  and
  [`colorize_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/colorize_edge_attrs.md)
  functions

- Added functions to selectively modify existing node and edge
  attributes: `copy_[node/edge]_attrs()`, `drop_[node/edge]_attrs()`,
  `mutate_[node/edge]_attrs()`, `recode_[node/edge]_attrs()`,
  `rename_[node/edge]_attrs()`, and `rescale_[node/edge]_attrs()`.

- New node and edge attributes can now be easily added to a graph via a
  data frame using the
  [`join_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/join_node_attrs.md)
  and
  [`join_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/join_edge_attrs.md)
  functions

- Several graph generators are available for quickly adding graph
  primitives to a graph object
  ([`add_balanced_tree()`](https://rich-iannone.github.io/DiagrammeR/reference/add_balanced_tree.md),
  [`add_cycle()`](https://rich-iannone.github.io/DiagrammeR/reference/add_cycle.md),
  [`add_full_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/add_full_graph.md),
  [`add_path()`](https://rich-iannone.github.io/DiagrammeR/reference/add_path.md),
  [`add_prism()`](https://rich-iannone.github.io/DiagrammeR/reference/add_prism.md),
  and
  [`add_star()`](https://rich-iannone.github.io/DiagrammeR/reference/add_star.md))

- All traversal functions can now migrate numeric node or edge attribute
  values to the traversed edges (e.g.,
  [`trav_out_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out_edge.md),
  [`trav_in_node()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_node.md))
  by providing an attribute name to `copy_attrs_from()`; for those
  traversal functions where nodes or edges may receive multiple values,
  one can specify an aggregation type in their `agg` argument (e.g,.
  [`trav_in_node()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_node.md),
  [`trav_both_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_both_edge.md))

- Multiple conditions can be specified for all traversal types and for
  the
  [`select_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes.md)
  and
  [`select_edges()`](https://rich-iannone.github.io/DiagrammeR/reference/select_edges.md)
  functions, plus, they are much easier to write

- Added the `mk_cond()` helper function for creating conditions for any
  of the traversal functions (`trav_...()`), and, the
  [`select_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes.md)
  and
  [`select_edges()`](https://rich-iannone.github.io/DiagrammeR/reference/select_edges.md)
  functions; this helper allows for easier composition of
  selection/traversal conditions using variables and/or function calls

- With a selection of edges one can now use `select_rev_edges_ws()` to
  transform that selection to that of the selected edges’ reverse edges
  (where available); the option is there to add the reverse edges to the
  edge selection or to simply replace the current selection

- Caching attributes for later use has been made simpler with a
  collection of `cache_...()` functions (or, set the cache explicitly
  using
  [`set_cache()`](https://rich-iannone.github.io/DiagrammeR/reference/set_cache.md));
  get the graph’s cache using the
  [`get_cache()`](https://rich-iannone.github.io/DiagrammeR/reference/get_cache.md)
  function

- Added functions to allow for layout control of nodes
  ([`set_node_position()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_position.md),
  [`layout_nodes_w_string()`](https://rich-iannone.github.io/DiagrammeR/reference/layout_nodes_w_string.md),
  `nudge_node_positions()`)

- Added functions to convert **DiagrammeR** graphs to **igraph** graphs
  and vice versa
  ([`to_igraph()`](https://rich-iannone.github.io/DiagrammeR/reference/to_igraph.md),
  [`from_igraph()`](https://rich-iannone.github.io/DiagrammeR/reference/from_igraph.md))

- Now you can create a graph from an adjacency matrix
  ([`from_adj_matrix()`](https://rich-iannone.github.io/DiagrammeR/reference/from_adj_matrix.md))

- Added functions to get community membership with a variety of
  algorithms:
  [`get_cmty_edge_btwns()`](https://rich-iannone.github.io/DiagrammeR/reference/get_cmty_edge_btwns.md),
  [`get_cmty_fast_greedy()`](https://rich-iannone.github.io/DiagrammeR/reference/get_cmty_fast_greedy.md),
  [`get_cmty_l_eigenvec()`](https://rich-iannone.github.io/DiagrammeR/reference/get_cmty_l_eigenvec.md),
  [`get_cmty_louvain()`](https://rich-iannone.github.io/DiagrammeR/reference/get_cmty_louvain.md),
  and
  [`get_cmty_walktrap()`](https://rich-iannone.github.io/DiagrammeR/reference/get_cmty_walktrap.md).

- Added functions to determine similarity coefficient scores for graph
  nodes:
  [`get_dice_similarity()`](https://rich-iannone.github.io/DiagrammeR/reference/get_dice_similarity.md)
  and
  [`get_dice_similarity()`](https://rich-iannone.github.io/DiagrammeR/reference/get_dice_similarity.md).

- Constraint scores for nodes can now be determined using the
  `get_constraint()` function

- Functions for getting information on nodes neighbors have been added:
  [`get_nbrs()`](https://rich-iannone.github.io/DiagrammeR/reference/get_nbrs.md),
  [`get_non_nbrs()`](https://rich-iannone.github.io/DiagrammeR/reference/get_non_nbrs.md),
  [`get_similar_nbrs()`](https://rich-iannone.github.io/DiagrammeR/reference/get_similar_nbrs.md).

- Groups of nodes that are weakly or strongly connected components can
  be determined using the
  [`get_w_connected_cmpts()`](https://rich-iannone.github.io/DiagrammeR/reference/get_w_connected_cmpts.md)
  and
  [`get_s_connected_cmpts()`](https://rich-iannone.github.io/DiagrammeR/reference/get_s_connected_cmpts.md)
  functions

- Get articulation points (i.e., nodes that, when removed, disconnect
  the graph) with the
  [`get_articulation_points()`](https://rich-iannone.github.io/DiagrammeR/reference/get_articulation_points.md)
  function

- Obtain centrality measures for graph nodes using the
  [`get_closeness()`](https://rich-iannone.github.io/DiagrammeR/reference/get_closeness.md)
  and
  [`get_betweenness()`](https://rich-iannone.github.io/DiagrammeR/reference/get_betweenness.md)
  functions

- Get the minimum-spanning tree subgraph from a graph with weighted
  edges using the `get_min_spanning_tree()` function

- The edge direction may be reversed for an entire graph
  ([`rev_edge_dir()`](https://rich-iannone.github.io/DiagrammeR/reference/rev_edge_dir.md))
  or for part of a graph using an edge selection
  ([`rev_edge_dir_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/rev_edge_dir_ws.md))

- Depth-first search and breadth-first search algorithms are available
  in the
  [`do_dfs()`](https://rich-iannone.github.io/DiagrammeR/reference/do_dfs.md)
  and
  [`do_bfs()`](https://rich-iannone.github.io/DiagrammeR/reference/do_bfs.md)
  functions

- Degree data for plots can now be easily obtained using the
  [`get_degree_distribution()`](https://rich-iannone.github.io/DiagrammeR/reference/get_degree_distribution.md)
  and
  [`get_degree_histogram()`](https://rich-iannone.github.io/DiagrammeR/reference/get_degree_histogram.md)
  functions

- Global graph attributes are now more easily modifiable using a set of
  functions for this purpose:
  [`add_global_graph_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/add_global_graph_attrs.md),
  [`delete_global_graph_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_global_graph_attrs.md),
  [`get_global_graph_attr_info()`](https://rich-iannone.github.io/DiagrammeR/reference/get_global_graph_attr_info.md).

- Added option to display different text labels on nodes via the
  `display` node attribute; this is easily set with the
  [`set_node_attr_to_display()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attr_to_display.md)
  function

- Rewrote many graph functions (e.g. traversals) so that they are faster
  for very large graphs

- A log of all graph functions that directly modify the graph is now
  part of the graph object (`graph$graph_log`)

- Added functionality to automatically generate graph backups at every
  graph modification; this is in the form of RDS files deposited in a
  subdirectory (name is based on the graph ID) of the working directory;
  the option (`write_backups`, set to `FALSE` by default) is available
  in all functions that initialize a graph object
  ([`create_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/create_graph.md),
  `create_random_graph()`,
  [`from_igraph()`](https://rich-iannone.github.io/DiagrammeR/reference/from_igraph.md),
  [`from_adj_matrix()`](https://rich-iannone.github.io/DiagrammeR/reference/from_adj_matrix.md))

## DiagrammeR 0.8

CRAN release: 2015-10-08

- Revised many graph functions so they work better together

- Added many **testthat** tests to maintain the quality of the graph
  functions

- Added functions `create_random_graph()`,
  [`import_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/import_graph.md),
  [`combine_graphs()`](https://rich-iannone.github.io/DiagrammeR/reference/combine_graphs.md),
  `country_graph()` and `select_graph_from_series()`

- Added support for **visNetwork** graphs as a rendering option with
  [`render_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/render_graph.md)

## DiagrammeR 0.7

CRAN release: 2015-06-11

- Renamed functions `graphviz_graph()` and `graphviz_render()` to
  [`create_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/create_graph.md)
  and
  [`render_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/render_graph.md),
  respectively

- Removed function `graphviz_export()` (exporting now handled with
  [`render_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/render_graph.md))

- Added several new functions to inspect, analyze, and modify graphs:
  `display_graph_object()`, `node_info()`, `edge_info()`,
  `node_present()`, `edge_present()`, `get_nodes()`,
  [`get_edges()`](https://rich-iannone.github.io/DiagrammeR/reference/get_edges.md),
  [`get_predecessors()`](https://rich-iannone.github.io/DiagrammeR/reference/get_predecessors.md),
  [`get_successors()`](https://rich-iannone.github.io/DiagrammeR/reference/get_successors.md),
  `node_count()`, `edge_count()`,
  [`is_graph_empty()`](https://rich-iannone.github.io/DiagrammeR/reference/is_graph_empty.md),
  [`is_graph_directed()`](https://rich-iannone.github.io/DiagrammeR/reference/is_graph_directed.md),
  [`add_node()`](https://rich-iannone.github.io/DiagrammeR/reference/add_node.md),
  `add_edges()`,
  [`delete_node()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_node.md),
  [`delete_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_edge.md),
  `node_type()`, `edge_relationship()`, `create_series()`,
  `add_to_series()`, `remove_from_series()`, `graph_count()`,
  `subset_series()`, `trigger_script()`, `render_graph_from_series()`,
  and `series_info()`

## DiagrammeR 0.6

CRAN release: 2015-04-30

- Added several functions to work with graphs:
  [`create_node_df()`](https://rich-iannone.github.io/DiagrammeR/reference/create_node_df.md),
  `create_edges()`, `combine_nodes()`, `combine_edges()`,
  `scale_nodes()`, `scale_edges()`, `get_nodes()`, `node_info()`,
  `graphviz_graph()`, `graphviz_render()`, and `graphviz_export()`

- Removed the `graphviz_nodes_edges_df()` and `graphviz_single_df()`
  functions

## DiagrammeR 0.5

CRAN release: 2015-03-19

- Added support for subgraphs and Gantt charts in **mermaid** diagrams

- Added function `graphviz_nodes_edges_df()` for generating **Graphviz**
  **DOT** code that defines nodes and edges (and their attributes) from
  data in two data frames: one for nodes, the other for the edge
  operations

- Added function `graphviz_single_df()` for generating **Graphviz**
  **DOT** code from a single data frame

- Incorporated the new substitution operators `@_{...}` or `@^{...}` in
  `grViz` statements for subscripting and superscripting, respectively

## DiagrammeR 0.4

CRAN release: 2015-01-30

- Added support for substitution in **Graphviz** graph specifications

- Added support for **Graphviz** diagrams in the **Shiny** app

## DiagrammeR 0.3

- Added support for the **Graphviz** **neato**, **twopi**, and **circo**
  engines

## DiagrammeR 0.2

- Added the **viz.js** library to enable **Graphviz** support

## DiagrammeR 0.1

CRAN release: 2015-01-09

- Initial release

- Incorporated into the **htmlwidgets** framework

- Added basic **shiny** app
