# DiagrammeR 0.9

* Modified basic structure of node and edge data frames such that ID values are automatically set as integer values

* Just as nodes do, edges now have edge ID values (they can be obtained using `get_edge_ids()` and they can be used directly in the `select_edges_by_edge_id()` function)

* So long as node `label` values are unique, they may now be used to compose edges using the `add_edge()` function with `use_labels = TRUE`

* Quickly and flexibly add color to nodes and edges using the `colorize_node_attrs()` and `colorize_edge_attrs()` functions

* Added functions to selectively modify existing node and edge attributes (e.g., `copy_node_attrs()`, `mutate_edge_attrs()`, `recode_node_attrs()`, etc.); new node and edges attributes can also be added via a data frame using the `join_node_attrs()` and `join_edge_attrs()` functions

* Several graph generators are available for quickly adding graph primitives to a graph object (`add_balanced_tree()`, `add_cycle()`, `add_full_graph()`, `add_path()`, `add_prism()`, and `add_star()`)

* All traversal functions can now migrate numeric node or edge attribute values to the traversed edges (e.g., `trav_out_edge()`, `trav_in_node()`) by providing an attribute name to `copy_attrs_from`; for those traversal functions where nodes or edges may receive multiple values, one can specify an aggregation type in their `agg` argument (e.g,. `trav_in_node()`, `trav_both_edge()`)

* Multiple conditions can be specified for all traversal types, plus, they are much easier to write

* With a selection of edges one can now transform that selection to that of the selected edges' reverse edges (where available); the option is there to add the reverse edges to the edge selection or to simply replace the current selection

* Caching attributes for later use has been made simpler with a collection of `cache_...()` functions (or, set the cache explicitly using `set_cache()`); get the graph's cache using the `get_cache()` function

* Added functions to allow for layout control of nodes (`set_node_position()`, `layout_nodes_w_string()`, `nudge_node_positions()`)

* Added functions to convert **DiagrammeR** graphs to **igraph** graphs and vice versa (`to_igraph()`, `from_igraph()`)

* Now you can create a graph from an adjacency matrix (`from_adj_matrix()`)

* Added functions to get community membership with a variety of algorithms: `get_cmty_edge_btwns()`, `get_cmty_fast_greedy()`, `get_cmty_l_eigenvec()`, `get_cmty_louvain()`, and `get_cmty_walktrap()`.

* Added functions to determine similarity coefficient scores for graph nodes: `get_dice_similarity()` and `get_dice_similarity()`.

* Constraint scores for nodes can now be determined using the `get_constraint()` function

* Functions for getting information on nodes neighbors have been added: `get_nbrs()`, `get_non_nbrs()`, `get_similar_nbrs()`.

* Groups of nodes that are weakly or strongly connected components can be determined using the `get_w_connected_cmpts()` and `get_s_connected_cmpts()` functions

* The edge direction may be reversed for an entire graph (`rev_edge_dir()`) or for part of a graph using an edge selection (`rev_edge_dir_ws()`)  

* Depth-first search and breadth-first search algorithms are available in the `do_dfs()` and `do_bfs()` functions

* Degree data for plots can now be easily obtained using the `get_degree_distribution()` and `get_degree_histogram()` functions

* Global graph attributes are now more easily modifiable using a set of functions for this purpose: `add_global_graph_attrs()`, `clear_global_graph_attrs()`, `delete_global_graph_attrs()`, `get_global_graph_attrs()`, `set_global_graph_attrs()`

* Added option to display different text labels on nodes via the `display` node attribute; this is easily set with the `set_node_attr_to_display()` function

* Rewrote many graph functions (e.g. traversals) so that they are faster for very large graphs

* A log of all graph functions that directly modify the graph is now part of the graph object

* Added functionality to automatically generate graph backups at every graph modification; this is in the form of RDS files deposited in a subdirectory (name is based on the graph ID) of the working directory; the option (`write_backups`, set to `FALSE` by default) is available in all functions that initialize a graph object (`create_graph()`, `create_random_graph()`, `from_igraph()`, `from_adj_matrix()`)

# DiagrammeR 0.8

* Revised many graph functions so they work better together

* Added many **testthat** tests to maintain the quality of the graph functions

* Added functions `create_random_graph()`, `import_graph()`, `combine_graphs()`, `country_graph()` and `select_graph_from_series()`

* Added support for **visNetwork** graphs as a rendering option with `render_graph()`

# DiagrammeR 0.7

* Renamed functions `graphviz_graph()` and `graphviz_render()` to `create_graph()` and `render_graph()`, respectively

* Removed function `graphviz_export()` (exporting now handled with `render_graph()`)

* Added several new functions to inspect, analyze, and modify graphs: `display_graph_object()`, `node_info()`, `edge_info()`, `node_present()`, `edge_present()`, `get_nodes()`, `get_edges()`, `get_predecessors()`, `get_successors()`, `node_count()`, `edge_count()`, `is_graph_empty()`, `is_graph_directed()`, `add_node()`, `add_edges()`, `delete_node()`, `delete_edge()`, `node_type()`, `edge_relationship()`, `create_series()`, `add_to_series()`, `remove_from_series()`, `graph_count()`, `subset_series()`, `trigger_script()`, `render_graph_from_series()`, and `series_info()`

# DiagrammeR 0.6

* Added several functions to work with graphs: `create_nodes()`, `create_edges()`, `combine_nodes()`, `combine_edges()`, `scale_nodes()`, `scale_edges()`, `get_nodes()`, `node_info()`, `graphviz_graph()`, `graphviz_render()`, and `graphviz_export()`

* Removed the `graphviz_nodes_edges_df()` and `graphviz_single_df()` functions

# DiagrammeR 0.5

* Added support for subgraphs and Gantt charts in **mermaid** diagrams

* Added function `graphviz_nodes_edges_df()` for generating **Graphviz** **DOT** code that defines nodes and edges (and their attributes) from data in two data frames: one for nodes, the other for the edge operations

* Added function `graphviz_single_df()` for generating **Graphviz** **DOT** code from a single data frame

* Incorporated the new substitution operators `@_{...}` or `@^{...}` in `grViz` statements for subscripting and superscripting, respectively

# DiagrammeR 0.4

* Added support for substitution in **Graphviz** graph specifications

* Added support for **Graphviz** diagrams in the **Shiny** app

# DiagrammeR 0.3

* Added support for the **Graphviz** **neato**, **twopi**, and **circo** engines

# DiagrammeR 0.2

* Added the **viz.js** library to enable **Graphviz** support

# DiagrammeR 0.1

* Initial release

* Incorporated into the **htmlwidgets** framework

* Added basic **shiny** app
