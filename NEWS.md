# DiagrammeR 0.7

* Renamed functions `graphviz_graph` and `graphviz_render` to `create_graph` and `render_graph`, respectively

* Removed function `graphviz_export` (exporting now handled with `render_graph`)

* Added several new functions to inspect, analyze, and modify graphs: `display_graph_object`, `node_info`, `edge_info`, `node_present`, `edge_present`, `get_nodes`, `get_edges`, `get_predecessors`, `get_successors`, `node_count`, `edge_count`, `is_graph_empty`, `is_graph_directed`, `add_node`, `add_edges`, `delete_node`, `delete_edge`, `node_type`, `edge_relationship`, `create_series`, `add_to_series`, `remove_from_series`, `graph_count`, `subset_series`, `trigger_script`, `render_graph_from_series`, and `series_info`

# DiagrammeR 0.6

* Added several functions to work with graphs: `create_nodes`, `create_edges`, `combine_nodes`, `combine_edges`, `scale_nodes`, `scale_edges`, `get_nodes`, `node_info`, `graphviz_graph`, `graphviz_render`, and `graphviz_export`

* Removed the `graphviz_nodes_edges_df` and `graphviz_single_df` functions

# DiagrammeR 0.5

* Added support for subgraphs and Gantt charts in **mermaid** diagrams

* Added function `graphviz_nodes_edges_df` for generating **Graphviz** **DOT** code that defines nodes and edges (and their attributes) from data in two data frames: one for nodes, the other for the edge operations

* Added function `graphviz_single_df` for generating **Graphviz** **DOT** code from a single data frame

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
