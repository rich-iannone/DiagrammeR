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
