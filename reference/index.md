# Package index

## External NDF/EDF

These functions create external node data frames (NDFs) and external
edge data frames (EDFs). These specialized tables can be used to create
graphs.

- [`create_node_df()`](https://rich-iannone.github.io/DiagrammeR/reference/create_node_df.md)
  : Create a node data frame
- [`create_edge_df()`](https://rich-iannone.github.io/DiagrammeR/reference/create_edge_df.md)
  : Create an edge data frame
- [`combine_ndfs()`](https://rich-iannone.github.io/DiagrammeR/reference/combine_ndfs.md)
  : Combine multiple node data frames
- [`combine_edfs()`](https://rich-iannone.github.io/DiagrammeR/reference/combine_edfs.md)
  : Combine multiple edge data frames into a single edge data frame

## Graph Creation, Rendering, and I/O

Graphs can be created, read in, written out, and displayed with these
functions.

- [`create_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/create_graph.md)
  : Create a graph object
- [`import_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/import_graph.md)
  : Import a graph from various graph formats
- [`from_adj_matrix()`](https://rich-iannone.github.io/DiagrammeR/reference/from_adj_matrix.md)
  : Create a graph using an adjacency matrix
- [`from_igraph()`](https://rich-iannone.github.io/DiagrammeR/reference/from_igraph.md)
  : Convert an igraph graph to a DiagrammeR one
- [`to_igraph()`](https://rich-iannone.github.io/DiagrammeR/reference/to_igraph.md)
  : Convert a DiagrammeR graph to an igraph one
- [`open_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/open_graph.md)
  : Read a graph or graph series from disk
- [`save_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/save_graph.md)
  : Save a graph or graph series to disk
- [`export_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/export_graph.md)
  : Export a graph to various image formats
- [`export_csv()`](https://rich-iannone.github.io/DiagrammeR/reference/export_csv.md)
  : Export a graph to CSV files
- [`render_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/render_graph.md)
  : Render the graph in various formats
- [`display_metagraph()`](https://rich-iannone.github.io/DiagrammeR/reference/display_metagraph.md)
  : Display a property graph's underlying model

## Node and Edge Selection

Graph nodes or edges can be selected with these functions. This is
useful because some of the Graph Modification functions (ending with
“\_ws”) and all of the Graph Traversal functions can make use of the
stored selections.

- [`select_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes.md)
  : Select nodes in a graph
- [`select_nodes_by_id()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes_by_id.md)
  : Select nodes in a graph by their ID values
- [`select_nodes_by_degree()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes_by_degree.md)
  : Select nodes in the graph based on their degree values
- [`select_nodes_in_neighborhood()`](https://rich-iannone.github.io/DiagrammeR/reference/select_nodes_in_neighborhood.md)
  : Select nodes based on a walk distance from a specified node
- [`select_last_nodes_created()`](https://rich-iannone.github.io/DiagrammeR/reference/select_last_nodes_created.md)
  : Select the last set of nodes created in a graph
- [`select_edges()`](https://rich-iannone.github.io/DiagrammeR/reference/select_edges.md)
  : Select edges in a graph
- [`select_edges_by_node_id()`](https://rich-iannone.github.io/DiagrammeR/reference/select_edges_by_node_id.md)
  : Select edges in a graph using node ID values
- [`select_edges_by_edge_id()`](https://rich-iannone.github.io/DiagrammeR/reference/select_edges_by_edge_id.md)
  : Select edges in a graph using edge ID values
- [`select_last_edges_created()`](https://rich-iannone.github.io/DiagrammeR/reference/select_last_edges_created.md)
  : Select the last set of edges created in a graph
- [`get_selection()`](https://rich-iannone.github.io/DiagrammeR/reference/get_selection.md)
  : Get the current selection available in a graph object
- [`deselect_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/deselect_nodes.md)
  : Deselect any selected nodes in a graph
- [`deselect_edges()`](https://rich-iannone.github.io/DiagrammeR/reference/deselect_edges.md)
  : Deselect any selected edges in a graph
- [`invert_selection()`](https://rich-iannone.github.io/DiagrammeR/reference/invert_selection.md)
  : Invert selection of nodes or edges in a graph
- [`clear_selection()`](https://rich-iannone.github.io/DiagrammeR/reference/clear_selection.md)
  : Clear an active selection of nodes or edges

## Graph Modification

These functions make changes to the graph. We can change many aspects of
the graph: the graph representation itself (e.g., adding or removing
nodes and edges), or, the attributes of the nodes and edges (in terms of
associated data and aesthetics).

- [`add_node()`](https://rich-iannone.github.io/DiagrammeR/reference/add_node.md)
  : Add a node to an existing graph object
- [`add_n_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/add_n_nodes.md)
  : Add one or several unconnected nodes to the graph
- [`add_n_nodes_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/add_n_nodes_ws.md)
  : Add a multiple of new nodes with edges to or from one or more
  selected nodes
- [`add_node_df()`](https://rich-iannone.github.io/DiagrammeR/reference/add_node_df.md)
  : Add nodes from a node data frame to an existing graph object
- [`add_n_node_clones()`](https://rich-iannone.github.io/DiagrammeR/reference/add_n_node_clones.md)
  : Add one or several clones of an existing node to the graph
- [`add_node_clones_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/add_node_clones_ws.md)
  : Add clones of a selection of nodes
- [`add_nodes_from_table()`](https://rich-iannone.github.io/DiagrammeR/reference/add_nodes_from_table.md)
  : Add nodes and attributes to graph from a table
- [`add_nodes_from_df_cols()`](https://rich-iannone.github.io/DiagrammeR/reference/add_nodes_from_df_cols.md)
  : Add nodes from distinct values in data frame columns
- [`add_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge.md)
  : Add an edge between nodes in a graph object
- [`add_edge_df()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge_df.md)
  : Add edges from an edge data frame to an existing graph object
- [`add_edges_w_string()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edges_w_string.md)
  : Add one or more edges using a text string
- [`add_edge_clone()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge_clone.md)
  : Add a clone of an existing edge to the graph
- [`add_forward_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/add_forward_edges_ws.md)
  : Add new edges with identical definitions as with a selection of
  edges
- [`add_reverse_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/add_reverse_edges_ws.md)
  : Add new edges in the opposite directions of a selection of edges
- [`fully_connect_nodes_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/fully_connect_nodes_ws.md)
  : Fully connect all nodes in a selection of nodes
- [`add_edges_from_table()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edges_from_table.md)
  : Add edges and attributes to graph from a table
- [`add_path()`](https://rich-iannone.github.io/DiagrammeR/reference/add_path.md)
  : Add a path of nodes to the graph
- [`add_cycle()`](https://rich-iannone.github.io/DiagrammeR/reference/add_cycle.md)
  : Add a cycle of nodes to the graph
- [`add_balanced_tree()`](https://rich-iannone.github.io/DiagrammeR/reference/add_balanced_tree.md)
  : Add a balanced tree to the graph
- [`add_star()`](https://rich-iannone.github.io/DiagrammeR/reference/add_star.md)
  : Add a star of nodes to the graph
- [`add_prism()`](https://rich-iannone.github.io/DiagrammeR/reference/add_prism.md)
  : Add a prism of nodes to the graph
- [`add_full_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/add_full_graph.md)
  : Add a fully connected graph
- [`add_grid_2d()`](https://rich-iannone.github.io/DiagrammeR/reference/add_grid_2d.md)
  : Add a 2D grid of nodes to the graph
- [`add_grid_3d()`](https://rich-iannone.github.io/DiagrammeR/reference/add_grid_3d.md)
  : Add a 3D grid of nodes to the graph
- [`add_gnm_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/add_gnm_graph.md)
  : Add a G(n, m) Erdos-Renyi graph
- [`add_gnp_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/add_gnp_graph.md)
  : Add a G(n, p) Erdos-Renyi graph
- [`add_pa_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/add_pa_graph.md)
  : Add a preferential attachment graph
- [`add_growing_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/add_growing_graph.md)
  : Create a random growing graph with m edges added per step
- [`add_smallworld_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/add_smallworld_graph.md)
  : Add a Watts-Strogatz small-world graph
- [`add_islands_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/add_islands_graph.md)
  : Create a random islands graph with edges between the islands
- [`delete_node()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_node.md)
  : Delete a node from an existing graph object
- [`delete_nodes_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_nodes_ws.md)
  : Delete all nodes in a node selection
- [`delete_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_edge.md)
  : Delete an edge from an existing graph object
- [`delete_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_edges_ws.md)
  : Delete all selected edges in an edge selection
- [`delete_loop_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_loop_edges_ws.md)
  : Delete all loop edges associated with a selection of nodes
- [`fully_disconnect_nodes_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/fully_disconnect_nodes_ws.md)
  : Fully disconnect all nodes in a selection of nodes
- [`rev_edge_dir()`](https://rich-iannone.github.io/DiagrammeR/reference/rev_edge_dir.md)
  : Reverse the direction of all edges in a graph
- [`rev_edge_dir_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/rev_edge_dir_ws.md)
  : Reverse the direction of selected edges in a graph using an edge
  selection
- [`set_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attrs.md)
  : Set node attribute values
- [`set_node_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attrs_ws.md)
  : Set node attributes with a node selection
- [`join_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/join_node_attrs.md)
  : Join new node attribute values using a data frame
- [`rescale_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/rescale_node_attrs.md)
  : Rescale numeric node attribute values
- [`copy_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/copy_node_attrs.md)
  : Copy a node attribute column and set the name
- [`rename_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/rename_node_attrs.md)
  : Rename a node attribute
- [`drop_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/drop_node_attrs.md)
  : Drop a node attribute column
- [`mutate_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/mutate_node_attrs.md)
  : Mutate a set of node attribute values
- [`mutate_node_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/mutate_node_attrs_ws.md)
  : Mutate node attribute values for a selection of nodes
- [`colorize_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/colorize_node_attrs.md)
  : Apply colors based on node attribute values
- [`recode_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/recode_node_attrs.md)
  : Recode a set of node attribute values
- [`set_node_position()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_position.md)
  : Apply a layout position to a single node
- [`layout_nodes_w_string()`](https://rich-iannone.github.io/DiagrammeR/reference/layout_nodes_w_string.md)
  : Layout nodes using a text-based schematic
- [`nudge_node_positions_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/nudge_node_positions_ws.md)
  : Move layout positions of a selection of nodes
- [`set_node_attr_w_fcn()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attr_w_fcn.md)
  : Set node attribute values with a graph function
- [`set_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs.md)
  : Set edge attribute values
- [`set_edge_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs_ws.md)
  : Set edge attributes with an edge selection
- [`join_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/join_edge_attrs.md)
  : Join new edge attribute values using a data frame
- [`rescale_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/rescale_edge_attrs.md)
  : Rescale numeric edge attribute values
- [`copy_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/copy_edge_attrs.md)
  : Copy an edge attribute column and set the name
- [`rename_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/rename_edge_attrs.md)
  : Rename an edge attribute
- [`drop_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/drop_edge_attrs.md)
  : Drop an edge attribute column
- [`mutate_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/mutate_edge_attrs.md)
  : Mutate a set of edge attribute values
- [`mutate_edge_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/mutate_edge_attrs_ws.md)
  : Mutate edge attribute values for a selection of edges
- [`colorize_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/colorize_edge_attrs.md)
  : Apply colors based on edge attribute values
- [`recode_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/recode_edge_attrs.md)
  : Recode a set of edge attribute values
- [`node_data()`](https://rich-iannone.github.io/DiagrammeR/reference/node_data.md)
  : Insert node data attributes during node creation
- [`node_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/node_aes.md)
  : Insert node aesthetic attributes during node creation
- [`edge_data()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_data.md)
  : Insert edge data attributes during edge creation
- [`edge_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_aes.md)
  : Insert edge aesthetic attributes during edge creation

## Graph Transformation

Wholesale changes to a graph can be made with any of these functions.
Combine two graphs, transform them to different representations, even
change whether the graph is directed or undirected.

- [`combine_graphs()`](https://rich-iannone.github.io/DiagrammeR/reference/combine_graphs.md)
  : Combine two graphs into a single graph
- [`transform_to_subgraph_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/transform_to_subgraph_ws.md)
  : Create a subgraph using a node or edge selection
- [`transform_to_complement_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/transform_to_complement_graph.md)
  : Create a complement of a graph
- [`transform_to_min_spanning_tree()`](https://rich-iannone.github.io/DiagrammeR/reference/transform_to_min_spanning_tree.md)
  : Get a minimum spanning tree subgraph
- [`set_graph_undirected()`](https://rich-iannone.github.io/DiagrammeR/reference/set_graph_undirected.md)
  : Convert a directed graph to an undirected graph
- [`set_graph_directed()`](https://rich-iannone.github.io/DiagrammeR/reference/set_graph_directed.md)
  : Convert an undirected graph to a directed graph

## Graph Inspection

Sometimes, you’ll need to gather information about the working graph.
This collection of functions allows for extraction of graph components
(e.g., ndfs and edfs), attribute information, counts, node and edge
properties, and more.

- [`get_node_df()`](https://rich-iannone.github.io/DiagrammeR/reference/get_node_df.md)
  : Get a node data frame from a graph
- [`get_node_df_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/get_node_df_ws.md)
  : Get the graph's ndf filtered by a selection of nodes
- [`get_node_info()`](https://rich-iannone.github.io/DiagrammeR/reference/get_node_info.md)
  : Get detailed information on nodes
- [`get_last_nodes_created()`](https://rich-iannone.github.io/DiagrammeR/reference/get_last_nodes_created.md)
  : Get the last set of nodes created in a graph
- [`get_node_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/get_node_attrs.md)
  : Get node attribute values
- [`get_node_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/get_node_attrs_ws.md)
  : Get node attribute values from a selection of nodes
- [`get_degree_distribution()`](https://rich-iannone.github.io/DiagrammeR/reference/get_degree_distribution.md)
  : Get total degree distribution data for a graph
- [`get_degree_histogram()`](https://rich-iannone.github.io/DiagrammeR/reference/get_degree_histogram.md)
  : Get histogram data for a graph's degree frequency
- [`get_edge_df()`](https://rich-iannone.github.io/DiagrammeR/reference/get_edge_df.md)
  : Get an edge data frame from a graph
- [`get_edge_df_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/get_edge_df_ws.md)
  : Get the graph's edf filtered by a selection of edges
- [`get_edge_info()`](https://rich-iannone.github.io/DiagrammeR/reference/get_edge_info.md)
  : Get detailed information on edges
- [`get_last_edges_created()`](https://rich-iannone.github.io/DiagrammeR/reference/get_last_edges_created.md)
  : Get the last set of edges created in a graph
- [`get_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/get_edge_attrs.md)
  : Get edge attribute values
- [`get_edge_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/get_edge_attrs_ws.md)
  : Get edge attribute values from a selection of edges
- [`get_node_ids()`](https://rich-iannone.github.io/DiagrammeR/reference/get_node_ids.md)
  : Get a vector of node ID values
- [`get_nbrs()`](https://rich-iannone.github.io/DiagrammeR/reference/get_nbrs.md)
  : Get all neighbors of one or more nodes
- [`get_common_nbrs()`](https://rich-iannone.github.io/DiagrammeR/reference/get_common_nbrs.md)
  : Get all common neighbors between two or more nodes
- [`get_non_nbrs()`](https://rich-iannone.github.io/DiagrammeR/reference/get_non_nbrs.md)
  : Get non-neighbors of a node in a graph
- [`get_similar_nbrs()`](https://rich-iannone.github.io/DiagrammeR/reference/get_similar_nbrs.md)
  : Get neighboring nodes based on node attribute similarity
- [`get_predecessors()`](https://rich-iannone.github.io/DiagrammeR/reference/get_predecessors.md)
  : Get node IDs for predecessor nodes to the specified node
- [`get_successors()`](https://rich-iannone.github.io/DiagrammeR/reference/get_successors.md)
  : Get node IDs for successor nodes to the specified node
- [`get_all_connected_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/get_all_connected_nodes.md)
  : Get all nodes connected to a specified node
- [`get_articulation_points()`](https://rich-iannone.github.io/DiagrammeR/reference/get_articulation_points.md)
  : Get articulation points
- [`get_periphery()`](https://rich-iannone.github.io/DiagrammeR/reference/get_periphery.md)
  : Get nodes that form the graph periphery
- [`get_edge_ids()`](https://rich-iannone.github.io/DiagrammeR/reference/get_edge_ids.md)
  : Get a vector of edge ID values
- [`get_edges()`](https://rich-iannone.github.io/DiagrammeR/reference/get_edges.md)
  : Get node IDs associated with edges
- [`get_paths()`](https://rich-iannone.github.io/DiagrammeR/reference/get_paths.md)
  : Get paths from a specified node in a directed graph
- [`get_graph_info()`](https://rich-iannone.github.io/DiagrammeR/reference/get_graph_info.md)
  : Get metrics for a graph
- [`do_dfs()`](https://rich-iannone.github.io/DiagrammeR/reference/do_dfs.md)
  : Use the depth-first search (dfs) algorithm
- [`do_bfs()`](https://rich-iannone.github.io/DiagrammeR/reference/do_bfs.md)
  : Use the breadth-first search (bfs) algorithm
- [`count_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/count_nodes.md)
  : Get a count of all nodes
- [`count_edges()`](https://rich-iannone.github.io/DiagrammeR/reference/count_edges.md)
  : Get a count of all edges
- [`count_loop_edges()`](https://rich-iannone.github.io/DiagrammeR/reference/count_loop_edges.md)
  : Get count of all loop edges
- [`count_mutual_node_pairs()`](https://rich-iannone.github.io/DiagrammeR/reference/count_mutual_node_pairs.md)
  : Get the number of mutually-connected node pairs
- [`count_asymmetric_node_pairs()`](https://rich-iannone.github.io/DiagrammeR/reference/count_asymmetric_node_pairs.md)
  : Get the number of asymmetrically-connected node pairs
- [`count_automorphisms()`](https://rich-iannone.github.io/DiagrammeR/reference/count_automorphisms.md)
  : Get the number of automorphisms
- [`count_unconnected_nodes()`](https://rich-iannone.github.io/DiagrammeR/reference/count_unconnected_nodes.md)
  : Get count of all unconnected nodes
- [`count_unconnected_node_pairs()`](https://rich-iannone.github.io/DiagrammeR/reference/count_unconnected_node_pairs.md)
  : Get the number of unconnected node pairs
- [`count_w_connected_cmpts()`](https://rich-iannone.github.io/DiagrammeR/reference/count_w_connected_cmpts.md)
  : Get the number of weakly-connected components
- [`count_s_connected_cmpts()`](https://rich-iannone.github.io/DiagrammeR/reference/count_s_connected_cmpts.md)
  : Get the number of strongly-connected components
- [`get_multiedge_count()`](https://rich-iannone.github.io/DiagrammeR/reference/get_multiedge_count.md)
  : Get the count of multiple edges
- [`get_edge_count_w_multiedge()`](https://rich-iannone.github.io/DiagrammeR/reference/get_edge_count_w_multiedge.md)
  : Get count of edge definitions where multiple edges occur
- [`get_agg_degree_in()`](https://rich-iannone.github.io/DiagrammeR/reference/get_agg_degree_in.md)
  : Get an aggregate value from the indegree of nodes
- [`get_agg_degree_out()`](https://rich-iannone.github.io/DiagrammeR/reference/get_agg_degree_out.md)
  : Get an aggregate value from the outdegree of nodes
- [`get_agg_degree_total()`](https://rich-iannone.github.io/DiagrammeR/reference/get_agg_degree_total.md)
  : Get an aggregate value from the total degree of nodes
- [`get_mean_distance()`](https://rich-iannone.github.io/DiagrammeR/reference/get_mean_distance.md)
  : Get the mean distance
- [`get_min_eccentricity()`](https://rich-iannone.github.io/DiagrammeR/reference/get_min_eccentricity.md)
  : Get the minimum graph eccentricity
- [`get_max_eccentricity()`](https://rich-iannone.github.io/DiagrammeR/reference/get_max_eccentricity.md)
  : Get the maximum graph eccentricity
- [`get_reciprocity()`](https://rich-iannone.github.io/DiagrammeR/reference/get_reciprocity.md)
  : Get the graph reciprocity
- [`get_girth()`](https://rich-iannone.github.io/DiagrammeR/reference/get_girth.md)
  : Get the graph girth
- [`get_adhesion()`](https://rich-iannone.github.io/DiagrammeR/reference/get_adhesion.md)
  : Get graph adhesion
- [`get_min_cut_between()`](https://rich-iannone.github.io/DiagrammeR/reference/get_min_cut_between.md)
  : Get the minimum cut between source and sink nodes
- [`get_degree_in()`](https://rich-iannone.github.io/DiagrammeR/reference/get_degree_in.md)
  : Get indegree values for all nodes
- [`get_degree_out()`](https://rich-iannone.github.io/DiagrammeR/reference/get_degree_out.md)
  : Get outdegree values for all nodes
- [`get_degree_total()`](https://rich-iannone.github.io/DiagrammeR/reference/get_degree_total.md)
  : Get total degree values for all nodes
- [`get_betweenness()`](https://rich-iannone.github.io/DiagrammeR/reference/get_betweenness.md)
  : Get betweenness centrality scores
- [`get_closeness()`](https://rich-iannone.github.io/DiagrammeR/reference/get_closeness.md)
  : Get closeness centrality values
- [`get_pagerank()`](https://rich-iannone.github.io/DiagrammeR/reference/get_pagerank.md)
  : Get the PageRank values for all nodes
- [`get_alpha_centrality()`](https://rich-iannone.github.io/DiagrammeR/reference/get_alpha_centrality.md)
  : Get the alpha centrality for all nodes
- [`get_eigen_centrality()`](https://rich-iannone.github.io/DiagrammeR/reference/get_eigen_centrality.md)
  : Get the eigen centrality for all nodes
- [`get_authority_centrality()`](https://rich-iannone.github.io/DiagrammeR/reference/get_authority_centrality.md)
  : Get the authority scores for all nodes
- [`get_leverage_centrality()`](https://rich-iannone.github.io/DiagrammeR/reference/get_leverage_centrality.md)
  : Get leverage centrality
- [`get_radiality()`](https://rich-iannone.github.io/DiagrammeR/reference/get_radiality.md)
  : Get radiality centrality scores
- [`get_eccentricity()`](https://rich-iannone.github.io/DiagrammeR/reference/get_eccentricity.md)
  : Get node eccentricities
- [`get_coreness()`](https://rich-iannone.github.io/DiagrammeR/reference/get_coreness.md)
  : Get coreness values for graph nodes
- [`get_closeness_vitality()`](https://rich-iannone.github.io/DiagrammeR/reference/get_closeness_vitality.md)
  : Get closeness vitality
- [`get_dice_similarity()`](https://rich-iannone.github.io/DiagrammeR/reference/get_dice_similarity.md)
  : Get Dice similarity coefficient scores
- [`get_jaccard_similarity()`](https://rich-iannone.github.io/DiagrammeR/reference/get_jaccard_similarity.md)
  : Get Jaccard similarity coefficient scores
- [`get_w_connected_cmpts()`](https://rich-iannone.github.io/DiagrammeR/reference/get_w_connected_cmpts.md)
  : Get all nodes associated with connected components
- [`get_s_connected_cmpts()`](https://rich-iannone.github.io/DiagrammeR/reference/get_s_connected_cmpts.md)
  : Get nodes within strongly connected components
- [`get_cmty_edge_btwns()`](https://rich-iannone.github.io/DiagrammeR/reference/get_cmty_edge_btwns.md)
  : Get community membership by edge betweenness
- [`get_cmty_walktrap()`](https://rich-iannone.github.io/DiagrammeR/reference/get_cmty_walktrap.md)
  : Get community membership using the Walktrap method
- [`get_cmty_louvain()`](https://rich-iannone.github.io/DiagrammeR/reference/get_cmty_louvain.md)
  : Get community membership by Louvain optimization
- [`get_cmty_l_eigenvec()`](https://rich-iannone.github.io/DiagrammeR/reference/get_cmty_l_eigenvec.md)
  : Get community membership by leading eigenvector
- [`get_cmty_fast_greedy()`](https://rich-iannone.github.io/DiagrammeR/reference/get_cmty_fast_greedy.md)
  : Get community membership by modularity optimization
- [`is_node_present()`](https://rich-iannone.github.io/DiagrammeR/reference/is_node_present.md)
  : Determine whether a specified node is present
- [`is_edge_present()`](https://rich-iannone.github.io/DiagrammeR/reference/is_edge_present.md)
  : Determine whether a specified edge is present
- [`is_edge_loop()`](https://rich-iannone.github.io/DiagrammeR/reference/is_edge_loop.md)
  : Is the edge a loop edge?
- [`is_edge_multiple()`](https://rich-iannone.github.io/DiagrammeR/reference/is_edge_multiple.md)
  : Is the edge a multiple edge?
- [`is_edge_mutual()`](https://rich-iannone.github.io/DiagrammeR/reference/is_edge_mutual.md)
  : Is the edge mutual with another edge?
- [`is_graph_empty()`](https://rich-iannone.github.io/DiagrammeR/reference/is_graph_empty.md)
  : Is the graph empty?
- [`is_graph_directed()`](https://rich-iannone.github.io/DiagrammeR/reference/is_graph_directed.md)
  : Is the graph a directed graph?
- [`is_graph_undirected()`](https://rich-iannone.github.io/DiagrammeR/reference/is_graph_undirected.md)
  : Is the graph an undirected graph?
- [`is_graph_simple()`](https://rich-iannone.github.io/DiagrammeR/reference/is_graph_simple.md)
  : Is the graph a simple graph?
- [`is_graph_weighted()`](https://rich-iannone.github.io/DiagrammeR/reference/is_graph_weighted.md)
  : Is the graph a weighted graph?
- [`is_graph_connected()`](https://rich-iannone.github.io/DiagrammeR/reference/is_graph_connected.md)
  : Is the graph a connected graph?
- [`is_graph_dag()`](https://rich-iannone.github.io/DiagrammeR/reference/is_graph_dag.md)
  : Is the graph a directed acyclic graph?
- [`is_property_graph()`](https://rich-iannone.github.io/DiagrammeR/reference/is_property_graph.md)
  : Is the graph a property graph?

## Value Caching

Think of caching as a way to temporarily store a vector of values within
the graph. Why do this? Well, one could extract node or edge data as
vector, cache that in the graph, and then later use it as input in
another function, all in the same functional pipeline.

- [`set_cache()`](https://rich-iannone.github.io/DiagrammeR/reference/set_cache.md)
  : Cache a vector in the graph
- [`get_cache()`](https://rich-iannone.github.io/DiagrammeR/reference/get_cache.md)
  : Get a cached vector from a graph object
- [`delete_cache()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_cache.md)
  : Delete vectors cached in a graph object

## Graph Traversal

Traversing the graph is useful if you have a defined graph model and
you’d like to explore the connections and relationships between nodes
and edges. Traversals effectively modify a selection of nodes or edges.
With the modified selection, we can extract related attributes and work
with them.

- [`trav_out()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out.md)
  : Traverse from one or more selected nodes onto adjacent, outward
  nodes
- [`trav_in()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in.md)
  : Traverse from one or more selected nodes onto adjacent, inward nodes
- [`trav_both()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_both.md)
  : Traverse from one or more selected nodes onto neighboring nodes
- [`trav_out_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out_edge.md)
  : Traverse from one or more selected nodes onto adjacent, outward
  edges
- [`trav_in_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_edge.md)
  : Traverse from one or more selected nodes onto adjacent, inward edges
- [`trav_both_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_both_edge.md)
  : Traverse from one or more selected nodes onto adjacent edges
- [`trav_out_node()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out_node.md)
  : Traverse from one or more selected edges onto adjacent, outward
  nodes
- [`trav_in_node()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_node.md)
  : Traverse from one or more selected edges onto adjacent, inward nodes
- [`trav_out_until()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_out_until.md)
  : Traverse outward node-by-node until stopping conditions are met
- [`trav_in_until()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_in_until.md)
  : Traverse inward node-by-node until stopping conditions are met
- [`trav_reverse_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/trav_reverse_edge.md)
  : Traverse to any reverse edges

## Graph Series

A series of graphs can be occasionally useful. It’s a convenient
container for a set of closely related graphs (for example, a network
that changes with time).

- [`create_graph_series()`](https://rich-iannone.github.io/DiagrammeR/reference/create_graph_series.md)
  : Create a graph series object
- [`add_graph_to_graph_series()`](https://rich-iannone.github.io/DiagrammeR/reference/add_graph_to_graph_series.md)
  : Add graph object to a graph series object
- [`remove_graph_from_graph_series()`](https://rich-iannone.github.io/DiagrammeR/reference/remove_graph_from_graph_series.md)
  : Remove a graph from a graph series
- [`filter_graph_series()`](https://rich-iannone.github.io/DiagrammeR/reference/filter_graph_series.md)
  : Subset a graph series object
- [`count_graphs_in_graph_series()`](https://rich-iannone.github.io/DiagrammeR/reference/count_graphs_in_graph_series.md)
  : Count graphs in a graph series object
- [`get_graph_from_graph_series()`](https://rich-iannone.github.io/DiagrammeR/reference/get_graph_from_graph_series.md)
  : Get a graph available in a series
- [`get_graph_series_info()`](https://rich-iannone.github.io/DiagrammeR/reference/get_graph_series_info.md)
  : Get information on a graph series
- [`render_graph_from_graph_series()`](https://rich-iannone.github.io/DiagrammeR/reference/render_graph_from_graph_series.md)
  : Render a graph available in a series

## Graph Metadata

Graph metadata consist of the attributes that don’t necessarily belong
to the individual nodes or edges, but, may affect their appearance when
the graph is rendered. These metadata can also apply directly to the
graph object itself.

- [`add_global_graph_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/add_global_graph_attrs.md)
  : Add one or more global graph attributes
- [`delete_global_graph_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_global_graph_attrs.md)
  : Delete one of the global graph attributes stored within a graph
  object
- [`get_global_graph_attr_info()`](https://rich-iannone.github.io/DiagrammeR/reference/get_global_graph_attr_info.md)
  : Get global graph attributes
- [`set_graph_name()`](https://rich-iannone.github.io/DiagrammeR/reference/set_graph_name.md)
  : Set graph name
- [`set_graph_time()`](https://rich-iannone.github.io/DiagrammeR/reference/set_graph_time.md)
  : Set graph date-time and timezone
- [`set_node_attr_to_display()`](https://rich-iannone.github.io/DiagrammeR/reference/set_node_attr_to_display.md)
  : Set the node attribute values to be rendered
- [`set_edge_attr_to_display()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attr_to_display.md)
  : Set the edge attribute values to be rendered
- [`get_graph_name()`](https://rich-iannone.github.io/DiagrammeR/reference/get_graph_name.md)
  : Get graph name
- [`get_graph_time()`](https://rich-iannone.github.io/DiagrammeR/reference/get_graph_time.md)
  : Get the graph date-time or timezone
- [`get_graph_log()`](https://rich-iannone.github.io/DiagrammeR/reference/get_graph_log.md)
  : Get the graph log information

## Graph Actions

Take action! Think of graph actions as little subroutines that execute
at every meaningful change of the graph.

- [`add_graph_action()`](https://rich-iannone.github.io/DiagrammeR/reference/add_graph_action.md)
  : Add a graph action for execution at every transform
- [`delete_graph_actions()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_graph_actions.md)
  : Delete one or more graph actions stored within a graph object
- [`get_graph_actions()`](https://rich-iannone.github.io/DiagrammeR/reference/get_graph_actions.md)
  : Get information on any available graph actions
- [`reorder_graph_actions()`](https://rich-iannone.github.io/DiagrammeR/reference/reorder_graph_actions.md)
  : Reorder the stored series of graph actions
- [`trigger_graph_actions()`](https://rich-iannone.github.io/DiagrammeR/reference/trigger_graph_actions.md)
  : Trigger the execution of a series of graph actions

## Graphviz, mermaid, and visNetwork

- [`DiagrammeR()`](https://rich-iannone.github.io/DiagrammeR/reference/DiagrammeR.md)
  : R + mermaid.js
- [`grViz()`](https://rich-iannone.github.io/DiagrammeR/reference/grViz.md)
  : R + viz.js
- [`generate_dot()`](https://rich-iannone.github.io/DiagrammeR/reference/generate_dot.md)
  : Generate DOT code using a graph object
- [`replace_in_spec()`](https://rich-iannone.github.io/DiagrammeR/reference/replace_in_spec.md)
  : Razor-like template for diagram specification
- [`mermaid()`](https://rich-iannone.github.io/DiagrammeR/reference/mermaid.md)
  : R + mermaid.js
- [`visnetwork()`](https://rich-iannone.github.io/DiagrammeR/reference/visnetwork.md)
  : Render graph with visNetwork

## Shiny

- [`renderDiagrammeR()`](https://rich-iannone.github.io/DiagrammeR/reference/renderDiagrammeR.md)
  : Widget render function for use in Shiny
- [`DiagrammeROutput()`](https://rich-iannone.github.io/DiagrammeR/reference/DiagrammeROutput.md)
  : Widget output function for use in Shiny
- [`renderGrViz()`](https://rich-iannone.github.io/DiagrammeR/reference/renderGrViz.md)
  : Widget render function for use in Shiny
- [`grVizOutput()`](https://rich-iannone.github.io/DiagrammeR/reference/grVizOutput.md)
  : Widget output function for use in Shiny

## Datasets

- [`node_list_1`](https://rich-iannone.github.io/DiagrammeR/reference/node_list_1.md)
  : Node list - Version 1.
- [`edge_list_1`](https://rich-iannone.github.io/DiagrammeR/reference/edge_list_1.md)
  : Edge list - Version 1.
- [`node_list_2`](https://rich-iannone.github.io/DiagrammeR/reference/node_list_2.md)
  : Node list - Version 2.
- [`edge_list_2`](https://rich-iannone.github.io/DiagrammeR/reference/edge_list_2.md)
  : Edge list - Version 2.
- [`currencies`](https://rich-iannone.github.io/DiagrammeR/reference/currencies.md)
  : ISO-4217 currency data.
- [`usd_exchange_rates`](https://rich-iannone.github.io/DiagrammeR/reference/usd_exchange_rates.md)
  : US Dollar exchange rates.
