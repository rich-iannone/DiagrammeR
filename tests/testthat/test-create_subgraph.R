# Creating a subgraph

test_that("a subgraph can be created and such an object is correct", {

  # Create a simple graph
  nodes <-
    create_node_df(
      n = 8,
      value = c(3.5, 2.6, 9.4, 2.7,
                5.2, 2.1, 4.8, 8.5))

  edges <-
    create_edge_df(
      from = c(1, 2, 3, 7, 5,
               5, 8, 6, 1, 3),
      to = c(4, 3, 1, 3, 8,
             2, 4, 5, 6, 4),
      value = c(1.3, 2.8, 3.5, 7.1,
                0.2, 2.1, 4.8, 8.5,
                3.5, 7.2))

  graph <-
    create_graph(
      nodes_df = nodes,
      edges_df = edges)

  # Create a selection of nodes, stored within the
  # graph object
  graph_ns <-
    graph %>%
    select_nodes(conditions = value > 3)

  # Create a subgraph based on the selection
  subgraph_ns <-
    transform_to_subgraph_ws(graph_ns)

  # Expect that only those nodes with a value >3 are in the subgraph
  expect_setequal(
    get_node_ids(subgraph_ns),
    c(1, 3, 5, 7, 8))

  # Expect only certain edges to be present in the subgraph
  expect_setequal(
    get_edges(
      graph = subgraph_ns,
      return_type = "vector"),
      c("3->1", "7->3", "5->8"))

  # Create a selection of edges, stored within the
  # graph object
  graph_es <-
    graph %>%
    select_edges(conditions = value > 4)

  # Create a subgraph based on the selection
  subgraph_es <-
    transform_to_subgraph_ws(graph_es)

  # Expect that only those edges with a value >4 are in the subgraph
  expect_contains(get_edge_ids(subgraph_es), c(4, 7, 8, 10))

  # Expect only certain edges to be present in the subgraph
  expect_contains(
    get_edges(
      graph = subgraph_es,
      return_type = "vector"),
    c("7->3", "8->4", "6->5", "3->4"))

  # Expect an error when attempting to create a subgraph with
  # a graph without an active selection
  graph <- clear_selection(graph)

  expect_snapshot(error = TRUE,
    transform_to_subgraph_ws(graph))
})
