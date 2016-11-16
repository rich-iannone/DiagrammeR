context("Creating a subgraph")

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
             2, 4, 5, 6, 4))

  graph <-
    create_graph(
      nodes_df = nodes,
      edges_df = edges)

  # Create a selection of nodes, stored within the
  # graph object
  graph <-
    graph %>%
    select_nodes("value > 3")

  # Create a subgraph based on the selection
  subgraph <- create_subgraph_ws(graph)

  # Expect that only those nodes with a value >3 are in the subgraph
  expect_true(
    all(c(1, 3, 5, 7, 8) %in% get_node_ids(subgraph)))

  # Expect only certain edges to be present in the subgraph
  expect_true(
    all(c("3 -> 1", "7 -> 3", "5 -> 8") %in%
          get_edges(subgraph, return_type = "vector")))

  # Expect an error when attempting to create a subgraph with
  # a graph without an active selection
  graph <- clear_selection(graph)
  expect_error(create_subgraph_ws(graph))
})
