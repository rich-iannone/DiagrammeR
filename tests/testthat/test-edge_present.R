context("Edge presence")

test_that("a specified edge can be queried for presence in a graph", {

  # Create a simple graph
  nodes <- create_node_df(n = 4)

  edges <-
    create_edge_df(
      from = c(1, 1, 3),
      to = c(2, 3, 4))

  graph <-
    create_graph(
      nodes_df = nodes,
      edges_df = edges)

  # Expect that an edge between nodes `1` and `3`
  # is in the graph
  expect_true(
    edge_present(
      graph = graph,
      from = 1,
      to = 3))

  # Expect that an edge between nodes `1` and `4`
  # is not in the graph
  expect_false(
    edge_present(
      graph = graph,
      from = 1,
      to = 4))

  # Expect an error if requesting information on
  # more than a single node ID in either the `from`
  # or the `to` lists
  expect_error(
    edge_present(
      graph = graph,
      from = c(1, 3),
      to = c(2, 4)))
})
