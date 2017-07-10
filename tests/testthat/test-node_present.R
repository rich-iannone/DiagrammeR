context("Node presence")

test_that("a specified node can be queried for presence in a graph", {

  # Create a simple graph
  nodes <- create_node_df(n = 2)

  graph <-
    create_graph(nodes_df = nodes)

  # Expect that node with ID `1` is in the graph
  expect_true(
    node_present(
      graph = graph,
      node = 1))

  # Expect that node with ID `6` is not in the graph
  expect_false(
    node_present(
      graph = graph,
      node = 6))

  # Expect an error if requesting information on more
  # than a single node ID
  expect_error(
    node_present(
      graph = graph,
      node = c(1, 2)))
})
