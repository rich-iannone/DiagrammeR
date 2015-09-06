context("Node presence")

test_that("a specified node can be queried for presence in a graph", {

  # Create a simple graph
  nodes <- create_nodes(nodes = c("a", "b"))

  graph <- create_graph(nodes_df = nodes)

  # Expect that node with ID 'a' is in the graph
  expect_true(node_present(graph = graph, "a"))

  # Expect that node with ID "A" is not in the graph
  expect_false(node_present(graph = graph, "A"))

  # Expect an error if requesting information on more
  # than a single node ID
  expect_error(
    node_present(graph = graph, node = c("a", "b"))
  )
})
