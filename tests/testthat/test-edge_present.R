context("Edge presence")

test_that("a specified edge can be queried for presence in a graph", {

  # Create a simple graph
  nodes <- create_nodes(nodes = c("a", "b", "c", "d"))

  edges <- create_edges(from = c("a", "a", "c"),
                        to   = c("b", "c", "d"))

  graph <- create_graph(nodes_df = nodes,
                        edges_df = edges)

  # Expect that an edge between nodes "a" and "c" is in the graph
  expect_true(edge_present(graph = graph,
                           from = "a",
                           to = "c"))

  # Expect that an edge between nodes "a" and "d" is not in the graph
  expect_false(edge_present(graph = graph,
                            from = "a",
                            to = "d"))

  # Expect an error if requesting information on more than a
  # single node ID in either the "from" or the "to" lists
  expect_error(
    edge_present(graph = graph,
                 from = c("a", "c"),
                 to = c("b", "d"))
  )

  # Expect an error if either of the nodes ID values in either
  # the "from" or the "to" lists are not in the graph
  expect_error(
    edge_present(graph = graph,
                 from = "a",
                 to = "e")
  )
})
