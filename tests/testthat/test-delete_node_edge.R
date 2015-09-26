context("Deletion of nodes and edges from an existing graph")

test_that("nodes and edges can be deleted from a graph", {

  # Create an empty graph
  graph <- create_graph()

  # Add four nodes
  graph <- add_node(graph, node = "a")
  graph <- add_node(graph, node = "b")
  graph <- add_node(graph, node = "c")
  graph <- add_node(graph, node = "d")

  # Add edges
  graph <- add_edges(graph, from = "a", to = "b",
                     rel = "to_get")

  graph <-
    add_edges(graph,
              from = c("a", "a"),
              to = c("c", "d"),
              rel = "received_from")

  # Get the graph's nodes
  graph_nodes <- get_nodes(graph)

  # Get the graph's edges
  graph_edges <- get_edges(graph)

  # Remove a node (removing a node removes its edges)
  graph <- delete_node(graph, node = "d")

  # Get the graph's nodes after calling 'delete_node'
  graph_nodes_delete_node <- get_nodes(graph)

  # Get the graph's edges after calling 'delete_node'
  graph_edges_delete_node <- get_edges(graph)

  # Expect that the number of nodes will be decreased
  expect_less_than(length(graph_nodes_delete_node),
                   length(graph_nodes))

  # Expect that the number of edges will be decreased
  expect_less_than(length(graph_edges_delete_node[[1]]),
                   length(graph_edges[[1]]))

  # Expect that node ID "d" will not be present in the revised graph
  expect_true(!("d" %in% graph_nodes_delete_node))
  expect_true(!("d" %in% graph_edges_delete_node[[1]]))
  expect_true(!("d" %in% graph_edges_delete_node[[2]]))

  # Remove an edge (removing an edge retains nodes)
  graph <- delete_edge(graph, from = "a", to = "c")

  # Get the graph's edges after calling 'delete_node'
  graph_edges_delete_edge <- get_edges(graph)

  # Expect that edge between nodes "a" and "c" will not be present
  expect_false(edge_present(graph, from = "a", to = "c"))

  # Expect that the nodes involved in the edge deletion are retained
  expect_true(node_present(graph, node = "a"))
  expect_true(node_present(graph, node = "c"))
})

test_that("the function can be stopped with certain input values", {

  # Create an empty graph
  graph <- create_graph()

  # Add four nodes
  graph <- add_node(graph, node = "a")
  graph <- add_node(graph, node = "b")
  graph <- add_node(graph, node = "c")
  graph <- add_node(graph, node = "d")

  # Add edges with the 'to_get' relationship
  graph <- add_edges(graph, from = "a", to = "b",
                     rel = "to_get")

  # Add edges with the 'received_from' relationship
  graph <-
    add_edges(graph,
              from = c("a", "a"),
              to = c("c", "d"),
              rel = "received_from")

  # Expect an error the node specified is not a single value
  expect_error(delete_node(graph, node = c("a", "b")))

  # Expect an error if the node specified is not present in the graph
  expect_error(delete_node(graph, node = "e"))

  # Expect an error if either node specified is not a single value
  expect_error(delete_edge(graph, from = c("a", "b"), to = "c"))

  # Expect an error if both nodes specified are not present in the graph
  expect_error(delete_edge(graph, from = "a", to = "e"))
})
