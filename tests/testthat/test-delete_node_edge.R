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
  graph <- add_edge(graph, from = "a", to = "b", rel = "to_get")
  graph <- add_edge(graph, from = "a", to = "c", rel = "received_from")
  graph <- add_edge(graph, from = "c", to = "d", rel = "received_from")

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

  # Add edges
  graph <- add_edge(graph, from = "a", to = "b", rel = "to_get")
  graph <- add_edge(graph, from = "a", to = "c", rel = "received_from")
  graph <- add_edge(graph, from = "c", to = "d", rel = "received_from")

  # Expect an error the node specified is not a single value
  expect_error(delete_node(graph, node = c("a", "b")))

  # Expect an error if the node specified is not present in the graph
  expect_error(delete_node(graph, node = "e"))

  # Expect an error if either node specified is not a single value
  expect_error(delete_edge(graph, from = c("a", "b"), to = "c"))

  # Expect an error if both nodes specified are not present in the graph
  expect_error(delete_edge(graph, from = "a", to = "e"))
})

test_that("nodes and edges can be deleted from a graph via a selection", {

# Create an empty graph
graph <- create_graph()

# Add four nodes
graph <- add_node(graph, node = "a")
graph <- add_node(graph, node = "b")
graph <- add_node(graph, node = "c")
graph <- add_node(graph, node = "d")

# Add edges
graph <- add_edge(graph, from = "a", to = "b", rel = "to_get")
graph <- add_edge(graph, from = "a", to = "c", rel = "received_from")
graph <- add_edge(graph, from = "c", to = "d", rel = "received_from")

# Select 2 nodes
graph <- select_nodes(graph, nodes = c("c", "d"))

# Delete the nodes specified in the selection
graph_node_deletion <-
  delete_nodes_in_selection(graph)

# Expect a node count of 2
expect_equal(node_count(graph_node_deletion), 2)

# Expect nodes `a` and `b` to be present
expect_true(all(c("a", "b") %in% get_nodes(graph_node_deletion)))

# Expect the graph to be unchanged if there is no node selection
graph <- clear_selection(graph)
graph <- delete_nodes_in_selection(graph)

# Expect a node count of 4
expect_equal(node_count(graph), 4)

# Expect nodes `a`, `b`, `c`, and `d` to be present
expect_true(all(c("a", "b", "c", "d") %in% get_nodes(graph)))

# Select 2 edges
graph <- select_edges(graph, from = "a", to = "b")
graph <- select_edges(graph, from = "a", to = "c")

# Delete the edges specified in the selection
graph_edge_deletion <-
  delete_edges_in_selection(graph)

# Expect an edge count of 1
expect_equal(edge_count(graph_edge_deletion), 1)

# Expect edge `c` -> `d` to be present
expect_true("c -> d" %in% get_edges(graph_edge_deletion, return_type = "vector"))

# Expect the graph to be unchanged if there is no edge selection
graph <- clear_selection(graph)
graph <- delete_edges_in_selection(graph)

# Expect an edge count of 3
expect_equal(edge_count(graph), 3)

# Expect edges `a` -> `b`, `a` -> `c`, and `c` -> `d` to be present
expect_true(all(c("a -> b", "a -> c", "c -> d") %in%
                get_edges(graph, return_type = "vector")))
})
