context("Deletion of nodes and edges from an existing graph")

test_that("nodes and edges can be deleted from a graph", {

  # Create an empty graph and
  # add four nodes
  graph <-
    create_graph() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_node()

  # Add edges to the graph
  graph <-
    graph %>%
    add_edge(
      from = 1,
      to = 2,
      rel = "to_get") %>%
    add_edge(
      from = 1,
      to = 3,
      rel = "received_from") %>%
    add_edge(
      from = 3,
      to = 4,
      rel = "received_from")

  # Get the graph's nodes
  graph_nodes <- get_node_ids(graph)

  # Get the graph's edges
  graph_edges <-
    get_edges(
      x = graph,
      return_type = "list")

  # Remove a node (removing a node removes its edges)
  graph <-
    delete_node(
      graph = graph,
      node = 4)

  # Get the graph's nodes after calling `delete_node()`
  graph_nodes_delete_node <- get_node_ids(graph)

  # Get the graph's edges after calling `delete_node()`
  graph_edges_delete_node <-
    get_edges(
      x = graph,
      return_type = "list")

  # Expect that the number of nodes will be decreased
  expect_lt(
    length(graph_nodes_delete_node),
    length(graph_nodes))

  # Expect that the number of edges will be decreased
  expect_lt(
    length(graph_edges_delete_node[[1]]),
    length(graph_edges[[1]]))

  # Expect that node ID `4` will not be present in
  # the revised graph
  expect_true(
    !(4 %in% graph_nodes_delete_node))

  expect_true(
    !(4 %in% graph_edges_delete_node[[1]]))

  expect_true(
    !(4 %in% graph_edges_delete_node[[2]]))

  # Remove an edge (removing an edge retains nodes)
  graph <-
    delete_edge(
      graph = graph,
      from = 1,
      to = 3)

  # Get the graph's edges after calling `delete_node()`
  graph_edges_delete_edge <-
    get_edges(
      x = graph,
      return_type = "list")

  # Expect that edge between nodes `1` and `3` will
  # not be present
  expect_false(
    is_edge_present(
      graph = graph,
      from = 1,
      to = 3))

  # Expect that the nodes involved in the edge
  # deletion are retained
  expect_true(
    is_node_present(graph, node = 1))

  expect_true(
    is_node_present(graph, node = 3))
})

test_that("errors can occur when deleting nodes or edges with certain input values", {

  # Create an empty graph and
  # add four nodes
  graph <-
    create_graph() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_node()

  # Add edges to the graph
  graph <-
    graph %>%
    add_edge(
      from = 1,
      to = 2,
      rel = "to_get") %>%
    add_edge(
      from = 1,
      to = 3,
      rel = "received_from") %>%
    add_edge(
      from = 3,
      to = 4,
      rel = "received_from")

  # Expect an error the node specified is not a
  # single value
  expect_error(
    delete_node(graph, node = c(1, 2)))

  # Expect an error if the node specified is not
  # present in the graph
  expect_error(
    delete_node(graph, node = 5))

  # Expect an error if either node specified is not
  # a single value
  expect_error(
    delete_edge(graph, from = c(1, 2), to = 3))

  # Expect an error if both nodes specified are not
  # present in the graph
  expect_error(
    delete_edge(graph, from = 1, to = 5))
})

test_that("nodes and edges can be deleted from a graph via a selection", {

  # Create an empty graph and
  # add four nodes
  graph <-
    create_graph() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_node()

  # Add edges to the graph
  graph <-
    graph %>%
    add_edge(
      from = 1,
      to = 2,
      rel = "to_get") %>%
    add_edge(
      from = 1,
      to = 3,
      rel = "received_from") %>%
    add_edge(
      from = 3,
      to = 4,
      rel = "received_from")

  # Select 2 nodes
  graph <-
    select_nodes(
      graph = graph,
      nodes = c(3, 4))

  # Delete the nodes specified in the selection
  graph_node_deletion <- delete_nodes_ws(graph)

  # Expect a node count of 2
  expect_equal(
    node_count(graph_node_deletion), 2)

  # Expect nodes `1` and `2` to be present
  expect_true(
    all(c(1, 2) %in% get_node_ids(graph_node_deletion)))

  # Expect an error if trying to delete a node where
  # there is no node selection
  expect_error(
    graph %>%
      clear_selection() %>%
      delete_nodes_ws())

  # Expect a node count of 4
  expect_equal(
    node_count(graph), 4)

  # Expect nodes `1`, `2`, `3`, and `4` to be present
  expect_true(
    all(1:4 %in% get_node_ids(graph)))

  # Select 2 edges
  graph <-
    select_edges(
      graph = graph,
      from = 1,
      to = 2)

  graph <-
    select_edges(
      graph = graph,
      from = 1,
      to = 3)

  # Delete the edges specified in the selection
  graph_edge_deletion <- delete_edges_ws(graph)

  # Expect an edge count of 1
  expect_equal(
    edge_count(graph_edge_deletion), 1)

  # Expect edge `3`->`4` to be present
  expect_true(
    "3->4" %in% get_edges(
      x = graph_edge_deletion,
      return_type = "vector"))

  # Expect an error if trying to delete an edge where
  # there is no edge selection
  expect_error(
    graph %>%
      clear_selection() %>%
      delete_edges_ws())

  # Expect an edge count of 3
  expect_equal(
    edge_count(graph), 3)

  # Expect edges `1`->`2`, `1`->`3`, and `3`->`4`
  # to be present
  expect_true(
    all(
      c("1->2", "1->3", "3->4") %in% get_edges(
        x = graph,
        return_type = "vector")))
})

test_that("edges can be deleted from a graph using node label values", {

  # Create a directed graph with 2
  # labeled nodes and an edge
  graph_labeled_nodes <-
    create_graph() %>%
    add_n_nodes(
      n = 2,
      label = c("one", "two")) %>%
    add_edge(
      from = "one",
      to = "two")

  # Delete the single graph edge by
  # specifying the node label names
  graph_deleted_edge <-
    graph_labeled_nodes %>%
    delete_edge(
      from = "one",
      to = "two")

  # Expect a node count of 2
  expect_equal(
    node_count(graph_deleted_edge), 2)

  # Expect an edge count of 0
  expect_equal(
    edge_count(graph_deleted_edge), 0)

  # Expect an error when specifying a node
  # label that does not exist
  expect_error(
    graph_labeled_nodes %>%
      delete_edge(
        from = "zero",
        to = "two"))

  expect_error(
    graph_labeled_nodes %>%
      delete_edge(
        from = "one",
        to = "three"))

  # Create a directed graph with 3
  # labeled nodes (with indistinct labels)
  # and 1 edge
  graph_labeled_nodes_not_distinct_labels_1 <-
    create_graph() %>%
    add_path(
      n = 2,
      label = c("one", "two")) %>%
    add_node(
      label = "one")

  # Expect an error when trying to delete
  # an edge using non-distinct node labels
  expect_error(
    graph_labeled_nodes_not_distinct_labels_1 %>%
      delete_edge(
        from = "one",
        to = "two"))

  # Create another directed graph with 3
  # labeled nodes (with indistinct labels)
  # and 1 edge
  graph_labeled_nodes_not_distinct_labels_2 <-
    create_graph() %>%
    add_path(
      n = 2,
      label = c("one", "two")) %>%
    add_node(
      label = "two")

  # Expect an error when trying to delete
  # an edge using non-distinct node labels
  expect_error(
    graph_labeled_nodes_not_distinct_labels_2 %>%
      delete_edge(
        from = "one",
        to = "two"))
})
