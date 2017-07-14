context("Node types")

test_that("a specified node type can be read from graph objects", {

  # Create a node data frame
  nodes <-
    create_node_df(
      n = 4,
      type = c("first", "second", "third", "fourth"))

  # Create the graph object using the node data frame
  graph <- create_graph(nodes_df = nodes)

  read_node_a <-
    node_type(
      graph = graph,
      node = 1,
      action = "read")

  read_node_b <-
    node_type(
      graph = graph,
      node = 2,
      action = "read")

  read_node_c <-
    node_type(
      graph = graph,
      node = 3,
      action = "read")

  read_node_d <-
    node_type(
      graph = graph,
      node = 4,
      action = "read")

  read_nodes <-
    c(read_node_a, read_node_b,
      read_node_c, read_node_d)

  # Expect that the node types for each ID match
  # what was defined
  expect_equal(
    read_nodes,
    c("first", "second", "third", "fourth"))

  # Expect an error if the node ID provided doesn't
  # match a node in the graph
  expect_error(
    node_type(
      graph = graph,
      node = 5,
      action = "read"))

  # Remove a type assigned to node `4` using three
  # different keywords ("delete", "remove", and "drop")
  graph_delete_type_from_d <-
    node_type(
      graph = graph,
      node = 4,
      action = "delete")

  graph_remove_type_from_d <-
    node_type(
      graph = graph,
      node = 4,
      action = "remove")

  graph_drop_type_from_d <-
    node_type(
      graph = graph,
      node = 4,
      action = "drop")

  # Expect that the type value in all three cases will
  # be blank for node `4`
  expect_true(
    is.na(graph_delete_type_from_d$nodes_df$type[4]))

  expect_true(
    is.na(graph_remove_type_from_d$nodes_df$type[4]))

  expect_true(
    is.na(graph_drop_type_from_d$nodes_df$type[4]))

  # Add back the type assigned to node `4` using two
  # different keywords ("add" and "create")
  graph_add_type_for_d <-
    node_type(
      graph = graph_drop_type_from_d,
      node = 4,
      action = "add",
      value = "d_type_back")

  graph_create_type_for_d <-
    node_type(
      graph = graph_drop_type_from_d,
      node = 4,
      action = "create",
      value = "d_type_back")

  # Expect that the type value in both cases will be
  # as specified for node `4`
  expect_equal(
    graph_add_type_for_d$nodes_df$type[4],
    "d_type_back")

  expect_equal(
    graph_create_type_for_d$nodes_df$type[4],
    "d_type_back")

  # Create a simple graph with two nodes where one has
  # a type value available and the other has no
  # type assigned
  nodes <-
    create_node_df(
      n = 2,
      type = c("a_type", NA))

  # Create the graph object using the node data frame
  graph <- create_graph(nodes_df = nodes)

  # Expect a value of TRUE when checking whether
  # node `1` has a type value assigned
  expect_true(
    node_type(
      graph = graph,
      node = 1,
      action = "check"))

  # Expect a value of FALSE when checking whether
  # node `2` has a type value assigned
  expect_false(
    node_type(
      graph = graph,
      node = 2,
      action = "check"))

  # Attempt to remove a type value for a node where
  # no type value is set
  graph_attempt_remove_type_b <-
    node_type(
      graph = graph,
      node = 2,
      action = "delete")

  # Expect that the graph returned is exactly the
  # same as the input graph
  expect_equal(
    graph, graph_attempt_remove_type_b)

  # Attempt to add a type value for a node where a
  # type value is already set
  graph_attempt_add_type_a <-
    node_type(
      graph = graph,
      node = 1,
      action = "add",
      value = "update_a")

  # Expect that the graph returned is exactly the same
  # as the input graph and no change was made to the
  # type value of node `1`
  expect_equal(
    graph, graph_attempt_add_type_a)

  # Update the type value for a node where a type
  # value is already set
  graph_attempt_update_type_a <-
    node_type(
      graph = graph,
      node = 1,
      action = "update",
      value = "update_a")

  # Expect that the type value of node `1` was
  # changed to "update_a"
  expect_equal(
    graph_attempt_update_type_a$nodes_df$type[1],
    "update_a")

  # Attempt to update a type value for a node where
  # no type value is set
  graph_attempt_update_type_b <-
    node_type(
      graph = graph,
      node = 2,
      action = "update",
      value = "update_b")

  # Expect that the graph returned is exactly the
  # same as the input graph since there is no type
  # value for node `2` to update
  expect_equal(
    graph, graph_attempt_update_type_b)
})
