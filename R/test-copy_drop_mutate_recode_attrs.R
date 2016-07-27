context("Copying, dropping, mutating and recoding attrs.")

test_that("Copying node attributes is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add two nodes to the graph
  graph <- add_node(graph, node = "a")
  graph <- add_node(graph, node = "b")

  # Add a node attribute
  graph <- set_node_attrs(graph, "value", 1)

  # Make a copy the `value` node attribute as
  # the `value_2` node attribute
  graph <-
    copy_node_attrs(
      graph, "value", "value_2")

  # Verify that the new column has been made
  expect_true("value_2" %in% colnames(graph$nodes_df))

  # Expect that the values in `value` and `value_2`
  # are identical
  expect_identical(
    graph$nodes_df$value,
    graph$nodes_df$value_2)

  # Expect an error if `node_attr_from` and
  # `node_attr_to` are identical
  expect_error(
    copy_node_attrs(
      graph, "value_2", "value_2"))

  # Expect an error if `node_attr_to` is
  # `nodes` or `node`
  expect_error(
    copy_node_attrs(
      graph, "value_2", "nodes"))

  expect_error(
    copy_node_attrs(
      graph, "value_2", "node"))

  # Expect an error if `node_attr_from` is not one
  # of the graph's columns
  expect_error(
    copy_node_attrs(
      graph, "values", "value_3"))
})

test_that("Copying edge attributes is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add 4 nodes to the graph
  graph <- add_n_nodes(graph, 4)

  # Add 5 edges to the graph
  graph <-
    add_edges_w_string(
    graph, "1->3 2->4 1->4 3->2")

  # Add an edge attribute
  graph <- set_edge_attrs(graph, "value", 1)

  # Make a copy the `value` node attribute as
  # the `value_2` node attribute
  graph <-
    copy_edge_attrs(
      graph, "value", "value_2")

  # Verify that the new column has been made
  expect_true("value_2" %in% colnames(graph$edges_df))

  # Expect that the values in `value` and `value_2`
  # are identical
  expect_identical(
    graph$edges_df$value,
    graph$edges_df$value_2)

  # Expect an error if `edge_attr_from` and
  # `edge_attr_to` are identical
  expect_error(
    copy_edge_attrs(
      graph, "value_2", "value_2"))

  # Expect an error if `edge_attr_to` is
  # `from` or `to`
  expect_error(
    copy_edge_attrs(
      graph, "value_2", "from"))

  expect_error(
    copy_edge_attrs(
      graph, "value_2", "to"))

  # Expect an error if `edge_attr_from` is not one
  # of the graph's columns
  expect_error(
    copy_edge_attrs(
      graph, "values", "value_3"))
})
