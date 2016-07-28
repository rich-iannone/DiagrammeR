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

test_that("Dropping node attributes is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add two nodes to the graph
  graph <- add_node(graph, node = "a")
  graph <- add_node(graph, node = "b")

  # Add a node attribute
  graph <- set_node_attrs(graph, "value", 1)

  # Expect an error if the length of `node_attr` > 1
  expect_error(
    drop_node_attrs(graph, c("value", "label")))

  # Expect an error if a column to drop does not exist
  expect_error(
    drop_node_attrs(graph, "value_2"))

  # Drop the `value` node attribute
  graph <- drop_node_attrs(graph, "value")

  # Verify that the `value` column is not present
  expect_true(!("value" %in% colnames(graph$nodes_df)))

  # Expect an error if `node_attr` is any of
  # `nodes`, `node`, `type`, or `label`
  expect_error(drop_node_attrs(graph, "nodes"))
  expect_error(drop_node_attrs(graph, "node"))
  expect_error(drop_node_attrs(graph, "type"))
  expect_error(drop_node_attrs(graph, "label"))
})

test_that("Dropping edge attributes is possible", {

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

  # Expect an error if the length of `edge_attr` > 1
  expect_error(
    drop_edge_attrs(graph, c("value", "rel")))

  # Expect an error if a column to drop does not exist
  expect_error(
    drop_edge_attrs(graph, "value_2"))

  # Drop the `value` edge attribute
  graph <- drop_edge_attrs(graph, "value")

  # Verify that the `value` column is not present
  expect_true(!("value" %in% colnames(graph$edges_df)))

  # Expect an error if `edge_attr` is any of
  # `from`, `to`, or `rel`
  expect_error(drop_edge_attrs(graph, "from"))
  expect_error(drop_edge_attrs(graph, "to"))
  expect_error(drop_edge_attrs(graph, "rel"))
})

test_that("Mutating node attributes is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add two nodes to the graph
  graph <- add_node(graph, node = "a")
  graph <- add_node(graph, node = "b")

  # Add node attributes
  graph <- set_node_attrs(graph, "value", 1)
  graph <- set_node_attrs(graph, "shape", "square")

  # Expect an error if `node_attr_from` is not one
  # of the graph's columns
  expect_error(
    mutate_node_attrs(
      graph, "value_2", ". * 2"))

  # Expect an error if trying to mutate a non-numeric
  # node attribute
  expect_error(
    mutate_node_attrs(
      graph, "shape", ". * 2"))

  # Mutate the `value` node attribute
  graph <-
    mutate_node_attrs(
      graph, "value", ". * 2")

  # Expect each value in `value` to be 2
  expect_equal(graph$nodes_df$value, c("2", "2"))

  # Mutate the `value` node attribute again but
  # also copy mutated values into `value_2`
  graph <-
    mutate_node_attrs(
      graph, "value", ". * 2",
      node_attr_to = "value_2")

  # Verify that the new column has been made
  expect_true("value_2" %in% colnames(graph$nodes_df))

  # Expect each value in `value_2` to be 4
  expect_equal(graph$nodes_df$value_2, c("4", "4"))

  # Expect an error if `node_attr_to` is `nodes` or `node`
  expect_error(
    mutate_node_attrs(
      graph, "value", ". * 2",
      node_attr_to = "nodes"))

  expect_error(
    mutate_node_attrs(
      graph, "value", ". * 2",
      node_attr_to = "node"))

  # Mutate the `value` node attribute and
  # copy and overwrite mutated values into `value_2`
  graph <-
    mutate_node_attrs(
      graph, "value", ". * 4",
      node_attr_to = "value_2")

  # Expect each value in `value_2` to be 8
  expect_equal(graph$nodes_df$value_2, c("8", "8"))
})
