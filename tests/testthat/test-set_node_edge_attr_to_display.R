test_that("node attributes can be chosen for display", {

  # Create a graph with a path
  # and a `value` node attribute
  graph <-
    create_graph() %>%
    add_path(
      n = 5,
      node_data = node_data(
        value = 1:5))

  # For node ID values of `1`
  # to `3`, choose to display the
  # node `value` attribute (for
  # the other nodes, display nothing)
  graph <-
    graph %>%
    set_node_attr_to_display(
      nodes = 1:3,
      attr = value,
      default = NA)

  # Expect `value` for `display`
  # node attribute in first 3 rows,
  # then, 2 NA values
  expect_equal(
    graph$nodes_df$display,
    c("value", "value", "value", NA, NA))

  # Call function several times
  # to set the `display` node
  # attribute
  graph <-
    graph %>%
    set_node_attr_to_display(
      nodes = 4,
      attr = label) %>%
    set_node_attr_to_display(
      nodes = c(1, 5),
      attr = id)

  # Expect set values for the
  # `display` node attribute
  expect_equal(
    graph$nodes_df$display,
    c("id", "value", "value", "label", "id"))
})

test_that("edge attributes can be chosen for display", {

  # Create a graph with a path
  # and a `value` edge attribute
  graph <-
    create_graph() %>%
    add_path(
      n = 5,
      edge_data = edge_data(
        value = 1:4))

  # For edge ID values of `1` to
  # `3`, choose to display the
  # edge `value` attribute (for
  # the other nodes, display nothing)
  graph <-
    graph %>%
    set_edge_attr_to_display(
      edges = 1:3,
      attr = value,
      default = NA)

  # Expect `value` for `display` edge
  # attribute in the first 3 rows,
  # then, an NA value
  expect_equal(
    graph$edges_df$display,
    c("value", "value", "value", NA))

  # Call function again times to
  # set the `display` edge attribute
  graph <-
    graph %>%
    set_edge_attr_to_display(
      edges = c(1, 3),
      attr = id)

  # Expect set values for the
  # `display` edge attribute
  expect_equal(
    graph$edges_df$display,
    c("id", "value", "id", NA))
})
