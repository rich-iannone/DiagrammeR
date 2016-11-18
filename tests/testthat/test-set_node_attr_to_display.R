context("Set node attribute value to display")

test_that("node attributes can be chosen for display", {

  # Create a random graph
  graph <-
    create_random_graph(
      5, 5, set_seed = 23)

  # For node ID values of `1` to `3`, choose
  # to display the node `value` attribute (for
  # the other nodes, display nothing)
  graph <-
    graph %>%
    set_node_attr_to_display(
      nodes = 1:3, attr = "value", default = NA)

  # Expect `value` for `display` node attribute in
  # first 3 rows, then, 2 NA values
  expect_equal(
    graph$nodes_df$display,
    c("value", "value", "value", NA, NA))

  # Call function several times to set the `display`
  # node attribute
  graph <-
    graph %>%
    set_node_attr_to_display(
      nodes = 4, attr = "label") %>%
    set_node_attr_to_display(
      nodes = c(1, 5), attr = "id")

  # Expect set values for `display` node attribute
  expect_equal(
    graph$nodes_df$display,
    c("id", "value", "value", "label", "id"))

})
