context("Rescaling node or edge attribute values in a graph object")

test_that("rescaling node attributes in a graph is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      5, 10, set_seed = 3,
      directed = TRUE)

  # Rescale the `value` node attribute, so that
  # its values are rescaled between 0 and 1
  graph_r_value_0_1 <-
    graph %>%
    rescale_node_attrs("value")

  # Expect that certain (rescaled) values are now
  # available in the graph's ndf
  expect_equal(
    graph_r_value_0_1$nodes_df$value,
    c(0.000, 1.000, 0.308, 0.231, 0.692))

  # Scale the values in the `value` node attribute
  # to different shades of gray for the `fillcolor`
  # and `fontcolor` node attributes
  graph_r_value_fill_font_color <-
    graph %>%
    rescale_node_attrs(
      "value", "gray80", "gray20", "fillcolor") %>%
    rescale_node_attrs(
      "value", "gray5", "gray95", "fontcolor")

  # Expect that the `fillcolor` and `fontcolor` node
  # attribute columns are now available
  expect_true(
    all(c("fillcolor", "fontcolor") %in%
          colnames(graph_r_value_fill_font_color$nodes_df)))

  # Expect that certain (rescaled) values are now
  # available in the graph's ndf
  expect_equal(
    graph_r_value_fill_font_color$nodes_df$fillcolor,
    c("#CCCCCC", "#333333", "#999999", "#A6A6A6", "#5E5E5E"))

  expect_equal(
    graph_r_value_fill_font_color$nodes_df$fontcolor,
    c("#0D0D0D", "#F2F2F2", "#4B4B4B", "#3B3B3B", "#A4A4A4"))

  # Expect an error if using supplying a node attribute
  # that doesn't exist (`values` instead of `value`)
  expect_error(graph %>% rescale_node_attrs("values"))
})

test_that("rescaling edge attributes in a graph is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      5, 7, set_seed = 3,
      directed = TRUE) %>%
    set_edge_attrs(
      "weight", rnorm(edge_count(.), 5))

  # Rescale the `weight` edge attribute, so that
  # its values are rescaled between 0 and 1
  graph_r_value_0_1 <-
    graph %>%
    rescale_edge_attrs("weight")

  # Expect that certain (rescaled) values are now
  # available in the graph's edf
  expect_equal(
    graph_r_value_0_1$edges_df$weight,
    c(0.000, 0.240, 0.848, 1.000, 0.815, 0.525, 0.584))

  # Scale the values in the `weight` edge attribute
  # to different shades of gray for the `fillcolor`
  # and `fontcolor` edge attributes
  graph_r_value_fill_font_color <-
    graph %>%
    rescale_edge_attrs(
      "weight", "gray80", "gray20", "fillcolor") %>%
    rescale_edge_attrs(
      "weight", "gray5", "gray95", "fontcolor")

  # Expect that the `fillcolor` and `fontcolor` node
  # attribute columns are now available
  expect_true(
    all(c("fillcolor", "fontcolor") %in%
          colnames(graph_r_value_fill_font_color$edges_df)))

  # Expect that certain (rescaled) values are now
  # available in the graph's ndf
  expect_equal(
    graph_r_value_fill_font_color$edges_df$fillcolor,
    c("#CCCCCC", "#A4A4A4", "#484848", "#333333", "#4C4C4C",
      "#777777", "#6E6E6E"))

  expect_equal(
    graph_r_value_fill_font_color$edges_df$fontcolor,
    c("#0D0D0D", "#3D3D3D", "#CBCBCB", "#F2F2F2", "#C2C2C2",
      "#7C7C7C", "#898989"))

  # Expect an error if using supplying a node attribute
  # that doesn't exist (`weights` instead of `weight`)
  expect_error(graph %>% rescale_edges_attrs("weights"))
})
