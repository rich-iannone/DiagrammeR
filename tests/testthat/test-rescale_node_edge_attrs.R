context("Rescaling node or edge attribute values in a graph object")

test_that("rescaling node attributes in a graph is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 5, m = 10,
      set_seed = 23)

  # Rescale the `value` node attribute, so that
  # its values are rescaled between 0 and 1
  graph_r_value_0_1 <-
    graph %>%
    rescale_node_attrs(node_attr_from = value)

  # Expect that certain (rescaled) values are now
  # available in the graph's ndf
  expect_equal(
    graph_r_value_0_1$nodes_df$value,
    c(0.583, 0.000, 0.167, 0.833, 1.000))

  # Scale the values in the `value` node attribute
  # to different shades of gray for the `fillcolor`
  # and `fontcolor` node attributes
  graph_r_value_fill_font_color <-
    graph %>%
    rescale_node_attrs(
      node_attr_from = value,
      to_lower_bound = "gray80",
      to_upper_bound = "gray20",
      node_attr_to = fillcolor) %>%
    rescale_node_attrs(
      node_attr_from = value,
      to_lower_bound = "gray5",
      to_upper_bound = "gray95",
      node_attr_to = fontcolor)

  # Expect that the `fillcolor` and `fontcolor` node
  # attribute columns are now available
  expect_true(
    all(
      c("fillcolor", "fontcolor") %in%
        colnames(graph_r_value_fill_font_color$nodes_df)))

  # Expect that certain (rescaled) values are now
  # available in the graph's ndf
  expect_equal(
    graph_r_value_fill_font_color$nodes_df$fillcolor,
    c("#6E6E6E", "#CCCCCC", "#B0B0B0", "#4A4A4A", "#333333"))

  expect_equal(
    graph_r_value_fill_font_color$nodes_df$fontcolor,
    c("#898989", "#0D0D0D", "#2E2E2E", "#C7C7C7", "#F2F2F2"))

  # Expect an error if using supplying a node attribute
  # that doesn't exist (`values` instead of `value`)
  expect_error(
    graph %>% rescale_node_attrs(node_attr_from = values))
})

test_that("rescaling edge attributes in a graph is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 5, m = 7,
      set_seed = 23) %>%
    set_edge_attrs(
      edge_attr = weight,
      values = rnorm(count_edges(.), 5))

  # Rescale the `weight` edge attribute, so that
  # its values are rescaled between 0 and 1
  graph_r_value_0_1 <-
    graph %>%
    rescale_edge_attrs(
      edge_attr_from = weight)

  # Expect that certain (rescaled) values are now
  # available in the graph's edf
  expect_equal(
    graph_r_value_0_1$edges_df$weight,
    c(0.845, 0.100, 0.546, 1.000, 0.000, 0.898, 0.410))

  # Scale the values in the `weight` edge attribute
  # to different shades of gray for the `fillcolor`
  # and `fontcolor` edge attributes
  graph_r_value_fill_font_color <-
    graph %>%
    rescale_edge_attrs(
      edge_attr_from = weight,
      to_lower_bound = "gray80",
      to_upper_bound = "gray20",
      edge_attr_to = fillcolor) %>%
    rescale_edge_attrs(
      edge_attr_from = weight,
      to_lower_bound = "gray5",
      to_upper_bound = "gray95",
      edge_attr_to = fontcolor)

  # Expect that the `fillcolor` and `fontcolor` node
  # attribute columns are now available
  expect_true(
    all(
      c("fillcolor", "fontcolor") %in%
        colnames(graph_r_value_fill_font_color$edges_df)))

  # Expect that certain (rescaled) values are now
  # available in the graph's ndf
  expect_equal(
    graph_r_value_fill_font_color$edges_df$fillcolor,
    c("#484848", "#BBBBBB", "#747474", "#333333",
      "#CCCCCC", "#414141", "#898989"))

  expect_equal(
    graph_r_value_fill_font_color$edges_df$fontcolor,
    c("#CACACA", "#212121", "#818181", "#F2F2F2",
      "#0D0D0D", "#D7D7D7", "#616161"))

  # Expect an error if using supplying a node attribute
  # that doesn't exist (`weights` instead of `weight`)
  expect_error(
    graph %>%
      rescale_edge_attrs(
        edge_attr_from = weights))
})
