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
    c(0.5, 1.0, 0.0, 0.0, 0.0))

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
    c("#7B7B7B", "#333333", "#CCCCCC", "#CCCCCC", "#CCCCCC"))

  expect_equal(
    graph_r_value_fill_font_color$nodes_df$fontcolor,
    c("#767676", "#F2F2F2", "#0D0D0D", "#0D0D0D", "#0D0D0D"))

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
    c(0.408, 0.208, 0.218, 1.000, 0.701, 0.312, 0.000))

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
    c("#898989", "#A9A9A9", "#A8A8A8", "#333333", "#5D5D5D",
      "#999999", "#CCCCCC"))

  expect_equal(
    graph_r_value_fill_font_color$edges_df$fontcolor,
    c("#616161", "#363636", "#383838", "#F2F2F2", "#A6A6A6",
      "#4C4C4C", "#0D0D0D"))

  # Expect an error if using supplying a node attribute
  # that doesn't exist (`weights` instead of `weight`)
  expect_error(graph %>% rescale_edges_attrs("weights"))
})
