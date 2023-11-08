# Rescaling node or edge attribute values in a graph object

test_that("rescaling node attributes in a graph is possible", {

  # Create a randomized graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10, m = 22,
      node_data = node_data(value = 1:10),
      set_seed = 23
    )

  # Rescale the `value` node attribute, so that
  # its values are rescaled between 0 and 1
  graph_r_value_0_1 <-
    graph %>%
    rescale_node_attrs(
      node_attr_from = value,
      to_lower_bound = 0,
      to_upper_bound = 1
    )

  # Expect that certain (rescaled) values are now
  # available in the graph's ndf
  expect_equal(
    graph_r_value_0_1$nodes_df$value,
    c(0.000, 0.111, 0.222, 0.333, 0.444,
      0.556, 0.667, 0.778, 0.889, 1.000))

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
  expect_contains(
    colnames(graph_r_value_fill_font_color$nodes_df),
    c("fillcolor", "fontcolor"))

  # Expect that certain (rescaled) values are now
  # available in the graph's ndf
  expect_equal(
    graph_r_value_fill_font_color$nodes_df$fillcolor,
    c("#CCCCCC", "#B9B9B9", "#A7A7A7", "#959595", "#848484",
      "#737373", "#626262", "#525252", "#424242", "#333333"))

  expect_equal(
    graph_r_value_fill_font_color$nodes_df$fontcolor,
    c("#0D0D0D", "#232323", "#393939", "#515151", "#696969",
      "#838383", "#9D9D9D", "#B9B9B9", "#D5D5D5", "#F2F2F2"))

  # Expect an error if using supplying a node attribute
  # that doesn't exist (`values` instead of `value`)
  expect_error(
    graph %>%
      rescale_node_attrs(
        node_attr_from = values))
})

test_that("rescaling edge attributes in a graph is possible", {

  # Create a randomized graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 5,
      m = 10,
      edge_data = edge_data(
        weight = 1:10),
      set_seed = 23)

  # Rescale the `weight` edge attribute, so that
  # its values are rescaled between 0 and 1
  graph_r_value_0_1 <-
    graph %>%
    rescale_edge_attrs(
      edge_attr_from = weight,
      to_lower_bound = 0,
      to_upper_bound = 1)

  # Expect that certain (rescaled) values are now
  # available in the graph's edf
  expect_equal(
    graph_r_value_0_1$edges_df$weight,
    c(0.000, 0.111, 0.222, 0.333, 0.444,
      0.556, 0.667, 0.778, 0.889, 1.000))

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
    c("#CCCCCC", "#B9B9B9", "#A7A7A7", "#959595", "#848484",
      "#737373", "#626262", "#525252", "#424242", "#333333"))

  expect_equal(
    graph_r_value_fill_font_color$edges_df$fontcolor,
    c("#0D0D0D", "#232323", "#393939", "#515151", "#696969",
      "#838383", "#9D9D9D", "#B9B9B9", "#D5D5D5", "#F2F2F2"))

  # Expect an error if using supplying a node attribute
  # that doesn't exist (`weights` instead of `weight`)
  expect_error(
    graph %>%
      rescale_edge_attrs(
        edge_attr_from = weights))
})
