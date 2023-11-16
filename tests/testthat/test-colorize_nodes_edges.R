# Colorizing based on node and edge attrs

test_that("Adding color based on node attributes is possible", {

  # Create a random graph of
  # 50 nodes and 85 edges
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 50,
      m = 85,
      node_data = node_data(
        value = 1:50),
      set_seed = 23)

  # Find group membership values for all nodes
  # in the graph through the Walktrap community
  # finding algorithm and join those group values
  # to the graph's internal node data frame (ndf)
  # with the `join_node_attrs()` function
  graph <-
    graph %>%
    join_node_attrs(
      get_cmty_walktrap(.))

  # Use the `colorize_node_attrs()` function
  # to set different `fillcolor` values
  graph <-
    graph %>%
    colorize_node_attrs(
      node_attr_from = walktrap_group,
      node_attr_to = fillcolor)

  # Expect that the `fillcolor` column has
  # been created in the node data frame
  expect_contains(colnames(graph$nodes_df), "fillcolor")

  # Expect that there are as many different
  # colors in the `fillcolor` column as there
  # are walktrap communities
  expect_equal(
    length(unique(graph$nodes_df$walktrap_group)),
    length(unique(graph$nodes_df$fillcolor)))

  # Expect that each value in the `fillcolor`
  # column is a properly-formed hexadecimal color
  # code
  expect_match(
    graph$nodes_df$fillcolor,
    "#[0-9A-F]{6}")

  # Use the `colorize_node_attrs()` function
  # to set different `color` values with an
  # alpha value set to `90`
  graph <-
    graph %>%
    colorize_node_attrs(
      node_attr_from = walktrap_group,
      node_attr_to = color,
      alpha = 90) %>%
    set_node_attrs(
      node_attr = fontcolor,
      values = "white")

  # Expect that the `color` column has
  # been created in the node data frame
  expect_contains(colnames(graph$nodes_df), "color")

  # Expect that there are as many different
  # colors in the `color` column as there
  # are walktrap communities
  expect_equal(
    length(unique(graph$nodes_df$walktrap_group)),
    length(unique(graph$nodes_df$color)))

  # Expect that each value in the `color` column
  # is a properly-formed hexadecimal color code
  # with alpha value as suffix
  expect_match(
    graph$nodes_df$color,
    "#[0-9A-F]{6}[0-9]{2}")

  # Create a random graph of
  # 10 nodes and 22 edges
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      node_data = node_data(
        value = 1:10),
      set_seed = 23)

  # Bucketize values in `value` using `cut_points`
  # and assign colors to each of the bucketed ranges
  # (for values not part of any bucket, a gray color
  # is assigned by default)
  graph <-
    graph %>%
    colorize_node_attrs(
      node_attr_from = value,
      node_attr_to = fillcolor,
      cut_points = c(1, 3, 5, 7, 9))

  # Expect that the `fillcolor` column has
  # been created in the node data frame
  expect_contains(colnames(graph$nodes_df), "fillcolor")

  # Expect that there are 5 colors in the
  # `fillcolor` column
  expect_length(unique(graph$nodes_df$fillcolor), 5)

  # Expect that each value in the `fillcolor`
  # column is a properly-formed hexadecimal color
  # code
  expect_match(
    graph$nodes_df$fillcolor,
    "#[0-9A-F]{6}")

  # Bucketize values as before but use an alpha
  # value of `90`
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      node_data = node_data(
        value = 1:10),
      set_seed = 23) %>%
    colorize_node_attrs(
      node_attr_from = value,
      node_attr_to = fillcolor,
      cut_points = c(1, 3, 5, 7, 9),
      alpha = 90)

  # Expect that there are 5 colors in the
  # `fillcolor` column
  expect_length(unique(graph$nodes_df$fillcolor), 5)

  # Expect that each value in the `color` column
  # is a properly-formed hexadecimal color code
  # with alpha value as suffix
  expect_match(
    graph$nodes_df$fillcolor,
    "#[0-9A-F]{6}[0-9]{2}")

  # Bucketize values as before but use an alpha
  # value of `100`
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      node_data = node_data(
        value = 1:10),
      set_seed = 23) %>%
    colorize_node_attrs(
      node_attr_from = value,
      node_attr_to = fillcolor,
      cut_points = c(1, 3, 5, 7, 9),
      alpha = 100)

  # Expect that there are 5 colors in the
  # `fillcolor` column
  expect_length(unique(graph$nodes_df$fillcolor), 5)

  # Expect that each value in the `fillcolor`
  # column is a properly-formed hexadecimal color
  # code
  expect_match(
    graph$nodes_df$fillcolor,
    "#[0-9A-F]{6}")

  # Bucketize as before but color according to hexadecimal vector input
  # Includes one valid hex color not in x11_hex()
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      node_data = node_data(
        value = 1:10),
      set_seed = 23) %>%
    colorize_node_attrs(
      node_attr_from = value,
      node_attr_to = fillcolor,
      cut_points = c(1, 3, 5, 7, 9),
      palette = c("#64ccc9", "#8b3e2f", "#00eeee", "#556b2f", "#9932cc",
                  "#698b69", "#cd2626", "#4a4a4a"))

  # Expect that there are 5 colors in the
  # `fillcolor` column
  expect_length(unique(graph$nodes_df$fillcolor), 5)

  # Expect that each value in the `fillcolor`
  # column is a properly-formed hexadecimal color
  # code
  expect_match(
    graph$nodes_df$fillcolor,
    "#[0-9A-F]{6}")

  # Expect error when given mix of valid and invalid hexadecimal colors
  expect_error(
    create_graph() %>%
      add_gnm_graph(
        n = 10,
        m = 22,
        node_data = node_data(
          value = 1:10),
        set_seed = 23) %>%
      colorize_node_attrs(
        node_attr_from = value,
        node_attr_to = fillcolor,
        cut_points = c(1, 3, 5, 7, 9),
        palette = c("458b00l15", "foo", "#00eeee", "bar", "orange")))

})

test_that("Adding color based on edge attributes is possible", {

  # Create a random graph of
  # 10 nodes and 10 edges;
  # add the `weight` and `rel`
  # edge attrs
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 10,
      edge_data = edge_data(
        weight = rnorm(10, 5, 2)),
      set_seed = 23) %>%
    set_edge_attrs(
      edge_attr = "rel",
      values = c("A", "A", "B", "B", "D",
                 "A", "B", "C", "D", "A"))

  # Use the `colorize_edge_attrs()` function
  # to set different `color` values
  graph <-
    graph %>%
    colorize_edge_attrs(
      edge_attr_from = rel,
      edge_attr_to = color) %>%
    colorize_edge_attrs(
      edge_attr_from = rel,
      edge_attr_to = fontcolor,
      alpha = 90)

  # Expect that the `color` and `fontcolor`
  # columns have been created in the edf
  expect_contains(
    colnames(graph$edges_df),
    c("color", "fontcolor")
  )

  # Expect that there are as many different
  # colors in the `color` and `fontcolor`
  # columns as there are distinct `rel` values
  expect_equal(
    length(unique(graph$edges_df$rel)),
    length(unique(graph$edges_df$color)))

  expect_equal(
    length(unique(graph$edges_df$rel)),
    length(unique(graph$edges_df$fontcolor)))

  # Expect that each value in the `color`
  # column is a properly-formed hexadecimal color
  # code
  expect_match(
    graph$edges_df$color,
    "#[0-9A-F]{6}")

  # Expect that each value in the `fontcolor`
  # column is a properly-formed hexadecimal
  # color code with alpha value as suffix
  expect_match(
    graph$edges_df$fontcolor,
    "#[0-9A-F]{6}[0-9]{2}")

  # Bucketize values in `weight` using `cut_points`
  # and assign colors to each of the bucketed ranges
  # (for values not part of any bucket, a gray color
  # is assigned by default)
  graph <-
    graph %>%
    colorize_edge_attrs(
      edge_attr_from = weight,
      edge_attr_to = labelfontcolor,
      cut_points = c(0, 2, 4, 6, 8, 10))

  # Expect that the `labelfontcolor` column has
  # been created in the edge data frame
  expect_contains(
    colnames(graph$edges_df),
    "labelfontcolor"
    )

  # Expect that there are 4 colors in the
  # `labelfontcolor` column
  expect_length(unique(graph$edges_df$labelfontcolor), 4)

  # Expect that each value in the `labelfontcolor`
  # column is a properly-formed hexadecimal color
  # code
  expect_match(
    graph$edges_df$labelfontcolor,
    "#[0-9A-F]{6}")

  # Create a random graph of
  # 10 nodes and 10 edges;
  # add the `weight` and `rel`
  # edge attrs
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 10,
      edge_data = edge_data(
        weight = rnorm(10, 5, 2)),
      set_seed = 23) %>%
    set_edge_attrs(
      edge_attr = "rel",
      values = c("A", "A", "B", "B", "D",
                 "A", "B", "C", "D", "A"))

  # Bucketize values as before but use an alpha
  # value of `90`
  graph <-
    graph %>%
    colorize_edge_attrs(
      edge_attr_from = weight,
      edge_attr_to = labelfontcolor,
      cut_points = c(0, 2, 4, 6, 8, 10),
      alpha = 90)

  # Expect that there are 4 colors in the
  # `labelfontcolor` column
  expect_length(unique(graph$edges_df$labelfontcolor), 4)

  # Expect that each value in the `labelfontcolor`
  # column is a properly-formed hexadecimal color
  # code with alpha value as suffix
  expect_match(
    graph$edges_df$labelfontcolor,
    "#[0-9A-F]{6}[0-9]{2}")

  # Create a random graph of 10 nodes and 10 edges;
  # add the `weight` and `rel` edge attrs
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 10,
      edge_data = edge_data(
        weight = 1:10),
      set_seed = 23) %>%
    set_edge_attrs(
      edge_attr = rel,
      values = c("A", "A", "B", "B", "D",
                 "A", "B", "C", "D", "A"))

  # Bucketize values as before but use an alpha
  # value of `100`
  graph <-
    graph %>%
    colorize_edge_attrs(
      edge_attr_from = weight,
      edge_attr_to = labelfontcolor,
      cut_points = c(0, 2, 4, 6, 8, 10),
      alpha = 100)

  # Expect that there are 6 colors in the
  # `labelfontcolor` column
  expect_length(
    unique(graph$edges_df$labelfontcolor),
    6)

  # Expect that each value in the `labelfontcolor`
  # column is a properly-formed hexadecimal color
  # code
  expect_match(
    graph$edges_df$labelfontcolor,
    "#[0-9A-F]{6}")

  # Color edges according to hexadecimal vector input
  # Includes one valid hex color not in x11_hex()
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 10,
      edge_data = edge_data(
        weight = rnorm(10, 5, 2)),
      set_seed = 23) %>%
    set_edge_attrs(
      edge_attr = "rel",
      values = c("A", "A", "B", "B", "D",
                 "A", "B", "C", "D", "A")) %>%
    colorize_edge_attrs(
      edge_attr_from = rel,
      edge_attr_to = color,
      palette = c("#64ccc9", "#8b3e2f", "#00eeee", "#556b2f", "#9932cc",
                  "#698b69", "#cd2626", "#4a4a4a"))

  # Expect that each value in the `color`
  # column is a properly-formed hexadecimal color
  # code
  expect_match(
    graph$edges_df$color,
    "#[0-9A-F]{6}")

  # Expect error message if given invalid hexadecimal color
  expect_snapshot(error = TRUE,
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 10,
      edge_data = edge_data(
        weight = rnorm(10, 5, 2)),
      set_seed = 23) %>%
    set_edge_attrs(
      edge_attr = "rel",
      values = c("A", "A", "B", "B", "D",
                 "A", "B", "C", "D", "A")) %>%
    colorize_edge_attrs(
      edge_attr_from = rel,
      edge_attr_to = color,
      palette = c("#458b00l15", "foo", "#00eeee", "bar", "orange")))
})
