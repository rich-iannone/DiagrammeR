context("Colorizing based on node and edge attrs")

test_that("Adding color based on node attributes is possible", {

  # Create a random graph of 50 nodes and 85 edges
  graph <-
    create_random_graph(
      50, 85, set_seed = 23)

  # Find group membership values for all nodes
  # in the graph through the Walktrap community
  # finding algorithm and join those group values
  # to the graph's internal node data frame (ndf)
  # with the `join_node_attrs()` function
  graph <-
    graph %>%
    join_node_attrs(get_cmty_walktrap(.))

  # Use the `colorize_node_attrs()` function
  # to set different `fillcolor` values
  graph <-
    graph %>%
    colorize_node_attrs(
      "walktrap_group", "fillcolor")

  # Expect that the `fillcolor` column has
  # been created in the node data frame
  expect_true(
    "fillcolor" %in% colnames(graph$nodes_df))

  # Expect that there are as many different
  # colors in the `fillcolor` column as there
  # are walktrap communities
  expect_equal(
    length(unique(graph$nodes_df$walktrap_group)),
    length(unique(graph$nodes_df$fillcolor)))

  # Expect that each value in the `fillcolor`
  # column is a properly-formed hexadecimal color
  # code
  expect_match(graph$nodes_df$fillcolor, "#[0-9A-F]{6}")

  # Use the `colorize_node_attrs()` function
  # to set different `color` values with an
  # alpha value set to `90`
  graph <-
    graph %>%
    colorize_node_attrs(
      "walktrap_group", "color", alpha = 90) %>%
    set_node_attrs("fontcolor", "white") %>%
    set_global_graph_attrs(
      "graph", "layout", "circo")

  # Expect that the `fillcolor` column has
  # been created in the node data frame
  expect_true(
    "color" %in% colnames(graph$nodes_df))

  # Expect that there are as many different
  # colors in the `fillcolor` column as there
  # are walktrap communities
  expect_equal(
    length(unique(graph$nodes_df$walktrap_group)),
    length(unique(graph$nodes_df$color)))

  # Expect that each value in the `color` column
  # is a properly-formed hexadecimal color code
  # with alpha value as suffix
  expect_match(graph$nodes_df$color, "#[0-9A-F]{6}[0-9]{2}")

  # Create a random graph of 10 nodes and 22
  # edges; this function automatically provides
  # a node attribute `value` which has values
  # in the range of 0 to 10.
  graph <-
    create_random_graph(
      10, 22, set_seed = 1)

  # Bucketize values in `value` using `cut_points`
  # and assign colors to each of the bucketed ranges
  # (for values not part of any bucket, a gray color
  # is assigned by default)
  graph <-
    graph %>%
    colorize_node_attrs(
      "value", "fillcolor",
      cut_points = c(1, 3, 5, 7, 9))

  # Expect that the `fillcolor` column has
  # been created in the node data frame
  expect_true(
    "fillcolor" %in% colnames(graph$nodes_df))

  # Expect that there are 5 colors in the
  # `fillcolor` column
  expect_equal(
    length(unique(graph$nodes_df$fillcolor)), 5)

  # Expect that each value in the `fillcolor`
  # column is a properly-formed hexadecimal color
  # code
  expect_match(graph$nodes_df$fillcolor, "#[0-9A-F]{6}")
})
