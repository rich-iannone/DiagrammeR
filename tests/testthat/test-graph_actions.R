context("Using graph actions")

test_that("actions can be added to a graph object", {

  # Create an empty graph object
  graph <- create_graph()

  # Add a graph action that sets a node
  # attr column with a function; the
  # main function `set_node_attr_w_fcn()`
  # uses the `get_betweenness()` function
  # to provide betweenness values in the
  # `btwns` column
  graph <-
    graph %>%
    add_graph_action(
      fcn = "set_node_attr_w_fcn",
      node_attr_fcn = "get_betweenness",
      column_name = "btwns",
      action_name = "get_btwns")

  # Expect a `data.frame` object with in
  # `graph$graph_actions`
  expect_is(
    graph$graph_actions, "data.frame")

  # Extract `graph$graph_actions` to a
  # separate object
  graph_actions <-
    graph$graph_actions

  # Expect a single row in the data frame
  expect_equal(
    nrow(graph_actions), 1)

  # Expect three columns in the data frame
  expect_equal(
    ncol(graph_actions), 3)

  # Expect certain column names in the
  # data frame object
  expect_equal(
    colnames(graph_actions),
    c("action_index", "action_name", "expression"))

  # Expect the `action_index` to be 1
  expect_equal(
    graph_actions$action_index, 1)

  # Expect the `action_name` to be `get_btwns`
  expect_equal(
    graph_actions$action_name, "get_btwns")

  # Expect the action in the data frame to
  # be correctly generated
  expect_equal(
    graph_actions$expression,
    "set_node_attr_w_fcn(graph = graph, node_attr_fcn = 'get_betweenness', column_name = 'btwns')")
})

test_that("actions can be deleted from a graph object", {

  # Create an empty graph object
  graph <- create_graph()

  # Add three graph actions to the
  # graph
  graph <-
    graph %>%
    add_graph_action(
      fcn = "set_node_attr_w_fcn",
      node_attr_fcn = "get_pagerank",
      column_name = "pagerank",
      action_name = "get_pagerank") %>%
    add_graph_action(
      fcn = "rescale_node_attrs",
      node_attr_from = "pagerank",
      node_attr_to = "width",
      action_name = "pagerank_to_width") %>%
    add_graph_action(
      fcn = "colorize_node_attrs",
      node_attr_from = "width",
      node_attr_to = "fillcolor",
      action_name = "pagerank_fillcolor")

  number_of_graph_actions_before_deletion <-
    nrow(graph$graph_actions)

  # Delete two of the graph actions
  graph <-
    graph %>%
    delete_graph_actions(
      actions = c(2, 3))

  # Expect that one graph action remains
  # Expect a single row in the data frame
  expect_equal(
    nrow(graph$graph_actions),
    number_of_graph_actions_before_deletion - 2)

  # Expect that the first graph action
  # remains in the graph
  # Expect the `action_index` to be 1
  expect_equal(
    graph$graph_actions$action_index, 1)

  # Expect the `action_name` to be `get_btwns`
  expect_equal(
    graph$graph_actions$action_name, "get_pagerank")

  # Expect the action in the data frame to
  # be correctly generated
  expect_equal(
    graph$graph_actions$expression,
    "set_node_attr_w_fcn(graph = graph, node_attr_fcn = 'get_pagerank', column_name = 'pagerank')")
})

test_that("actions within a graph object can be reordered", {

  # Create an empty graph object
  graph <- create_graph()

  # Add three graph actions to the
  # graph
  graph <-
    graph %>%
    add_graph_action(
      fcn = "set_node_attr_w_fcn",
      node_attr_fcn = "get_pagerank",
      column_name = "pagerank",
      action_name = "get_pagerank") %>%
    add_graph_action(
      fcn = "rescale_node_attrs",
      node_attr_from = "pagerank",
      node_attr_to = "width",
      action_name = "pagerank_to_width") %>%
    add_graph_action(
      fcn = "colorize_node_attrs",
      node_attr_from = "width",
      node_attr_to = "fillcolor",
      action_name = "pagerank_fillcolor")

  # Get the names of the graph actions
  # before the reordering occurs
  names_of_graph_actions_before_reordering <-
    graph$graph_actions$action_name

  # Reorder the graph actions so that `2`,
  # precedes `3`, which precedes `1`
  graph <-
    graph %>%
    reorder_graph_actions(
      indices = c(2, 3, 1))

  # Expect three graph actions in the
  # graph object
  expect_equal(
    nrow(graph$graph_actions), 3)

  # Get the names of the graph actions
  # before the reordering occurs
  names_of_graph_actions_after_reordering <-
    graph$graph_actions$action_name

  # Expect that the graph action names
  # appear in the order according to the
  # vector provided as `indices`
  expect_equal(
    names_of_graph_actions_after_reordering,
    names_of_graph_actions_before_reordering[c(2, 3, 1)])
})
