context("Joining data frames to graph dfs")

test_that("joining a data frame to an ndf is possible", {

  # Create a simple graph
  graph <-
    create_graph() %>%
    add_n_nodes(n = 5) %>%
    add_edges_w_string(
      edges = "1->2 1->3 2->4 2->5 3->5")

  # Create a data frame with node ID values and a
  # set of numeric values
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(25)

  # Create a data frame from which `values` will
  # be a join column
  df <-
    data.frame(
      id = 1:6,
      values = round(rnorm(6, 5), 2))

  # Perform the join operation
  graph <-
    graph %>%
    join_node_attrs(df = df)

  # Expect that 4 columns exists in the graph's
  # node data frame
  expect_equal(
    ncol(graph$nodes_df), 4)

  # Expect a column named `values` in the graph's
  # node data frame
  expect_true(
    "values" %in% colnames(graph$nodes_df))

  # Expect that the `values` column in the graph's
  # node data frame is equivalent to the first 5 values
  # in the `values` column of `df`
  expect_equivalent(
    graph$nodes_df$values, df$values[1:5])

  # Expect an error if `by_graph` specified but not
  # `by_df`
  expect_error(
    create_graph() %>%
    add_n_nodes(n = 5) %>%
    add_edges_w_string(
      edges = "1->2 1->3 2->4 2->5 3->5") %>%
    join_node_attrs(
      df = df,
      by_graph = "nodes"))

  # Expect an error if `by_df` specified but not
  # `by_graph`
  expect_error(
    create_graph() %>%
      add_n_nodes(n = 5) %>%
      add_edges_w_string(
        edges = "1->2 1->3 2->4 2->5 3->5") %>%
      join_node_attrs(
        df = df,
        by_df = "node"))
})

test_that("joining a data frame to an edf is possible", {

  # Create a simple graph
  graph <-
    create_graph() %>%
    add_n_nodes(n = 5) %>%
    add_edges_w_string(
      edges = "1->2 1->3 2->4 2->5 3->5")

  # Create a data frame with node ID values and a
  # set of numeric values
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(25)

  # Create a data frame from which `values` will
  # be a join column
  df <-
    data.frame(
      from = c(1, 1, 2, 2, 3),
      to = c(2, 3, 4, 5, 5),
      values = rnorm(5, 5))

  # Perform the join operation
  graph <-
    graph %>%
    join_edge_attrs(df = df)

  # Expect that 5 columns exists in the graph's
  # edge data frame
  expect_equal(
    ncol(graph$edges_df), 5)

  # Expect a column named `values` in the graph's
  # edge data frame
  expect_true(
    "values" %in% colnames(graph$edges_df))

  # Expect that the `values` column in the graph's
  # node data frame is equivalent to the first 5 values
  # in the `values` column of `df`
  expect_equivalent(
    graph$edges_df$values, df$values[1:5])

  # Expect an error if `by_graph` specified but not
  # `by_df`
  expect_error(
    create_graph() %>%
      add_n_nodes(n = 5) %>%
      add_edges_w_string(edges = "1->2 1->3 2->4 2->5 3->5") %>%
      join_edge_attrs(
        df = df,
        by_graph = c("from", "to")))

  # Expect an error if `by_df` specified but not
  # `by_graph`
  expect_error(
    create_graph() %>%
      add_n_nodes(n = 5) %>%
      add_edges_w_string(
        edges = "1->2 1->3 2->4 2->5 3->5") %>%
      join_edge_attrs(
        df = df,
        by_df = c("from", "to")))
})
