context("Getting degree information for all nodes in a graph")

test_that("a degree data frame can be generated", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 10,
      set_seed = 23)

  # Get the total degree values for all nodes
  # in the graph
  total_degree_df <- get_degree_total(graph)

  # Expect that the output is a data frame
  expect_is(
    total_degree_df, "data.frame")

  # Expect 2 columns in the data frame
  expect_equal(
    ncol(total_degree_df), 2)

  # Expect 10 rows in the data frame
  expect_equal(
    nrow(total_degree_df), 10)

  # Expect certain column names for the df
  expect_identical(
    colnames(total_degree_df),
    c("id", "total_degree"))

  # Expect certain values in the 2nd column
  expect_equal(
    total_degree_df$total_degree,
    c(1, 1, 5, 3, 3, 1, 0, 2, 1, 3))

  # Get the total degree values for all nodes
  # in the graph as normalized values
  total_degree_df_norm <-
    get_degree_total(
      graph = graph,
      normalized = TRUE)

  # Expect that the output is a data frame
  expect_is(
    total_degree_df_norm, "data.frame")

  # Expect 2 columns in the data frame
  expect_equal(
    ncol(total_degree_df_norm), 2)

  # Expect 10 rows in the data frame
  expect_equal(
    nrow(total_degree_df_norm), 10)

  # Expect certain column names for the df
  expect_identical(
    colnames(total_degree_df_norm),
    c("id", "total_degree"))

  # Expect certain values in the 2nd column
  expect_equal(
    total_degree_df_norm$total_degree,
    c(0.1111111, 0.1111111, 0.5555556, 0.3333333, 0.3333333,
      0.1111111, 0.0000000, 0.2222222, 0.1111111, 0.3333333),
    tolerance = 0.002)

  # Get the in-degree values for all nodes
  # in the graph
  in_degree_df <- get_degree_in(graph)

  # Expect that the output is a data frame
  expect_is(
    in_degree_df, "data.frame")

  # Expect 2 columns in the data frame
  expect_equal(
    ncol(in_degree_df), 2)

  # Expect 10 rows in the data frame
  expect_equal(
    nrow(in_degree_df), 10)

  # Expect certain column names for the df
  expect_identical(
    colnames(in_degree_df),
    c("id", "indegree"))

  # Expect certain values in the 2nd column
  expect_equal(
    in_degree_df$indegree,
    c(1, 0, 2, 1, 0, 1, 0, 2, 1, 2))

  # Get the total degree values for all nodes
  # in the graph as normalized values
  in_degree_df_norm <-
    get_degree_in(
      graph = graph,
      normalized = TRUE)

  # Expect that the output is a data frame
  expect_is(
    in_degree_df_norm, "data.frame")

  # Expect 2 columns in the data frame
  expect_equal(
    ncol(in_degree_df_norm), 2)

  # Expect 10 rows in the data frame
  expect_equal(
    nrow(in_degree_df_norm), 10)

  # Expect certain column names for the df
  expect_identical(
    colnames(in_degree_df_norm),
    c("id", "indegree"))

  # Expect certain values in the 2nd column
  expect_equal(
    in_degree_df_norm$indegree,
    c(0.1111111, 0.0000000, 0.2222222, 0.1111111, 0.0000000,
      0.1111111, 0.0000000, 0.2222222, 0.1111111, 0.2222222),
    tolerance = 0.002)

  # Get the out-degree values for all nodes
  # in the graph
  out_degree_df <- get_degree_out(graph)

  # Expect that the output is a data frame
  expect_is(
    out_degree_df, "data.frame")

  # Expect 2 columns in the data frame
  expect_equal(
    ncol(out_degree_df), 2)

  # Expect 10 rows in the data frame
  expect_equal(
    nrow(out_degree_df), 10)

  # Expect certain column names for the df
  expect_identical(
    colnames(out_degree_df),
    c("id", "outdegree"))

  # Expect certain values in the 2nd column
  expect_equal(
    out_degree_df$outdegree,
    c(0, 1, 3, 2, 3, 0, 0, 0, 0, 1))

  # Get the total degree values for all nodes
  # in the graph as normalized values
  out_degree_df_norm <-
    get_degree_out(
      graph = graph,
      normalized = TRUE)

  # Expect that the output is a data frame
  expect_is(
    out_degree_df_norm, "data.frame")

  # Expect 2 columns in the data frame
  expect_equal(
    ncol(out_degree_df_norm), 2)

  # Expect 10 rows in the data frame
  expect_equal(
    nrow(out_degree_df_norm), 10)

  # Expect certain column names for the df
  expect_identical(
    colnames(out_degree_df_norm),
    c("id", "outdegree"))

  # Expect certain values in the 2nd column
  expect_equal(
    out_degree_df_norm$outdegree,
    c(0.0000000, 0.1111111, 0.3333333, 0.2222222, 0.3333333,
      0.0000000, 0.0000000, 0.0000000, 0.0000000, 0.1111111),
    tolerance = 0.002)
})
