context("Getting degree information for all nodes in a graph")

test_that("a degree data frame can be generated", {

  # Create a random graph
  graph <-
    create_random_graph(
      10, 22, set_seed = 23)

  # Get the total degree values for all nodes
  # in the graph
  total_degree_df <- get_degree_total(graph)

  # Expect that the output is a data frame
  expect_is(total_degree_df, "data.frame")

  # Expect 2 columns in the data frame
  expect_equal(ncol(total_degree_df), 2)

  # Expect 10 rows in the data frame
  expect_equal(nrow(total_degree_df), 10)

  # Expect certain column names for the df
  expect_identical(
    colnames(total_degree_df),
    c("id", "total_degree"))

  # Expect certain values in the 2nd column
  expect_equal(
    total_degree_df$total_degree,
    c(4, 5, 4, 3, 5, 7, 4, 2, 5, 5))

  # Get the total degree values for all nodes
  # in the graph as normalized values
  total_degree_df_norm <-
    get_degree_total(graph, normalized = TRUE)

  # Expect that the output is a data frame
  expect_is(total_degree_df_norm, "data.frame")

  # Expect 2 columns in the data frame
  expect_equal(ncol(total_degree_df_norm), 2)

  # Expect 10 rows in the data frame
  expect_equal(nrow(total_degree_df_norm), 10)

  # Expect certain column names for the df
  expect_identical(
    colnames(total_degree_df_norm),
    c("id", "total_degree"))

  # Expect certain values in the 2nd column
  expect_equal(
    total_degree_df_norm$total_degree,
    c(0.4444444, 0.5555556, 0.4444444, 0.3333333, 0.5555556,
      0.7777778, 0.4444444, 0.2222222, 0.5555556, 0.5555556),
    tolerance = 0.002)

  # Get the in-degree values for all nodes
  # in the graph
  in_degree_df <- get_degree_in(graph)

  # Expect that the output is a data frame
  expect_is(in_degree_df, "data.frame")

  # Expect 2 columns in the data frame
  expect_equal(ncol(in_degree_df), 2)

  # Expect 10 rows in the data frame
  expect_equal(nrow(in_degree_df), 10)

  # Expect certain column names for the df
  expect_identical(
    colnames(in_degree_df),
    c("id", "indegree"))

  # Expect certain values in the 2nd column
  expect_equal(
    in_degree_df$indegree,
    c(0, 0, 1, 0, 3, 4, 3, 2, 4, 5))

  # Get the total degree values for all nodes
  # in the graph as normalized values
  in_degree_df_norm <-
    get_degree_in(graph, normalized = TRUE)

  # Expect that the output is a data frame
  expect_is(in_degree_df_norm, "data.frame")

  # Expect 2 columns in the data frame
  expect_equal(ncol(in_degree_df_norm), 2)

  # Expect 10 rows in the data frame
  expect_equal(nrow(in_degree_df_norm), 10)

  # Expect certain column names for the df
  expect_identical(
    colnames(in_degree_df_norm),
    c("id", "indegree"))

  # Expect certain values in the 2nd column
  expect_equal(
    in_degree_df_norm$indegree,
    c(0.0000000, 0.0000000, 0.1111111, 0.0000000,
      0.3333333, 0.4444444, 0.3333333, 0.2222222,
      0.4444444, 0.5555556),
    tolerance = 0.002)

  # Get the out-degree values for all nodes
  # in the graph
  out_degree_df <- get_degree_out(graph)

  # Expect that the output is a data frame
  expect_is(out_degree_df, "data.frame")

  # Expect 2 columns in the data frame
  expect_equal(ncol(out_degree_df), 2)

  # Expect 10 rows in the data frame
  expect_equal(nrow(out_degree_df), 10)

  # Expect certain column names for the df
  expect_identical(
    colnames(out_degree_df),
    c("id", "outdegree"))

  # Expect certain values in the 2nd column
  expect_equal(
    out_degree_df$outdegree,
    c(4, 5, 3, 3, 2, 3, 1, 0, 1, 0))

  # Get the total degree values for all nodes
  # in the graph as normalized values
  out_degree_df_norm <-
    get_degree_out(graph, normalized = TRUE)

  # Expect that the output is a data frame
  expect_is(out_degree_df_norm, "data.frame")

  # Expect 2 columns in the data frame
  expect_equal(ncol(out_degree_df_norm), 2)

  # Expect 10 rows in the data frame
  expect_equal(nrow(out_degree_df_norm), 10)

  # Expect certain column names for the df
  expect_identical(
    colnames(out_degree_df_norm),
    c("id", "outdegree"))

  # Expect certain values in the 2nd column
  expect_equal(
    out_degree_df_norm$outdegree,
    c(0.4444444, 0.5555556, 0.3333333, 0.3333333,
      0.2222222, 0.3333333, 0.1111111, 0.0000000,
      0.1111111, 0.0000000),
    tolerance = 0.002)
})
