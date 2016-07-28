context("Get node calculations")

test_that("Getting betweenness is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      10, 22, set_seed = 1)

  betweenness_vals <-
    get_betweenness(graph)

  # Expect a data frame as output
  expect_is(betweenness_vals, "data.frame")

  # Expect 2 columns in the df
  expect_equal(ncol(betweenness_vals), 2)

  # Expect 10 rows in the df
  expect_equal(nrow(betweenness_vals), 10)

  # Expect node ID values in the first column
  expect_identical(betweenness_vals[,1],
                   as.character(1:10))
})

test_that("Getting bridging is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      10, 22, set_seed = 1)

  bridging_vals <-
    get_bridging(graph)

  # Expect a data frame as output
  expect_is(bridging_vals, "data.frame")

  # Expect 2 columns in the df
  expect_equal(ncol(bridging_vals), 2)

  # Expect 10 rows in the df
  expect_equal(nrow(bridging_vals), 10)

  # Expect node ID values in the first column
  expect_identical(bridging_vals[,1],
                   as.character(1:10))
})

test_that("Getting closeness is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      10, 22, set_seed = 1)

  closeness_vals <-
    get_closeness(graph)

  # Expect a data frame as output
  expect_is(closeness_vals, "data.frame")

  # Expect 2 columns in the df
  expect_equal(ncol(closeness_vals), 2)

  # Expect 10 rows in the df
  expect_equal(nrow(closeness_vals), 10)

  # Expect node ID values in the first column
  expect_identical(closeness_vals[,1],
                   as.character(1:10))
})

test_that("Getting constraint values is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      10, 22, set_seed = 1)

  constraint_vals <-
    get_constraint(graph)

  # Expect a data frame as output
  expect_is(constraint_vals, "data.frame")

  # Expect 2 columns in the df
  expect_equal(ncol(constraint_vals), 2)

  # Expect 10 rows in the df
  expect_equal(nrow(constraint_vals), 10)

  # Expect node ID values in the first column
  expect_identical(constraint_vals[,1],
                   as.character(1:10))
})
