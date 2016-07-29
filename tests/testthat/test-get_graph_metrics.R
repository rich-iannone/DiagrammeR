context("Get graph metrics")

test_that("Getting degree distribution is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      10, 22, set_seed = 1)

  degree_dist <-
    get_degree_histogram(graph)

  # Expect that `degree_dist` inherits from `table`
  expect_is(degree_dist, "table")

  # Expect certain names for the `degree_dist` object
  expect_identical(names(degree_dist),
                   c("3", "4", "5", "6"))

  # Expect certain values in the `degree_dist` object
  expect_equal(degree_dist[[1]], 2)
  expect_equal(degree_dist[[2]], 4)
  expect_equal(degree_dist[[3]], 2)
  expect_equal(degree_dist[[4]], 2)
})

test_that("Getting graph diameter is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      10, 22, set_seed = 1)

  graph_diam <-
    get_graph_diameter(graph)

  # Expect that `graph_diam` is a numeric vector
  expect_is(graph_diam, "numeric")

  # Expect that `graph_diam` is of length 1
  expect_equal(length(graph_diam), 1)

  # Expect that `graph_diam` has the value 8
  expect_equal(graph_diam, 8)
})

test_that("Getting graph eccentricity is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      10, 22, set_seed = 1)

  graph_eccen <-
    get_eccentricity(graph)

  # Expect that `graph_eccen` is a numeric vector
  expect_is(graph_eccen, "numeric")

  # Expect that `graph_eccen` has length 10 in
  # this case
  expect_equal(length(graph_eccen), 10)

  # Expect certain names for the `graph_eccen` object
  expect_equal(names(graph_eccen), as.character(1:10))
})

test_that("Getting graph periphery is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      10, 22, set_seed = 1)

  graph_periphery <-
    get_periphery(graph)

  # Expect that `graph_periphery` is a character vector
  expect_is(graph_periphery, "character")

  # Expect that `graph_periphery` has length 2 in
  # this case
  expect_equal(length(graph_periphery), 2)

  # Expect certain values for the
  # `graph_periphery` object
  expect_identical(graph_periphery, c("1", "10"))
})

test_that("Getting graph info is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      10, 22, set_seed = 1)

  graph_i <-
    graph_info(graph)

  # Expect that `graph_i` is a data frame
  expect_is(graph_i, "data.frame")

  # Expect that `graph_i` has 9 columns
  expect_equal(ncol(graph_i), 9)

  # Expect that `graph_i` has 1 row
  expect_equal(nrow(graph_i), 1)

  # Expect that `graph_i` has certain column names
  expect_identical(
    colnames(graph_i),
    c("name", "n", "e", "dens", "min_deg",
      "max_deg", "avg_deg", "time", "tz"))
})
