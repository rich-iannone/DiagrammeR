context("Defining node and edge aesthetics and data attributes")

test_that("creating node data attribute values is possible", {

  # Create a node data list object
  # using the `node_data()` function
  node_data <-
    node_data(
      values_1 = c(1, 2, 4, 5),
      values_2 = c("one", "two", "four", "five"))

  # Expect that a list object has
  # been generated
  expect_is(
    node_data, "list")

  # Expect specific names in the object
  expect_equal(
    names(node_data),
    c("values_1", "values_2"))

  # Expect specific values in each
  # of the list components
  expect_equal(
    node_data[[1]],
    c(1, 2, 4, 5))

  expect_equal(
    node_data[[2]],
    c("one", "two", "four", "five"))
})

test_that("creating edge data attribute values is possible", {

  # Create a edge data list object
  # using the `edge_data()` function
  edge_data <-
    edge_data(
      values_1 = c(1, 2, 4, 5),
      values_2 = c("one", "two", "four", "five"))

  # Expect that a list object has
  # been generated
  expect_is(
    edge_data, "list")

  # Expect specific names in the object
  expect_equal(
    names(edge_data),
    c("values_1", "values_2"))

  # Expect specific values in each
  # of the list components
  expect_equal(
    edge_data[[1]],
    c(1, 2, 4, 5))

  expect_equal(
    edge_data[[2]],
    c("one", "two", "four", "five"))
})
