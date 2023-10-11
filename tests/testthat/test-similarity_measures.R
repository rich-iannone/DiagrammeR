context("Similarity measures for whole graphs")

test_that("the dice similarity algorithm is functional", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      set_seed = 23)

  # Get the Dice similarity values for
  # nodes `5`, `6`, and `7`; all directions
  dice_all <-
    get_dice_similarity(
      graph = graph,
      nodes = 5:7,
      direction = "all")

  # Get the Dice similarity values for
  # nodes `5`, `6`, and `7`; `out` direction
  dice_out <-
    get_dice_similarity(
      graph = graph,
      nodes = 5:7,
      direction = "out")

  # Get the Dice similarity values for
  # nodes `5`, `6`, and `7`; `in` direction
  dice_in <-
    get_dice_similarity(
      graph = graph,
      nodes = 5:7,
      direction = "in")

  # Expect that a `matrix` object is returned
  expect_true(is.matrix(dice_all))
  expect_true(is.matrix(dice_out))
  expect_true(is.matrix(dice_in))

  # Expect a square matrix of 3 columns and 3 rows
  expect_equal(dim(dice_all), c(3, 3))
  expect_equal(dim(dice_out), c(3, 3))
  expect_equal(dim(dice_in), c(3, 3))

  # Expect all columns to be numeric
  expect_type(dice_all[, 1], "double")
  expect_type(dice_all[, 2], "double")
  expect_type(dice_all[, 3], "double")

  expect_type(dice_out[, 1], "double")
  expect_type(dice_out[, 2], "double")
  expect_type(dice_out[, 3], "double")

  expect_type(dice_in[, 1], "double")
  expect_type(dice_in[, 2], "double")
  expect_type(dice_in[, 3], "double")

  # Expect specific column names in this data frame
  expect_equal(
    colnames(dice_all), c("5", "6", "7"))

  expect_equal(
    colnames(dice_out), c("5", "6", "7"))

  expect_equal(
    colnames(dice_in), c("5", "6", "7"))

  # Expect specific row names in this data frame
  expect_equal(
    rownames(dice_all), c("5", "6", "7"))

  expect_equal(
    rownames(dice_out), c("5", "6", "7"))

  expect_equal(
    rownames(dice_in), c("5", "6", "7"))

  # Expect all values in the matrix to be less than
  # or equal to 1.0
  expect_true(
    all(as.numeric(dice_all) <= 1.0))

  expect_true(
    all(as.numeric(dice_out) <= 1.0))

  expect_true(
    all(as.numeric(dice_in) <= 1.0))

  # Get the Dice similarity values for all nodes,
  # all directions
  dice_all_all <-
    get_dice_similarity(
      graph = graph,
      direction = "all")

  # Expect the same number of matrix columns
  # and rows as there are nodes
  expect_equal(
    ncol(dice_all_all), count_nodes(graph = graph))

  expect_equal(
    nrow(dice_all_all), count_nodes(graph = graph))

  # Expect an error if one or more node IDs
  # provided are not in the graph
  expect_error(
    get_dice_similarity(
      graph = graph,
      nodes = 8:12,
      direction = "all"))
})

test_that("the Jaccard similarity algorithm is functional", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      set_seed = 23)

  # Expect an error when using a value
  # for `direction` that is not one of
  # three accepted values
  expect_error(
    get_jaccard_similarity(
      graph = graph,
      nodes = 5:7,
      direction = "away"))

  # Get the Jaccard similarity values for
  # nodes `5`, `6`, and `7`; all directions
  jaccard_all <-
    get_jaccard_similarity(
      graph = graph,
      nodes = 5:7,
      direction = "all")

  # Get the Jaccard similarity values for
  # nodes `5`, `6`, and `7`; `out` direction
  jaccard_out <-
    get_jaccard_similarity(
      graph = graph,
      nodes = 5:7,
      direction = "out")

  # Get the Jaccard similarity values for
  # nodes `5`, `6`, and `7`; `in` direction
  jaccard_in <-
    get_jaccard_similarity(
      graph = graph,
      nodes = 5:7,
      direction = "in")

  # Expect that a `matrix` object is returned
  expect_true(is.matrix(jaccard_all))
  expect_true(is.matrix(jaccard_out))
  expect_true(is.matrix(jaccard_in))

  # Expect a square matrix of 3 columns and 3 rows
  expect_equal(dim(jaccard_all), c(3, 3))
  expect_equal(dim(jaccard_out), c(3, 3))
  expect_equal(dim(jaccard_in), c(3, 3))

  # Expect all columns to be numeric
  expect_type(jaccard_all[, 1], "double")
  expect_type(jaccard_all[, 2], "double")
  expect_type(jaccard_all[, 3], "double")

  expect_type(jaccard_out[, 1], "double")
  expect_type(jaccard_out[, 2], "double")
  expect_type(jaccard_out[, 3], "double")

  expect_type(jaccard_in[, 1], "double")
  expect_type(jaccard_in[, 2], "double")
  expect_type(jaccard_in[, 3], "double")

  # Expect specific column names in this data frame
  expect_equal(
    colnames(jaccard_all), c("5", "6", "7"))

  expect_equal(
    colnames(jaccard_out), c("5", "6", "7"))

  expect_equal(
    colnames(jaccard_in), c("5", "6", "7"))

  # Expect specific row names in this data frame
  expect_equal(
    rownames(jaccard_all), c("5", "6", "7"))

  expect_equal(
    rownames(jaccard_out), c("5", "6", "7"))

  expect_equal(
    rownames(jaccard_in), c("5", "6", "7"))

  # Expect all values in the matrix to be less than
  # or equal to 1.0
  expect_true(
    all(as.numeric(jaccard_all) <= 1.0))

  expect_true(
    all(as.numeric(jaccard_out) <= 1.0))

  expect_true(
    all(as.numeric(jaccard_in) <= 1.0))

  # Get the Jaccard similarity values for all nodes,
  # all directions
  jaccard_all_all <-
    get_jaccard_similarity(
      graph = graph,
      direction = "all")

  # Expect the same number of matrix columns
  # and rows as there are nodes
  expect_equal(
    ncol(jaccard_all_all), count_nodes(graph = graph))

  expect_equal(
    nrow(jaccard_all_all), count_nodes(graph = graph))

  # Expect an error if one or more node IDs
  # provided are not in the graph
  expect_error(
    get_jaccard_similarity(
      graph = graph,
      nodes = 8:12,
      direction = "all"))
})
