context("Similarity measures for whole graphs")

test_that("the dice similarity algorithm is functional", {

  # Create a random, directed graph with 10 nodes and 15 edges
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 1)

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
  expect_is(
    dice_all, "matrix")

  expect_is(
    dice_out, "matrix")

  expect_is(
    dice_in, "matrix")

  # Expect a square matrix of 3 columns and 3 rows
  expect_equal(
    ncol(dice_all), 3)

  expect_equal(
    nrow(dice_all), 3)

  expect_equal(
    ncol(dice_out), 3)

  expect_equal(
    nrow(dice_out), 3)

  expect_equal(
    ncol(dice_in), 3)

  expect_equal(
    nrow(dice_in), 3)

  # Expect all columns to be numeric
  expect_true(
    inherits(dice_all[, 1], "numeric"))

  expect_true(
    inherits(dice_all[, 2], "numeric"))

  expect_true(
    inherits(dice_all[, 3], "numeric"))

  expect_true(
    inherits(dice_out[, 1], "numeric"))

  expect_true(
    inherits(dice_out[, 2], "numeric"))

  expect_true(
    inherits(dice_out[, 3], "numeric"))

  expect_true(
    inherits(dice_in[, 1], "numeric"))

  expect_true(
    inherits(dice_in[, 2], "numeric"))

  expect_true(
    inherits(dice_in[, 3], "numeric"))

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
    ncol(dice_all_all), node_count(graph))

  expect_equal(
    nrow(dice_all_all), node_count(graph))

  # Expect an error if one or more node IDs
  # provided are not in the graph
  expect_error(
    get_dice_similarity(
      graph = graph,
      nodes = 8:12,
      direction = "all"))
})

test_that("the Jaccard similarity algorithm is functional", {

  # Create a random, directed graph with 10 nodes and 15 edges
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 1)

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
  expect_is(
    jaccard_all, "matrix")

  expect_is(
    jaccard_out, "matrix")

  expect_is(
    jaccard_in, "matrix")

  # Expect a square matrix of 3 columns and 3 rows
  expect_equal(
    ncol(jaccard_all), 3)

  expect_equal(
    nrow(jaccard_all), 3)

  expect_equal(
    ncol(jaccard_out), 3)

  expect_equal(
    nrow(jaccard_out), 3)

  expect_equal(
    ncol(jaccard_in), 3)

  expect_equal(
    nrow(jaccard_in), 3)

  # Expect all columns to be numeric
  expect_true(
    inherits(jaccard_all[, 1], "numeric"))

  expect_true(
    inherits(jaccard_all[, 2], "numeric"))

  expect_true(
    inherits(jaccard_all[, 3], "numeric"))

  expect_true(
    inherits(jaccard_out[, 1], "numeric"))

  expect_true(
    inherits(jaccard_out[, 2], "numeric"))

  expect_true(
    inherits(jaccard_out[, 3], "numeric"))

  expect_true(
    inherits(jaccard_in[, 1], "numeric"))

  expect_true(
    inherits(jaccard_in[, 2], "numeric"))

  expect_true(
    inherits(jaccard_in[, 3], "numeric"))

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
    ncol(jaccard_all_all), node_count(graph))

  expect_equal(
    nrow(jaccard_all_all), node_count(graph))

  # Expect an error if one or more node IDs
  # provided are not in the graph
  expect_error(
    get_jaccard_similarity(
      graph = graph,
      nodes = 8:12,
      direction = "all"))
})
