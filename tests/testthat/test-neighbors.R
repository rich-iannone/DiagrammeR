context("Determining neighbors and non-neighbors of nodes")

test_that("Getting all neighbors of one or more nodes is possible", {

  # Create a random, directed graph with 18 nodes
  # and 22 edges
  random_graph <-
    create_random_graph(
      n = 18,
      m = 22,
      directed = TRUE,
      fully_connected = TRUE,
      set_seed = 20)

  # Find all neighbor nodes for node `5`
  all_nbrs_5 <- get_nbrs(random_graph, 5)

  # Expect that nodes `1`, `2`, `12`, and `18` are
  # the neighbors of node `5`
  expect_identical(all_nbrs_5, c(1, 2, 12, 18))

  # Find all neighbor nodes for nodes `5`, `7`,
  # and `15`
  all_nbrs_5_7_15 <-
    get_nbrs(random_graph, c(5, 7, 15))

  # Expect that nodes `1`, `2`, `6`, `12`,
  # and `18` are the neighbors of nodes `5`,
  # `7`, and `15`
  expect_identical(all_nbrs_5_7_15,
                   c(1, 2, 6, 12, 18))

  # Expect an NA value if there are no neighbors
  expect_true(is.na(get_nbrs(random_graph, 7)))
})

test_that("Getting non-neighbors of a node is possible", {

  # Create a random, directed graph with 18 nodes
  # and 22 edges
  random_graph <-
    create_random_graph(
      n = 18,
      m = 22,
      directed = TRUE,
      fully_connected = TRUE,
      set_seed = 20)

  # Find all non-neighbors of node `5`
  non_nbrs_5 <- get_non_nbrs(random_graph, 5)

  # Expect certain nodes that are are non-neighbors
  # of node `5`
  expect_equal(
    non_nbrs_5,
    c(3, 4, 6, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17))

  # Expect an NA value if there are no non-neighbors
  expect_true(
    is.na(create_graph() %>% add_node %>% get_non_nbrs(1)))
})

test_that("Getting common neighbors of 2 or more nodes is possible", {

  # Create a random, directed graph with 18 nodes
  # and 22 edges
  random_graph <-
    create_random_graph(
      n = 18,
      m = 22,
      directed = TRUE,
      fully_connected = TRUE,
      set_seed = 20)

  # Expect NA when finding all common neighbor nodes
  # for nodes `5` and `7` (there are no common neighbors)
  expect_true(is.na(get_common_nbrs(random_graph, c(5, 7))))

  # Expect a common neighbor node of `1` for nodes
  # `9` and  `17`
  expect_equal(
    get_common_nbrs(random_graph, c(9, 17)), 1)
})

test_that("Getting similar neighbors of a node is possible", {

  # Create a random, directed graph with 18 nodes
  # and 22 edges
  random_graph <-
    create_random_graph(
      n = 18,
      m = 22,
      directed = TRUE,
      fully_connected = TRUE,
      set_seed = 20)

  # Expect NA when searching any nodes adjacent to
  # node `8` and beyond since the immediately adjacent
  # are not numerically equivalent in `value`
  expect_true(
    is.na(get_similar_nbrs(
      random_graph,
      node = 8,
      node_attr = "value")))

  # Expect several nodes to be matched when using
  # an absolute tolerance range
  expect_identical(
    get_similar_nbrs(
      random_graph,
      node = 8,
      node_attr = "value",
      tol_abs = c(3, 3)),
    c(3, 9, 10, 13, 17, 18))

  # Expect all nodes to be matched, except the
  # starting node when using a very high absolute
  # tolerance range
  expect_identical(
    get_similar_nbrs(
      random_graph,
      node = 8,
      node_attr = "value",
      tol_abs = c(10, 10)),
    c(1, 2, 3, 4, 5, 6, 9, 10, 11,
      12, 13, 14, 15, 16, 17, 18))

  # Expect certain nodes to be matched, when using
  # tolerance specified as a low and high percentage
  expect_identical(
    get_similar_nbrs(
      random_graph,
      node = 3,
      node_attr = "value",
      tol_pct = c(75, 75)),
    c(8, 9, 10, 17, 18))
})
