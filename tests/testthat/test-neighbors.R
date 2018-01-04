context("Determining neighbors and non-neighbors of nodes")

test_that("Getting all neighbors of one or more nodes is possible", {

  # Create a random, directed graph with 18 nodes
  # and 22 edges
  random_graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 18,
      m = 22,
      set_seed = 23)

  # Find all neighbor nodes for node `5`
  all_nbrs_5 <-
    get_nbrs(
      graph = random_graph,
      nodes = 5)

  # Expect certain nodes as neighbors
  expect_identical(
    all_nbrs_5, 6)

  # Find all neighbor nodes for nodes `5`, `7`,
  # and `15`
  all_nbrs_5_7_15 <-
    get_nbrs(
      graph = random_graph,
      nodes = c(5, 7, 15))

  # Expect certain nodes as neighbors
  expect_identical(
    all_nbrs_5_7_15,
    c(3, 4, 6, 7, 12, 15))

  # Expect an NA value if there are no neighbors
  expect_true(
    is.na(
      get_nbrs(
        graph = random_graph,
        nodes = 17)))
})

test_that("Getting non-neighbors of a node is possible", {

  # Create a random, directed graph
  # with 18 nodes and 22 edges
  random_graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 18,
      m = 22,
      set_seed = 23)

  # Find all non-neighbors of node `5`
  non_nbrs_5 <-
    get_non_nbrs(
      graph = random_graph,
      node = 5)

  # Expect certain nodes that
  # are non-neighbors of node `5`
  expect_equal(
    non_nbrs_5,
    c(1, 2, 3, 4, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18))

  # Expect that NA is returned when
  # there are no non-neighbors
  expect_true(
    create_graph() %>%
      add_path(n = 3) %>%
      get_non_nbrs(node = 2) %>%
      is.na())
})

test_that("Getting common neighbors of 2 or more nodes is possible", {

  # Create a random, directed graph with 18 nodes
  # and 22 edges
  random_graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 18,
      m = 22,
      set_seed = 23)

  # Expect NA when finding all
  # common neighbor nodes for nodes
  # `5` and `7` (there are no
  # common neighbors)
  expect_true(
    is.na(
      get_common_nbrs(
        graph = random_graph,
        nodes = c(5, 7))))

  # Expect a common neighbor
  # node of `4` for nodes
  # `1` and  `7`
  expect_equal(
    get_common_nbrs(
      graph = random_graph,
      nodes = c(1, 7)), 4)
})

test_that("Getting similar neighbors of a node is possible", {

  # Create a random, directed graph with 18 nodes
  # and 22 edges
  random_graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 18,
      m = 22,
      set_seed = 23) %>%
    set_node_attrs(
      node_attr = value,
      values = rnorm(count_nodes(graph = .), 5, 2))

  # Expect NA when searching any nodes adjacent to
  # node `2` and beyond since the immediately adjacent
  # are not numerically equivalent in `value`
  expect_true(
    is.na(
      get_similar_nbrs(
        graph = random_graph,
        node = 2,
        node_attr = value)))

  # Expect several nodes to be
  # matched when using an absolute
  # tolerance range
  expect_identical(
    get_similar_nbrs(
      graph = random_graph,
      node = 8,
      node_attr = value,
      tol_abs = c(2, 2)),
    c(1, 10))

  # Expect all connected nodes
  # to be matched, except the
  # starting node, when using a
  # very high absolute tolerance
  # range
  expect_equal(
    get_similar_nbrs(
      graph = random_graph,
      node = 8,
      node_attr = value,
      tol_abs = c(12, 12)),
    setdiff(1:18, c(8, 17)))

  # Expect certain nodes to be
  # matched, when using tolerance
  # specified as a low and high percentage
  expect_equal(
    get_similar_nbrs(
      graph = random_graph,
      node = 3,
      node_attr = value,
      tol_pct = c(50, 50)),
    c(1, 2, 4, 5, 6, 8, 9, 10, 11, 13, 14, 15, 18))
})
