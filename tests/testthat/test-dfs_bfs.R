# Performing the dfs and bfs algorithms

test_that("the dfs algorithm is functional", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 10,
      set_seed = 23)

  # Perform a depth-first search of the graph,
  # beginning at the root node `1` (the default
  # `direction = "all"` doesn't take edge
  # direction into account)
  dfs_all <-
    graph %>%
    do_dfs(node = 1)

  # Expect that an integer vector is returned
  expect_type(dfs_all, "integer")

  # Expect the length of the vector to be the
  # same as the number of nodes in the graph
  expect_length(
    dfs_all, count_nodes(graph = graph))

  # Expect that certain values are returned
  expect_equal(
    dfs_all,
    c(1, 5, 4, 8, 3, 2, 6, 10, 9, 7))

  # If not specifying a starting node, the function
  # will begin the search from a random node
  dfs_all_no_start <-
    graph %>%
    do_dfs()

  # Expect that an integer vector is returned
  expect_type(dfs_all_no_start, "integer")

  # Expect the length of the vector to be the
  # same as the number of nodes in the graph
  expect_length(
    dfs_all_no_start,
    count_nodes(graph = graph))

  # Using `direction = "in"` to cause the dfs
  # routine to visit nodes along inward edges
  dfs_in <-
    graph %>%
    do_dfs(
      node = 1,
      direction = "in")

  # Expect that an integer vector is returned
  expect_type(dfs_in, "integer")

  # Expect the length of the vector to be the
  # same as the number of nodes in the graph
  expect_length(
    dfs_in,
    count_nodes(graph = graph))

  # Expect that certain values are returned
  expect_equal(
    dfs_in,
    c(1, 5, 2, 3, 10, 4, 6, 7, 8, 9))

  # Using `direction = "out"` results in the dfs
  # moving along solely outward edges
  dfs_out <-
    graph %>%
    do_dfs(
      node = 1,
      direction = "out")

  # Expect that an integer vector is returned
  expect_type(dfs_out, "integer")

  # Expect the length of the vector to be the
  # same as the number of nodes in the graph
  expect_length(
    dfs_out,
    count_nodes(graph = graph))

  # Expect that certain values are returned
  expect_equal(
    dfs_out,
    c(1, 2, 3, 6, 8, 10, 4, 9, 5, 7))

  # Expect an error if performing dfs without
  # a node data frame in the graph
  expect_error(
    create_graph() %>%
      do_dfs())

  # Expect an error if an invalid `direction`
  # value provided
  expect_error(
    graph %>%
      do_dfs(direction = "any"))
})

test_that("the bfs algorithm is functional", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 10,
      set_seed = 23)

  # Perform a breadth-first search of the graph,
  # beginning at the root node `1` (the default
  # `direction = "all"` doesn't take edge
  # direction into account)
  bfs_all <-
    graph %>%
    do_bfs(node = 1)

  # Expect that an integer vector is returned
  expect_type(bfs_all, "integer")

  # Expect the length of the vector to be the
  # same as the number of nodes in the graph
  expect_length(
    bfs_all,
    count_nodes(graph = graph))

  # Expect that certain values are returned
  expect_equal(
    bfs_all,
    c(1, 5, 4, 10, 8, 9, 3, 2, 6, 7))

  # If not specifying a starting node, the function
  # will begin the search from a random node
  bfs_all_no_start <-
    graph %>%
    do_bfs()

  # Expect that an integer vector is returned
  expect_type(bfs_all_no_start, "integer")

  # Expect the length of the vector to be the
  # same as the number of nodes in the graph
  expect_length(
    bfs_all_no_start,
    count_nodes(graph = graph))

  # Using `direction = "in"` to cause the bfs
  # routine to visit nodes along inward edges
  bfs_in <-
    graph %>%
    do_bfs(
      node = 1,
      direction = "in")

  # Expect that an integer vector is returned
  expect_type(bfs_in, "integer")

  # Expect the length of the vector to be the
  # same as the number of nodes in the graph
  expect_length(
    bfs_in,
    count_nodes(graph = graph))

  # Expect that certain values are returned
  expect_equal(
    bfs_in,
    c(1, 5, 2, 3, 10, 4, 6, 7, 8, 9))

  # Using `direction = "out"` results in the bfs
  # moving along solely outward edges
  bfs_out <-
    graph %>%
    do_bfs(
      node = 1,
      direction = "out")

  # Expect that an integer vector is returned
  expect_type(bfs_out, "integer")

  # Expect the length of the vector to be the
  # same as the number of nodes in the graph
  expect_length(
    bfs_out,
    count_nodes(graph = graph))

  # Expect that certain values are returned
  expect_equal(
    bfs_out,
    c(1, 2, 3, 6, 8, 10, 4, 9, 5, 7))

  # Expect an error if performing bfs without
  # a node data frame in the graph
  expect_error(
    create_graph() %>%
      do_bfs())

  # Expect an error if an invalid `direction`
  # value provided
  expect_error(
    graph %>%
      do_bfs(direction = "any"))
})
