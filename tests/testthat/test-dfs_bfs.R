context("Performing the dfs and bfs algorithms")

test_that("the dfs algorithm is functional", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 10, m = 10,
      set_seed = 23)

  # Perform a depth-first search of the graph,
  # beginning at the root node `1` (the default
  # `direction = "all"` doesn't take edge
  # direction into account)
  dfs_all <- graph %>% do_dfs(1)

  # Expect that an integer vector is returned
  expect_is(dfs_all, "integer")

  # Expect the length of the vector to be the
  # same as the number of nodes in the graph
  expect_equal(length(dfs_all), node_count(graph))

  # Expect that certain values are returned
  expect_equal(
    dfs_all,
    c(1, 7, 2, 10, 5, 6, 9, 3, 4, 8))

  # If not specifying a starting node, the function
  # will begin the search from a random node
  dfs_all_no_start <- graph %>% do_dfs()

  # Expect that an integer vector is returned
  expect_is(dfs_all_no_start, "integer")

  # Expect the length of the vector to be the
  # same as the number of nodes in the graph
  expect_equal(length(dfs_all_no_start), node_count(graph))

  # Using `direction = "in"` to cause the dfs
  # routine to visit nodes along inward edges
  dfs_in <- graph %>% do_dfs(1, "in")

  # Expect that an integer vector is returned
  expect_is(dfs_in, "integer")

  # Expect the length of the vector to be the
  # same as the number of nodes in the graph
  expect_equal(length(dfs_in), node_count(graph))

  # Expect that certain values are returned
  expect_equal(
    dfs_in,
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

  # Using `direction = "out"` results in the dfs
  # moving along solely outward edges
  dfs_out <- graph %>% do_dfs(1, "out")

  # Expect that an integer vector is returned
  expect_is(dfs_out, "integer")

  # Expect the length of the vector to be the
  # same as the number of nodes in the graph
  expect_equal(length(dfs_out), node_count(graph))

  # Expect that certain values are returned
  expect_equal(
    dfs_out,
    c(1, 7, 9, 2, 10, 3, 4, 5, 6, 8))

  # Expect an error if performing dfs without
  # a node data frame in the graph
  expect_error(create_graph() %>% do_dfs())

  # Expect an error if an invalid `direction`
  # value provided
  expect_error(graph %>% do_dfs(direction = "any"))
})

test_that("the bfs algorithm is functional", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 15, m = 15,
      set_seed = 23)

  # Perform a breadth-first search of the graph,
  # beginning at the root node `1` (the default
  # `direction = "all"` doesn't take edge
  # direction into account)
  bfs_all <- graph %>% do_bfs(1)

  # Expect that an integer vector is returned
  expect_is(bfs_all, "integer")

  # Expect the length of the vector to be the
  # same as the number of nodes in the graph
  expect_equal(length(bfs_all), node_count(graph))

  # Expect that certain values are returned
  expect_equal(
    bfs_all,
    c(1, 12, 15, 14, 3, 5, 8, 2, 4, 9, 6, 7, 10, 11, 13))

  # If not specifying a starting node, the function
  # will begin the search from a random node
  bfs_all_no_start <- graph %>% do_bfs()

  # Expect that an integer vector is returned
  expect_is(bfs_all_no_start, "integer")

  # Expect the length of the vector to be the
  # same as the number of nodes in the graph
  expect_equal(length(bfs_all_no_start), node_count(graph))

  # Using `direction = "in"` to cause the bfs
  # routine to visit nodes along inward edges
  bfs_in <- graph %>% do_bfs(1, "in")

  # Expect that an integer vector is returned
  expect_is(bfs_in, "integer")

  # Expect the length of the vector to be the
  # same as the number of nodes in the graph
  expect_equal(length(bfs_in), node_count(graph))

  # Expect that certain values are returned
  expect_equal(
    bfs_in,
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))

  # Using `direction = "out"` results in the bfs
  # moving along solely outward edges
  bfs_out <- graph %>% do_bfs(1, "out")

  # Expect that an integer vector is returned
  expect_is(bfs_out, "integer")

  # Expect the length of the vector to be the
  # same as the number of nodes in the graph
  expect_equal(length(bfs_out), node_count(graph))

  # Expect that certain values are returned
  expect_equal(
    bfs_out,
    c(1, 12, 15, 2, 3, 4, 14, 5, 9, 6, 8, 10, 11, 13, 7))

  # Expect an error if performing bfs without
  # a node data frame in the graph
  expect_error(create_graph() %>% do_bfs())

  # Expect an error if an invalid `direction`
  # value provided
  expect_error(graph %>% do_bfs(direction = "any"))
})
