context("Perform graph object transformations")

test_that("Converting to igraph is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      10, 22, set_seed = 1)

  igraph_graph <-
    to_igraph(graph)

  # Expect that the new object is an igraph object
  expect_is(igraph_graph, "igraph")
})

test_that("Changing to undirected mode is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      10, 22, set_seed = 1,
      directed = TRUE)

  undirected_graph <-
    set_graph_undirected(graph)

  # Expect the the graph is undirected
  expect_true(graph$directed)
})

test_that("Reversing the graph edges is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      10, 22, set_seed = 1,
      directed = TRUE)

  # Get a vector of `from` nodes
  from_nodes <- graph$edges_df$from

  # Get a vector of `to` nodes
  to_nodes <- graph$edges_df$to

  # Change the edge direction for all edges in
  # the graph
  graph <- reverse_edge_direction(graph)

  # Expect that the `from` nodes in the new graph
  # object are identical to the `to` nodes before
  # reversing the edge direction
  expect_identical(graph$edges_df$from, to_nodes)

  # Expect that the `to` nodes in the new graph
  # object are identical to the `from` nodes before
  # reversing the edge direction
  expect_identical(graph$edges_df$to, from_nodes)
})
