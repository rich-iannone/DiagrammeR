context("Perform graph object transformations")

test_that("Converting to igraph is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 23)

  # Create an igraph object from a
  # DiagrammeR graph object
  igraph_graph <- to_igraph(graph)

  # Expect that the new object is an igraph object
  expect_is(
    igraph_graph, "igraph")
})

test_that("Changing to undirected mode is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 23,
      directed = TRUE)

  undirected_graph <-
    set_graph_undirected(graph)

  # Expect the the graph is undirected
  expect_true(
    graph$directed)
})

test_that("Reversing the graph edges is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 23,
      directed = TRUE)

  # Get a vector of `from` nodes
  from_nodes <- graph$edges_df$from

  # Get a vector of `to` nodes
  to_nodes <- graph$edges_df$to

  # Change the edge direction for all edges in
  # the graph
  graph <- rev_edge_dir(graph)

  # Expect that the `from` nodes in the new graph
  # object are identical to the `to` nodes before
  # reversing the edge direction
  expect_equal(
    graph$edges_df$from, to_nodes)

  # Expect that the `to` nodes in the new graph
  # object are identical to the `from` nodes before
  # reversing the edge direction
  expect_equal(
    graph$edges_df$to, from_nodes)

  # Expect an error if reversing edge direction
  # in an undirected graph
  expect_error(
    create_random_graph(
      n = 5, m = 5,
      directed = FALSE) %>%
      rev_edge_dir())
})

test_that("Creating a complement graph is possible", {

  # Create a simple graph with a single cycle
  graph <-
    create_graph() %>%
    add_cycle(n = 4)

  # Create the complement of the graph
  graph_c <- create_complement_graph(graph)

  # Expect 8 edges in the complement graph
  expect_equal(
    graph_c %>%
      get_edge_df() %>%
      nrow(), 8)

  # Expect that there are no loops in the
  # complement graph
  expect_equal(
    graph_c %>%
      get_edge_df() %>%
      filter(from == to) %>%
      nrow(), 0)

  # Create the complement of the original graph
  # with loops created
  graph_cl <-
    create_complement_graph(
      graph = graph,
      loops = TRUE)

  # Expect 12 edges in this complement graph
  expect_equal(
    graph_cl %>%
      get_edge_df() %>%
      nrow(), 12)
})
