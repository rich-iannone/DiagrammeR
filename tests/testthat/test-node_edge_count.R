context("Getting counts of nodes and edges in graph objects")

test_that("getting a node count for a graph is possible", {

  set.seed(26)

  # Create a node data frame
  nodes <-
    create_node_df(
      n = 26,
      label = TRUE,
      type = c(rep("a_to_g", 7),
               rep("h_to_p", 9),
               rep("q_to_x", 8),
               rep("y_and_z",2)))

  # Create an edge data frame
  edges <-
    create_edge_df(
      from = sample(1:26, replace = TRUE),
      to = sample(1:26, replace = TRUE),
      label = "edge",
      rel = "letter_to_letter")

  # Create the graph object using the node and edge data frames
  graph <-
    create_graph(
      nodes_df = nodes,
      edges_df = edges)

  # Obtain a count of nodes by type
  count_of_nodes <-
    node_count(
      graph = graph,
      type = TRUE)

  # Expect that the `count_of_nodes` object is a named vector
  expect_true(
    !is.null(names(count_of_nodes)))

  expect_true(
    all(
      names(count_of_nodes) ==
        c("a_to_g", "h_to_p",
          "q_to_x", "y_and_z")))

  expect_true(
    all(
      c(7, 9, 8, 2) %in% count_of_nodes))

  # Obtain a total count of nodes
  total_count_of_nodes <-
    node_count(
      graph = graph,
      type = FALSE)

  # Expect that the `total_count_of_nodes` object
  # is a named vector
  expect_is(
    total_count_of_nodes, "integer")

  expect_equal(
    total_count_of_nodes, 26)
})

test_that("getting an edge count for a graph is possible", {

  set.seed(26)

  # Create a node data frame
  nodes <-
    create_node_df(
      n = 26,
      label = TRUE,
      type = c(rep("a_to_g", 7),
               rep("h_to_p", 9),
               rep("q_to_x", 8),
               rep("y_and_z",2)))

  # Create an edge data frame
  edges <-
    create_edge_df(
      from = sample(1:26, replace = TRUE),
      to = sample(1:26, replace = TRUE),
      label = "edge",
      rel = "letter_to_letter")

  # Create the graph object using the node and edge data frames
  graph <-
    create_graph(
      nodes_df = nodes,
      edges_df = edges)

  # Obtain a count of nodes by relationship
  count_of_edges <-
    edge_count(
      graph = graph,
      rel = TRUE)

  # Expect that the `count_of_edges` object is a
  # named vector
  expect_true(
    !is.null(names(count_of_edges)))

  # Obtain a total count of edges
  total_count_of_edges <-
    edge_count(
      graph = graph,
      rel = FALSE)

  # Expect that the `total_count_of_edges` object is
  # a named vector
  expect_is(
    total_count_of_edges, "integer")

  expect_equal(
    total_count_of_edges, 26)
})

test_that("getting a node/edge count for an empty graph is possible", {

  # Create an empty graph
  empty_graph <- create_graph()

  # Expect that a node count of an empty graph
  # will return `0`
  expect_equal(
    node_count(
      graph = empty_graph,
      type = FALSE), 0)

  expect_equal(
    node_count(
      graph = empty_graph,
      type = TRUE), 0)

  # Expect that an edge count of an empty graph
  # will return `0`
  expect_equal(
    edge_count(
      graph = empty_graph,
      rel = FALSE), 0)

  expect_equal(
    edge_count(
      graph = empty_graph,
      rel = TRUE), 0)

  # Expect that an edge count with a relationship value
  # set to any character vector will automatically
  # return `0`
  expect_equal(
    edge_count(
      graph = empty_graph,
      rel = "rel"), 0)
})
