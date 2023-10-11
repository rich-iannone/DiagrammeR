# Getting info about a node's predecessors and successors

test_that("getting a node's predecessors/successors is possible", {

  suppressWarnings(RNGversion("3.5.0"))
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
      from = sample(1:26, replace = FALSE),
      to = sample(1:26, replace = FALSE),
      label = "edge",
      rel = "letter_to_letter")

  # Create the graph object using the node and
  # edge data frames
  graph <-
    create_graph(
      nodes_df = nodes,
      edges_df = edges)

  # Tests for `get_predecessors()`
  expect_type(
    get_predecessors(
      graph = graph,
      node = 26),
    "integer")

  expect_equal(
    get_predecessors(
      graph = graph,
      node = 26), 1)

  # Tests for `get_successors()`
  expect_type(
    get_successors(
      graph = graph,
      node = 1),
    "integer")

  expect_equal(
    get_successors(
      graph = graph,
      node = 1), 26)
})
