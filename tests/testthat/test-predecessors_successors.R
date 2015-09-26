context("Getting info about a node's predecessors and successors")

test_that("getting a node's predecessors/successors is possible", {

  set.seed(26)

  # Create a node data frame
  nodes <-
    create_nodes(nodes = LETTERS,
                 label = TRUE,
                 type = c(rep("a_to_g", 7),
                          rep("h_to_p", 9),
                          rep("q_to_x", 8),
                          rep("y_and_z",2)))

  # Create an edge data frame
  edges <-
    create_edges(from = sample(LETTERS, replace = TRUE),
                 to = sample(LETTERS, replace = TRUE),
                 label = "edge",
                 rel = "letter_to_letter")

  # Create the graph object using the node and edge data frames
  graph <- create_graph(nodes_df = nodes,
                        edges_df = edges,
                        graph_attrs = "layout = neato",
                        node_attrs = c("fontname = Helvetica",
                                       "shape = circle"))

  # Tests for 'get_predecessors'
  expect_true(is.na(get_predecessors(graph, node = "A")))
  expect_is(get_predecessors(graph, node = "Z"), "character")
  expect_equal(get_predecessors(graph, node = "Z"), c("A", "R", "R"))

  # Tests for 'get_successors'
  expect_is(get_successors(graph, node = "A"), "character")
  expect_equal(get_successors(graph, node = "A"), c("Z", "Y"))
  expect_true(is.na(get_successors(graph, node = "Z")))
})
