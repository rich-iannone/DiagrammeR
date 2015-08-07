context("Getting node IDs from the entire graph or within edges")

test_that("getting node IDs from various objects is possible", {

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
                 relationship = "letter_to_letter")

  # Create the graph object using the node and edge data frames
  graph <- create_graph(nodes_df = nodes,
                        edges_df = edges,
                        graph_attrs = "layout = neato",
                        node_attrs = c("fontname = Helvetica",
                                       "shape = circle"))

  # Get information on the graph's nodes
  gotten_nodes <- get_nodes(graph)

  # Expect a character vector object
  expect_is(gotten_nodes, "character")

  # Expect that the character vector object has no names
  expect_null(names(gotten_nodes))

  # Expect a vector that has all LETTERS
  expect_true(all(LETTERS == gotten_nodes))

  # Expect that the same node IDs will be returned from the
  # graph object, the node data frame, and the edge data frame
  expect_equal(get_nodes(graph), get_nodes(nodes))

  # Expect that the number of nodes obtain from the entire graph
  # will be greater than the nodes associated with edges (since there
  # will be free nodes with no edges)
  expect_more_than(length(get_nodes(graph)),
                   length(get_nodes(edges)))
})

test_that("getting node IDs associated within a graph's edges is possible", {

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
                 relationship = "letter_to_letter")

  # Create the graph object using the node and edge data frames
  graph <-
    create_graph(nodes_df = nodes,
                 edges_df = edges,
                 graph_attrs = "layout = neato",
                 node_attrs = c("fontname = Helvetica",
                                "shape = circle"))

  # Get the 'outgoing' and 'incoming' node ID values
  # in a list object
  gotten_edges_list <- get_edges(graph, return_type = "list")

  # Expect a list object
  expect_is(gotten_edges_list, "list")

  # Expect that the list is of length 2
  expect_true(length(gotten_edges_list) == 2L)

  # Expect character vectors of length 26 in 'gotten_edges_list'
  expect_true(length(gotten_edges_list[[1]]) == 26L)
  expect_is(gotten_edges_list[[1]], "character")

  expect_true(length(gotten_edges_list[[2]]) == 26L)
  expect_is(gotten_edges_list[[2]], "character")

  # Get the 'outgoing' and 'incoming' node ID values
  # in a data frame object
  gotten_edges_df <- get_edges(graph, return_type = "df")

  # Expect a data frame object
  expect_is(gotten_edges_df, "data.frame")

  # Expect that the data frame has 2 columns
  expect_true(ncol(gotten_edges_df) == 2L)

  # Expect columns of class 'character' and 26 rows in 'gotten_edges_df'
  expect_is(gotten_edges_df[,1], "character")
  expect_is(gotten_edges_df[,2], "character")
  expect_true(nrow(gotten_edges_df) == 26L)

  # Get the 'outgoing' and 'incoming' node ID values
  # in a vector object
  gotten_edges_vector <- get_edges(graph, return_type = "vector")

  # Expect a vector object of class 'character'
  expect_is(gotten_edges_vector, "character")

  # Expect that the vector object is of length 26
  expect_true(length(gotten_edges_vector) == 26L)

  # Expect that the ' -> ' substring is in each vector component
  expect_true(all(grepl(" -> ", gotten_edges_vector)))
})
