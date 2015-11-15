context("Getting info about nodes and edges")

test_that("getting info about a graph's nodes is possible", {

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

  # Get information on the graph's nodes
  info_nodes <- node_info(graph)

  # Expect a data frame object
  expect_true(class(info_nodes) == "data.frame")

  # Expect that the data frame has 7 columns
  expect_true(ncol(info_nodes) == 7)

  # Expect that the data frame has 26 rows
  expect_true(nrow(info_nodes) == 26)

  # Expect that certain columns will be classed as "character"
  expect_is(info_nodes$node, "character")
  expect_is(info_nodes$label, "character")
  expect_is(info_nodes$type, "character")

  # Expect that certain columns will be classed as "numeric"
  expect_is(info_nodes$degree, "numeric")
  expect_is(info_nodes$indegree, "numeric")
  expect_is(info_nodes$outdegree, "numeric")
  expect_is(info_nodes$loops, "numeric")

  # Create a graph with 4 nodes (with type information) but no edges
  graph <- create_graph()
  graph <- add_node(graph, node = "a", type = "free")
  graph <- add_node(graph, node = "b", type = "free")
  graph <- add_node(graph, node = "c", type = "free")
  graph <- add_node(graph, node = "d", type = "free")

  # Get information on nodes that have no edges
  info_nodes_no_edges <- node_info(graph)

  # Expect a data frame object
  expect_true(class(info_nodes_no_edges) == "data.frame")

  # Expect that the data frame has 7 columns
  expect_true(ncol(info_nodes_no_edges) == 7)

  # Expect that the data frame has 4 rows
  expect_true(nrow(info_nodes_no_edges) == 4)

  # Create an empty graph
  graph <- create_graph()

  # Get information on nodes from the empty graph
  info_nodes_empty_graph <- node_info(graph)

  # Expect a data frame object
  expect_true(class(info_nodes_empty_graph) == "data.frame")

  # Expect that the data frame has 7 columns
  expect_true(ncol(info_nodes_empty_graph) == 7)

  # Expect that the data frame has 4 rows
  expect_true(nrow(info_nodes_empty_graph) == 0)
})

test_that("getting info about a graph's edges is possible", {

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

  # Get information on the graph's edges
  info_edges <- edge_info(graph)

  # Expect a data frame object
  expect_true(class(info_edges) == "data.frame")

  # Expect that the data frame has 3 columns
  expect_true(ncol(info_edges) == 3)

  # Expect that the data frame has 26 rows
  expect_true(nrow(info_edges) == 26)

  # Expect that all columns will be classed as "character"
  expect_is(info_edges$from, "character")
  expect_is(info_edges$to, "character")
  expect_is(info_edges$rel, "character")

  # Create a graph with 4 nodes but no edges
  graph <- create_graph()
  graph <- add_node(graph, node = "a")
  graph <- add_node(graph, node = "b")
  graph <- add_node(graph, node = "c")
  graph <- add_node(graph, node = "d")

  info_graph_no_edges <- edge_info(graph)

  # Expect a data frame object
  expect_true(class(info_graph_no_edges) == "data.frame")

  # Expect that the data frame has 3 columns
  expect_true(ncol(info_graph_no_edges) == 3)

  # Expect that the data frame has no rows
  expect_true(nrow(info_graph_no_edges) == 0)
})
