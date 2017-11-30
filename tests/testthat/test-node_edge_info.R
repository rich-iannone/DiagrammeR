context("Getting info about nodes and edges")

test_that("getting info about a graph's nodes is possible", {

  set.seed(26)

  # Create a node data frame
  nodes <-
    create_node_df(
      n = 26,
      label = LETTERS,
      type = c(rep("a_to_g", 7),
               rep("h_to_p", 9),
               rep("q_to_x", 8),
               rep("y_and_z", 2)))

  # Create an edge data frame
  edges <-
    create_edge_df(
      from = sample(1:26, replace = TRUE),
      to = sample(1:26, replace = TRUE),
      label = "edge",
      rel = "letter_to_letter")

  # Create the graph object using the node and
  # edge data frames
  graph <-
    create_graph(
      nodes_df = nodes,
      edges_df = edges)

  # Get information on the graph's nodes
  info_nodes <- node_info(graph)

  # Expect a data frame object
  expect_is(
    info_nodes, "data.frame")

  # Expect that the data frame has 7 columns
  expect_true(
    ncol(info_nodes) == 7)

  # Expect that the data frame has 26 rows
  expect_true(
    nrow(info_nodes) == 26)

  # Expect that certain columns will be classed
  # as `integer` or `character`
  expect_is(
    info_nodes$id, "integer")

  expect_is(
    info_nodes$label, "character")

  expect_is(
    info_nodes$type, "character")

  # Expect that certain columns will be classed
  # as `numeric`
  expect_is(
    info_nodes$deg, "numeric")

  expect_is(
    info_nodes$indeg, "numeric")

  expect_is(
    info_nodes$outdeg, "numeric")

  expect_is(
    info_nodes$loops, "numeric")

  # Create a graph with 4 nodes (with type
  # information) but no edges
  graph <-
    create_graph() %>%
    add_node(type = "free") %>%
    add_node(type = "free") %>%
    add_node(type = "free") %>%
    add_node(type = "free")

  # Get information on nodes that have no edges
  info_nodes_no_edges <- node_info(graph)

  # Expect a data frame object
  expect_is(
    info_nodes_no_edges, "data.frame")

  # Expect that the data frame has 7 columns
  expect_true(
    ncol(info_nodes_no_edges) == 7)

  # Expect that the data frame has 4 rows
  expect_true(
    nrow(info_nodes_no_edges) == 4)

  # Expect that the `deg`, `indeg`, `outdeg`,
  # and `loops` columns have show 0
  expect_equal(
    info_nodes_no_edges$deg %>%
      unique, 0)

  expect_equal(
    info_nodes_no_edges$indeg %>%
      unique, 0)

  expect_equal(
    info_nodes_no_edges$outdeg %>%
      unique, 0)

  expect_equal(
    info_nodes_no_edges$loops %>%
      unique, 0)

  # Create an empty graph
  graph <- create_graph()

  # Get information on nodes from the empty graph
  info_nodes_empty_graph <- node_info(graph)

  # Expect a NULL value
  expect_null(
    info_nodes_empty_graph)
})

test_that("getting info about a graph's edges is possible", {

  set.seed(26)

  # Create a node data frame
  nodes <-
    create_node_df(
      n = 26,
      label = LETTERS,
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

  # Create the graph object using the node and
  # edge data frames
  graph <-
    create_graph(
      nodes_df = nodes,
      edges_df = edges)

  # Get information on the graph's edges
  info_edges <- edge_info(graph)

  # Expect a data frame object
  expect_is(
    info_edges, "data.frame")

  # Expect that the data frame has 4 columns
  expect_true(
    ncol(info_edges) == 4)

  # Expect that the data frame has 26 rows
  expect_true(
    nrow(info_edges) == 26)

  # Expect that columns will be classed
  # as either as `integer` or `character`
  expect_is(
    info_edges$from, "integer")

  expect_is(
    info_edges$to, "integer")

  expect_is(
    info_edges$rel, "character")

  # Create a graph with 4 nodes but no edges
  graph <-
    create_graph() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_node()

  # Get information on a graph that has no edges
  info_graph_no_edges <- edge_info(graph)

  # Expect an NA value
  expect_true(
    is.na(info_graph_no_edges))
})
