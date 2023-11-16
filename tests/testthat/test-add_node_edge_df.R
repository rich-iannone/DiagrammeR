# Adding node and/or edge data frames to an existing graph object

test_that("adding a node df to a graph is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Create a node data frame
  nodes <-
    create_node_df(
      n = 4,
      type = "letter",
      color = c("red", "green", "grey", "blue"),
      value = c(3.5, 2.6, 9.4, 2.7))

  # Add the node data frame to the graph object to
  # create a graph with nodes
  graph_2 <-
    add_node_df(
      graph = graph,
      node_df = nodes)

  # Create another node data frame
  nodes_2 <-
    create_node_df(
      n = 4,
      type = "letter",
      color = c("white", "brown", "aqua", "pink"),
      value = c(1.6, 6.4, 0.8, 4.2))

  # Add the second node data frame to the graph object
  # to add more nodes with attributes to the graph
  graph_3 <-
    add_node_df(
      graph = graph_2,
      node_df = nodes_2)

  # Expect that names in these graph objects match a
  # prescribed set of names
  expect_named(
    graph_2,
    c(
      "graph_info",
      "nodes_df",
      "edges_df",
      "global_attrs",
      "directed",
      "last_node",
      "last_edge",
      "node_selection",
      "edge_selection",
      "cache",
      "graph_actions",
      "graph_log"
    )
  )

  expect_named(
    graph_3,
    c(
      "graph_info",
      "nodes_df",
      "edges_df",
      "global_attrs",
      "directed",
      "last_node",
      "last_edge",
      "node_selection",
      "edge_selection",
      "cache",
      "graph_actions",
      "graph_log"
    )
  )

  # Expect graph objects of class `dgr_graph`
  expect_s3_class(graph_2, "dgr_graph")
  expect_s3_class(graph_3, "dgr_graph")

  # Expect that the `nodes_df` component is a data frame
  expect_s3_class(graph_2$nodes_df, "data.frame")
  expect_s3_class(graph_3$nodes_df, "data.frame")

  # Expect that the `nodes_df` data frame has 4 and 8 rows
  expect_equal(nrow(graph_2$nodes_df), 4)
  expect_equal(nrow(graph_3$nodes_df), 8)

  # Expect that the `nodes_df` data frame has 5 columns
  expect_equal(ncol(graph_2$nodes_df), 5)
  expect_equal(ncol(graph_3$nodes_df), 5)
})

test_that("adding an edge df to a graph is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Create a node data frame
  nodes <-
    create_node_df(
      n = 4,
      type = "letter",
      color = c("red", "green", "grey", "blue"),
      value = c(3.5, 2.6, 9.4, 2.7))

  # Add the node data frame to the graph object to create a
  # graph with nodes
  graph_2 <-
    add_node_df(
      graph = graph,
      node_df = nodes)

  # Create another node data frame
  nodes_2 <-
    create_node_df(
      n = 4,
      type = "letter",
      color = c("white", "brown", "aqua", "pink"),
      value = c(1.6, 6.4, 0.8, 4.2))

  # Add the second node data frame to the graph object to
  # add more nodes with attributes to the graph
  graph_3 <-
    add_node_df(
      graph = graph_2,
      node_df = nodes_2)

  # Create an edge data frame
  edges <-
    create_edge_df(
      from = c(1, 2, 3),
      to = c(4, 3, 1),
      rel = "leading_to")

  # Adding an edge df to an empty graph results in an error
  expect_error(
    add_edge_df(
      graph = create_graph(),
      edge_df = edges))

  # Add the edge data frame to the graph
  graph_3 <-
    add_edge_df(
      graph = graph_3,
      edge_df = edges)

  # Expect a graph object of class `dgr_graph`
  expect_s3_class(
    graph_3, "dgr_graph")

  # Expect that the `edges_df` component is a data frame
  expect_s3_class(
    graph_3$edges_df, "data.frame")

  # Expect that the `edges_df` data frame has 3 rows
  expect_equal(
    nrow(graph_3$edges_df), 3)

  # Expect that the `edges_df` data frame has 4 columns
  expect_equal(
    ncol(graph_3$edges_df), 4)

  # Add another edge to a graph that already has some
  # edges defined
  graph_3 <-
    add_edge_df(
      graph = graph_3,
      edge_df = create_edge_df(from = 2, to = 4))

  # Expect that the `edges_df` data frame has 4 rows
  expect_equal(
    nrow(graph_3$edges_df), 4)
})
