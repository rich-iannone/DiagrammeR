# Creating a graph object

test_that("an empty graph object can be created and such an object is correct", {

  # Create an empty graph
  graph <- create_graph()

  # Expect that names in graph object match a
  # prescribed set of names
  expect_named(
    graph,
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

  # Expect a graph object of class `dgr_graph`
  expect_s3_class(graph, "dgr_graph")

  # Expect that the `nodes_df` and  `edges_df` components are
  # a data frame
  expect_s3_class(graph$nodes_df, "data.frame")
  expect_s3_class(graph$edges_df , "data.frame")

  # Expect that the use of `is_graph_empty()` function
  # will result in TRUE
  expect_true(is_graph_empty(graph))

  # Expect that the `global_attrs` component is not NULL
  expect_false(is.null(graph$global_attrs))

  # Expect that the empty graph is directed by default
  expect_true(graph$directed)

  # Expect that the `is_graph_directed()` function
  # will return TRUE
  expect_true(is_graph_directed(graph))
})

test_that("a graph object with nodes can be created correctly", {

  # Create a node data frame
  nodes <-
    create_node_df(
      n = 4,
      type = "lower",
      style = "filled",
      color = "aqua",
      shape = c("circle", "circle",
                "rectangle", "rectangle"),
      data = c(3.5, 2.6, 9.4, 2.7))

  # Create the graph object using the node data frame
  graph <- create_graph(nodes_df = nodes)

  # Expect that names in this graph object match a
  # prescribed set of names
  expect_in(
    names(graph),
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

  # Expect a graph object of class `dgr_graph`
  expect_s3_class(
    graph, "dgr_graph")

  # Expect that several of the graph components
  # are not NULL
  expect_false(is.null(graph$nodes_df))

  expect_false(is.null(graph$global_attrs))

  # Expect that the `nodes_df` and `edges_df` component is
  # a data frame
  expect_s3_class(graph$nodes_df, "data.frame")
  expect_s3_class(graph$edges_df , "data.frame")

  # Expect that the graph is a directed graph
  expect_true(graph$directed)

  # Expect that the `nodes_df` df has 7 columns
  expect_equal(
    ncol(graph$nodes_df), 7)

  # Expect that the `nodes_df` df has 4 rows
  expect_equal(
    nrow(graph$nodes_df), 4)
})

test_that("a graph object with nodes and edges can be created correctly", {

  # Create a node data frame
  nodes <-
    create_node_df(
      n = 4,
      type = "lower",
      style = "filled",
      color = "aqua",
      shape = c("circle", "circle",
                "rectangle", "rectangle"),
      data = c(3.5, 2.6, 9.4, 2.7))

  # Create an edge data frame
  edges <-
    create_edge_df(
      from = c(1, 2, 3),
      to = c(4, 3, 1),
      rel = "leading_to")

  # Create the graph object using the node and
  # edge data frames
  graph <-
    create_graph(
      nodes_df = nodes,
      edges_df = edges)

  # Expect that names in this graph object match a
  # prescribed set of names
  expect_in(
    names(graph),
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

  # Expect a graph object of class `dgr_graph`
  expect_s3_class(
    graph, "dgr_graph")

  # Expect that the `global_attrs` component is not NULL
  expect_false(is.null(graph$global_attrs))

  # Expect that the `nodes_df` component is
  # a data frame
  expect_s3_class(
    graph$nodes_df, "data.frame")

  # Expect that the `edges_df` component is
  # a data frame
  expect_s3_class(
    graph$edges_df , "data.frame")

  # Expect that the graph is a directed graph
  expect_true(graph$directed)

  # Expect that the `nodes_df` df has 7 columns
  expect_equal(
    ncol(graph$nodes_df), 7)

  # Expect that the `nodes_df` df has 4 rows
  expect_equal(
    nrow(graph$nodes_df), 4)

  # Expect that the `edges_df` df has 4 columns
  expect_equal(
    ncol(graph$edges_df), 4)

  # Expect that the `edges_df` df has 3 rows
  expect_equal(
    nrow(graph$edges_df), 3)
})
