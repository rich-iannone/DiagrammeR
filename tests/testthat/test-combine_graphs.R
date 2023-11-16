# Combining two graph objects into a single graph object

test_that("graphs can be combined", {

  # Create two simple graphs
  nodes_1 <- create_node_df(n = 10)

  edges_1 <-
    create_edge_df(
      from = 1:9,
      to = 2:10)

  graph_1 <-
    create_graph(
      nodes_df = nodes_1,
      edges_df = edges_1)

  nodes_2 <- create_node_df(n = 10)

  edges_2 <-
    create_edge_df(
      from = 1:9,
      to = 2:10)

  graph_2 <-
    create_graph(
      nodes_df = nodes_2,
      edges_df = edges_2)

  # Combine the two graphs
  combined_graph_1 <-
    combine_graphs(graph_1, graph_2)

  # Expect a graph object of class `dgr_graph`
  expect_s3_class(
    combined_graph_1, "dgr_graph")

  # Expect that names in graph object match a
  # prescribed set of names
  expect_in(
    names(combined_graph_1),
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

  # Expect that the `global_attrs` graph component
  # is not NULL
  expect_false(is.null(combined_graph_1$global_attrs))

  # Expect that the `is_graph_directed()` function
  # will return TRUE
  expect_true(is_graph_directed(combined_graph_1))

  # Expect that the `nodes_df` df has 3 columns
  expect_equal(
    ncol(combined_graph_1$nodes_df), 3)

  # Expect that the `nodes_df` df has 20 rows
  expect_equal(
    nrow(combined_graph_1$nodes_df), 20)

  # Expect that the `edges_df` df has 4 columns
  expect_equal(
    ncol(combined_graph_1$edges_df), 4)

  # Expect that the `edges_df` df has 18 rows
  expect_equal(
    nrow(combined_graph_1$edges_df), 18)
})
