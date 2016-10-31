context("Combining two graph objects into a single graph object")

test_that("graphs can be combined", {

  # Create two simple graphs
  nodes_1 <- create_node_df(10)

  edges_1 <-
    create_edge_df(
      from = 1:9,
      to = 2:10)

  graph_1 <-
    create_graph(
      nodes_df = nodes_1,
      edges_df = edges_1)

  nodes_2 <- create_node_df(10)

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
  expect_is(combined_graph_1, "dgr_graph")

  # Expect that names in graph object match a
  # prescribed set of names
  expect_true(
    all(names(combined_graph_1) ==
          c("graph_name", "graph_time", "graph_tz",
            "nodes_df", "edges_df", "graph_attrs",
            "node_attrs", "edge_attrs", "global_attrs",
            "directed", "last_node")))

  # Expect that several of the graph
  # components are NULL
  expect_null(combined_graph_1$graph_name)
  expect_null(combined_graph_1$graph_time)
  expect_null(combined_graph_1$graph_tz)

  # Expect that several of the graph components
  # are not NULL
  expect_true(!is.null(combined_graph_1$graph_attrs))
  expect_true(!is.null(combined_graph_1$node_attrs))
  expect_true(!is.null(combined_graph_1$edge_attrs))

  # Expect that the `is_graph_directed()` function
  # will return TRUE
  expect_true(is_graph_directed(combined_graph_1))

  # Expect that the `nodes_df` df has 3 columns
  expect_true(ncol(combined_graph_1$nodes_df) == 3)

  # Expect that the `nodes_df` df has 20 rows
  expect_true(nrow(combined_graph_1$nodes_df) == 20)

  # Expect that the `edges_df` df has 3 columns
  expect_true(ncol(combined_graph_1$edges_df) == 3)

  # Expect that the `edges_df` df has 18 rows
  expect_true(nrow(combined_graph_1$edges_df) == 18)
})
