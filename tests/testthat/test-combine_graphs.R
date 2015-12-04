context("Combining two graph objects into a single graph object")

test_that("graphs can be combined", {

  # Create two simple graphs
  nodes_1 <- create_nodes(nodes = 1:10)
  edges_1 <- create_edges(from = 1:9,
                          to = 2:10)
  graph_1 <- create_graph(nodes_df = nodes_1,
                          edges_df = edges_1)

  nodes_2 <- create_nodes(nodes = 11:20)
  edges_2 <- create_edges(from = 11:19,
                          to = 12:20)
  graph_2 <- create_graph(nodes_df = nodes_2,
                          edges_df = edges_2)

  # Combine the two graphs
  combined_graph_1 <-
    combine_graphs(graph_1, graph_2)

  # Expect a graph object of class 'dgr_graph'
  expect_true(class(combined_graph_1) == "dgr_graph")

  # Expect that names in graph object match a prescribed set of names
  expect_true(all(names(combined_graph_1) ==
                    c("graph_name", "graph_time", "graph_tz",
                      "nodes_df", "edges_df", "graph_attrs",
                      "node_attrs", "edge_attrs", "directed",
                      "dot_code")))

  # Expect that several of the graph components are NULL
  expect_null(combined_graph_1$graph_name)
  expect_null(combined_graph_1$graph_time)
  expect_null(combined_graph_1$graph_tz)
  expect_null(combined_graph_1$graph_attrs)
  expect_null(combined_graph_1$node_attrs)
  expect_null(combined_graph_1$edge_attrs)

  # Expect that the 'is_graph_directed' function will return TRUE
  expect_true(is_graph_directed(combined_graph_1))

  # Expect that the 'nodes_df' data frame has 3 columns
  expect_true(ncol(combined_graph_1$nodes_df) == 3)

  # Expect that the 'nodes_df' data frame has 20 rows
  expect_true(nrow(combined_graph_1$nodes_df) == 20)

  # Expect that the 'edges_df' data frame has 3 columns
  expect_true(ncol(combined_graph_1$edges_df) == 3)

  # Expect that the 'edges_df' data frame has 18 rows
  expect_true(nrow(combined_graph_1$edges_df) == 18)

  # Create an auxiliary edge data frame for creating edges
  # across the two graphs to be combined
  extra_edges <- create_edges(from = c(5, 19, 1),
                              to = c(12, 3, 11))

  # Combine the two graphs with an extra edge data frame
  combined_graph_2 <-
    combine_graphs(graph_1, graph_2, edges_df = extra_edges)

  # Expect that several of the graph components are NULL
  expect_null(combined_graph_2$graph_name)
  expect_null(combined_graph_2$graph_time)
  expect_null(combined_graph_2$graph_tz)
  expect_null(combined_graph_2$graph_attrs)
  expect_null(combined_graph_2$node_attrs)
  expect_null(combined_graph_2$edge_attrs)

  # Expect that the 'is_graph_directed' function will return TRUE
  expect_true(is_graph_directed(combined_graph_2))

  # Expect that the 'nodes_df' data frame has 3 columns
  expect_true(ncol(combined_graph_2$nodes_df) == 3)

  # Expect that the 'nodes_df' data frame has 20 rows
  expect_true(nrow(combined_graph_2$nodes_df) == 20)

  # Expect that the 'edges_df' data frame has 3 columns
  expect_true(ncol(combined_graph_2$edges_df) == 3)

  # Expect that the 'edges_df' data frame has 18 rows
  expect_true(nrow(combined_graph_2$edges_df) == 21)
})
