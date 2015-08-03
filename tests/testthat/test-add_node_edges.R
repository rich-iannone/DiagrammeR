context("Adding nodes and/or edges to an existing graph object")

test_that("adding a node to a graph is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add two nodes
  graph <- add_node(graph, node = "a")
  graph <- add_node(graph, node = "b")

  # Expect that names in this graph object match a prescribed set of names
  expect_true(all(names(graph) == c("graph_name", "graph_time", "graph_tz",
                                    "nodes_df", "edges_df", "graph_attrs",
                                    "node_attrs", "edge_attrs", "directed",
                                    "dot_code")))

  # Expect a graph object of class 'dgr_graph'
  expect_true(class(graph) == "dgr_graph")

  # Expect that several of the graph components are NULL
  expect_null(graph$graph_name)
  expect_null(graph$graph_time)
  expect_null(graph$graph_tz)
  expect_null(graph$edges_df)
  expect_null(graph$graph_attrs)
  expect_null(graph$node_attrs)
  expect_null(graph$edge_attrs)

  # Expect that the 'nodes_df' component is a data frame
  expect_true(class(graph$nodes_df) == "data.frame")

  # Expect that the graph is a directed graph
  expect_true(graph$directed == TRUE)

  # Expect that the 'nodes_df' data frame has 2 columns
  expect_true(ncol(graph$nodes_df) == 2L)

  # Expect that the 'nodes_df' data frame has 2 rows
  expect_true(nrow(graph$nodes_df) == 2L)

  # Add a node that already exists in the graph
  graph_2 <- add_node(graph, node = "a")

  # Expect that the graph won't change states
  expect_equal(graph, graph_2)

  # Add a node with attributes to the graph
  graph_3 <- add_node(graph,
                      node = "c",
                      type = "fresh")

  # Expect that there will be 3 nodes in the graph
  expect_equal(node_count(graph_3), 3L)

  # Expect that the "type" value will be present for the node
  # in the new graph
  expect_equal(node_type(graph_3, node = "c"), "fresh")

  # Expect that the other nodes in the graph will still have
  # unassigned "type" values
  expect_true(is.na(node_type(graph_3, node = "a")))
  expect_true(is.na(node_type(graph_3, node = "b")))

  # Create a graph with a single, unlabeled node
  graph_unlabeled <- create_graph()
  graph_unlabeled <- add_node(graph = graph_unlabeled,
                              node = "a", label = FALSE)

  # Expect that the graph will have one unlabeled node
  expect_true(node_info(graph = graph_unlabeled)$label == " ")
})

test_that("adding an edge to a graph is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add two nodes
  graph <- add_node(graph, node = "a")
  graph <- add_node(graph, node = "b")

  # Add an edge
  graph <- add_edges(graph, from = "a", to = "b",
                     relationship = "to_get")

  # Expect that names in this graph object match a prescribed set of names
  expect_true(all(names(graph) == c("graph_name", "graph_time", "graph_tz",
                                    "nodes_df", "edges_df", "graph_attrs",
                                    "node_attrs", "edge_attrs", "directed",
                                    "dot_code")))

  # Expect a graph object of class 'dgr_graph'
  expect_true(class(graph) == "dgr_graph")

  # Expect that several of the graph components are NULL
  expect_null(graph$graph_name)
  expect_null(graph$graph_time)
  expect_null(graph$graph_tz)
  expect_null(graph$graph_attrs)
  expect_null(graph$node_attrs)
  expect_null(graph$edge_attrs)

  # Expect that the 'nodes_df' component is a data frame
  expect_true(class(graph$nodes_df) == "data.frame")

  # Expect that the 'edges_df' component is a data frame
  expect_true(class(graph$edges_df) == "data.frame")

  # Expect that the graph is a directed graph
  expect_true(graph$directed == TRUE)

  # Expect that the 'nodes_df' data frame has 3 columns
  expect_true(ncol(graph$edges_df) == 3L)

  # Expect that the 'nodes_df' data frame has 1 row
  expect_true(nrow(graph$edges_df) == 1L)
})
