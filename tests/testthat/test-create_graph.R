context("Creating a graph object")

test_that("an empty graph object can be created and such an object is correct", {

  # Create an empty graph
  graph <- create_graph()

  # Expect that names in graph object match a prescribed set of names
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
  expect_null(graph$nodes_df)
  expect_null(graph$edges_df)
  expect_null(graph$graph_attrs)
  expect_null(graph$node_attrs)
  expect_null(graph$edge_attrs)

  # Expect that the empty graph is directed by default
  expect_true(graph$directed)

  # Expect that the Graphviz DOT code for an empty graph
  # is essentially an empty Graphviz diagram
  expect_match(graph$dot_code, "digraph \\{\n\n\\}")

})
