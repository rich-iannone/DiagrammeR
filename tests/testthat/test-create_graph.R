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

  # Expect that the use of 'is_graph_empty' function will result in TRUE
  expect_true(is_graph_empty(graph))

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

  # Expect that the 'is_graph_directed' function will return TRUE
  expect_true(is_graph_directed(graph))

  # Expect that the Graphviz DOT code for an empty graph
  # is essentially an empty Graphviz diagram
  expect_match(graph$dot_code, "digraph \\{\n\n\\}")
})

test_that("a graph object with nodes can be created correctly", {

  # Create a node data frame
  nodes <-
    create_nodes(nodes = c("a", "b", "c", "d"),
                 label = FALSE,
                 type = "lower",
                 style = "filled",
                 color = "aqua",
                 shape = c("circle", "circle",
                           "rectangle", "rectangle"),
                 data = c(3.5, 2.6, 9.4, 2.7))

  # Create the graph object using the node data frame
  graph <- create_graph(nodes_df = nodes)

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

  # Expect that the 'nodes_df' data frame has 7 columns
  expect_true(ncol(graph$nodes_df) == 7)

  # Expect that the 'nodes_df' data frame has 4 rows
  expect_true(nrow(graph$nodes_df) == 4)
})

test_that("a graph object can be created with a just an edge data frame", {

  # Create an edge data frame
  edges <-
    create_edges(from = c("a", "b", "c"),
                 to = c("d", "c", "a"),
                 rel = "leading_to")

  # Create the graph object using the node and edge data frames
  graph <- create_graph(edges_df = edges)

  # Expect a graph object of class 'dgr_graph'
  expect_true(class(graph) == "dgr_graph")

  # Expect that several of the graph components are NULL
  expect_null(graph$graph_name)
  expect_null(graph$graph_time)
  expect_null(graph$graph_tz)
  expect_null(graph$graph_attrs)
  expect_null(graph$node_attrs)
  expect_null(graph$edge_attrs)

  # Expect that the 'edges_df' component is a data frame
  expect_true(class(graph$edges_df) == "data.frame")

  # Expect that the 'nodes_df' component is not NULL
  expect_true(!is.null(graph$nodes_df))

  # Expect that the 'nodes_df' component is a data frame
  expect_true(class(graph$edges_df) == "data.frame")

  # Expect that the 'nodes_df' data frame has 3 columns
  expect_true(ncol(graph$nodes_df) == 3)

  # Expect that the 'nodes_df' data frame has 4 rows
  expect_true(nrow(graph$nodes_df) == 4)

  # Expect that the 'nodes_df' component contains the node IDs specified
  # in the edge data frame
  expect_equal(graph$nodes_df$nodes,
               unique(c(graph$edges_df$from, graph$edges_df$to)))
})

test_that("a graph object with nodes and edges can be created correctly", {

  # Create a node data frame
  nodes <-
    create_nodes(nodes = c("a", "b", "c", "d"),
                 label = FALSE,
                 type = "lower",
                 style = "filled",
                 color = "aqua",
                 shape = c("circle", "circle",
                           "rectangle", "rectangle"),
                 data = c(3.5, 2.6, 9.4, 2.7))

  # Create an edge data frame
  edges <-
    create_edges(from = c("a", "b", "c"),
                 to = c("d", "c", "a"),
                 rel = "leading_to")

  # Create the graph object using the node and edge data frames
  graph <- create_graph(nodes_df = nodes,
                        edges_df = edges,
                        node_attrs = "fontname = Helvetica",
                        edge_attrs = c("color = blue",
                                       "arrowsize = 2"))

  # Expect a graph object of class 'dgr_graph'
  expect_true(class(graph) == "dgr_graph")

  # Expect that several of the graph components are NULL
  expect_null(graph$graph_name)
  expect_null(graph$graph_time)
  expect_null(graph$graph_tz)
  expect_null(graph$graph_attrs)

  # Expect that the 'nodes_df' component is a data frame
  expect_true(class(graph$nodes_df) == "data.frame")

  # Expect that the 'edges_df' component is a data frame
  expect_true(class(graph$edges_df) == "data.frame")

  # Expect that the 'node_attrs' component is a character vector of length 1
  expect_true(class(graph$node_attrs) == "character")
  expect_equal(length(graph$node_attrs), 1)

  # Expect that the 'edge_attrs' component is a character vector of length 2
  expect_true(class(graph$edge_attrs) == "character")
  expect_equal(length(graph$edge_attrs), 2)

  # Expect that the graph is a directed graph
  expect_true(graph$directed == TRUE)

  # Expect that the 'nodes_df' data frame has 7 columns
  expect_true(ncol(graph$nodes_df) == 7)

  # Expect that the 'nodes_df' data frame has 4 rows
  expect_true(nrow(graph$nodes_df) == 4)

  # Expect that the 'edges_df' data frame has 3 columns
  expect_true(ncol(graph$edges_df) == 3)

  # Expect that the 'edges_df' data frame has 3 rows
  expect_true(nrow(graph$edges_df) == 3)
})

test_that("different combinations of inputs can result in a graph", {

  # Create the graph object with only 'node_attrs'
  graph_n <- create_graph(node_attrs = "fontname = Helvetica")

  # Expect a graph object of class 'dgr_graph'
  expect_true(class(graph_n) == "dgr_graph")

  # Expect that several of the graph components are NULL or not NULL
  expect_null(graph_n$graph_name)
  expect_null(graph_n$graph_time)
  expect_null(graph_n$graph_tz)
  expect_null(graph_n$graph_attrs)
  expect_null(graph_n$edge_attrs)
  expect_false(is.null(graph_n$node_attrs))

  # Create the graph object with only 'edge_attrs'
  graph_e <- create_graph(edge_attrs = "color = blue")

  # Expect a graph object of class 'dgr_graph'
  expect_true(class(graph_e) == "dgr_graph")

  # Expect that several of the graph components are NULL or not NULL
  expect_null(graph_e$graph_name)
  expect_null(graph_e$graph_time)
  expect_null(graph_e$graph_tz)
  expect_null(graph_e$graph_attrs)
  expect_null(graph_e$node_attrs)
  expect_false(is.null(graph_e$edge_attrs))

  # Create the graph object with only 'graph_attrs'
  graph_g <- create_graph(graph_attrs = "layout = circo")

  # Expect a graph object of class 'dgr_graph'
  expect_true(class(graph_g) == "dgr_graph")

  # Expect that several of the graph components are NULL or not NULL
  expect_null(graph_g$graph_name)
  expect_null(graph_g$graph_time)
  expect_null(graph_g$graph_tz)
  expect_null(graph_g$node_attrs)
  expect_null(graph_g$edge_attrs)
  expect_false(is.null(graph_g$graph_attrs))

  # Create the graph object with only 'graph_attrs' and 'edge_attrs'
  graph_ge <- create_graph(graph_attrs = "layout = circo",
                           edge_attrs = "color = blue")

  # Expect a graph object of class 'dgr_graph'
  expect_true(class(graph_ge) == "dgr_graph")

  # Expect that several of the graph components are NULL or not NULL
  expect_null(graph_ge$graph_name)
  expect_null(graph_ge$graph_time)
  expect_null(graph_ge$graph_tz)
  expect_null(graph_ge$node_attrs)
  expect_false(is.null(graph_ge$edge_attrs))
  expect_false(is.null(graph_ge$graph_attrs))

  # Create the graph object with only 'graph_attrs' and 'node_attrs'
  graph_gn <- create_graph(graph_attrs = "layout = circo",
                           node_attrs = "fontname = Helvetica")

  # Expect a graph object of class 'dgr_graph'
  expect_true(class(graph_gn) == "dgr_graph")

  # Expect that several of the graph components are NULL or not NULL
  expect_null(graph_gn$graph_name)
  expect_null(graph_gn$graph_time)
  expect_null(graph_gn$graph_tz)
  expect_null(graph_gn$edge_attrs)
  expect_false(is.null(graph_gn$node_attrs))
  expect_false(is.null(graph_gn$graph_attrs))

  # Create the graph object with only 'node_attrs' and 'edge_attrs'
  graph_ne <- create_graph(node_attrs = "fontname = Helvetica",
                           edge_attrs = "color = blue")

  # Expect a graph object of class 'dgr_graph'
  expect_true(class(graph_ne) == "dgr_graph")

  # Expect that several of the graph components are NULL or not NULL
  expect_null(graph_ne$graph_name)
  expect_null(graph_ne$graph_time)
  expect_null(graph_ne$graph_tz)
  expect_null(graph_ne$graph_attrs)
  expect_false(is.null(graph_ne$node_attrs))
  expect_false(is.null(graph_ne$edge_attrs))
})
