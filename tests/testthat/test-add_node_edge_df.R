context("Adding node and/or edge data frames to an existing graph object")

test_that("adding a node df to a graph is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Create a node data frame
  nodes <-
    create_nodes(nodes = c("a", "b", "c", "d"),
                 type = "letter",
                 color = c("red", "green", "grey", "blue"),
                 value = c(3.5, 2.6, 9.4, 2.7))

  # Add the node data frame to the graph object to create a
  # graph with nodes
  graph_2 <- add_node_df(graph = graph, node_df = nodes)

  # Create another node data frame
  nodes_2 <-
    create_nodes(nodes = c("e", "f", "g", "h"),
                 type = "letter",
                 color = c("white", "brown", "aqua", "pink"),
                 value = c(1.6, 6.4, 0.8, 4.2))

  # Add the second node data frame to the graph object to
  # add more nodes with attributes to the graph
  graph_3 <- add_node_df(graph = graph_2, node_df = nodes_2)

  # Expect that names in these graph objects match a prescribed set of names
  expect_true(all(names(graph_2) == c("graph_name", "graph_time", "graph_tz",
                                      "nodes_df", "edges_df", "graph_attrs",
                                      "node_attrs", "edge_attrs", "directed",
                                      "dot_code")))

  expect_true(all(names(graph_3) == c("graph_name", "graph_time", "graph_tz",
                                      "nodes_df", "edges_df", "graph_attrs",
                                      "node_attrs", "edge_attrs", "directed",
                                      "dot_code")))

  # Expect graph objects of class 'dgr_graph'
  expect_true(class(graph_2) == "dgr_graph")
  expect_true(class(graph_3) == "dgr_graph")

  # Expect that the 'nodes_df' component is a data frame
  expect_true(class(graph_2$nodes_df) == "data.frame")
  expect_true(class(graph_3$nodes_df) == "data.frame")

  # Expect that the 'nodes_df' data frame has 4 and 8 rows
  expect_true(nrow(graph_2$nodes_df) == 4)
  expect_true(nrow(graph_3$nodes_df) == 8)

  # Expect that the 'nodes_df' data frame has 5 columns
  expect_true(ncol(graph_2$nodes_df) == 5)
  expect_true(ncol(graph_3$nodes_df) == 5)

  # Expect an error if adding a node data frame that contains one or more
  # nodes already in the graph
  expect_error(
    add_node_df(graph = graph_3, node_df = create_nodes("a"))
  )
})

test_that("adding an edge df to a graph is possible", {

  graph <- create_graph()

  # Create a node data frame
  nodes <-
    create_nodes(nodes = c("a", "b", "c", "d"),
                 type = "letter",
                 color = c("red", "green", "grey", "blue"),
                 value = c(3.5, 2.6, 9.4, 2.7))

  # Add the node data frame to the graph object to create a
  # graph with nodes
  graph_2 <- add_node_df(graph = graph, node_df = nodes)

  # Create another node data frame
  nodes_2 <-
    create_nodes(nodes = c("e", "f", "g", "h"),
                 type = "letter",
                 color = c("white", "brown", "aqua", "pink"),
                 value = c(1.6, 6.4, 0.8, 4.2))

  # Add the second node data frame to the graph object to
  # add more nodes with attributes to the graph
  graph_3 <- add_node_df(graph = graph_2, node_df = nodes_2)

  # Create an edge data frame
  edges <-
    create_edges(from = c("a", "b", "c"),
                 to = c("d", "c", "a"),
                 rel = "leading_to")

  # Adding an edge df to an empty graph results in an error
  expect_error(
    add_edge_df(graph = create_graph(),
                edge_df = edges)
  )

  # Adding an edge df with edges between nodes not in the
  # graph will result in an error
  expect_error(
    add_edge_df(graph = create_graph(create_nodes("a")),
                edge_df = edges)
  )

  # Add the edge data frame to the graph
  graph_3 <- add_edge_df(graph = graph_3, edge_df = edges)

  # Expect a graph object of class 'dgr_graph'
  expect_true(class(graph_3) == "dgr_graph")

  # Expect that the 'edges_df' component is a data frame
  expect_true(class(graph_3$edges_df) == "data.frame")

  # Expect that the 'edges_df' data frame has 6 rows
  expect_true(nrow(graph_3$edges_df) == 3)

  # Expect that the 'edges_df' data frame has 3 columns
  expect_true(ncol(graph_3$edges_df) == 3)

  # Add another edge to a graph that already has some
  # edges defined
  graph_3 <-
    add_edge_df(graph = graph_3,
                edge_df = create_edges("b", "d"))

  # Expect that the 'edges_df' data frame has 4 rows
  expect_true(nrow(graph_3$edges_df) == 4)
})
