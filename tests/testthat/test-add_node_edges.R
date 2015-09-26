context("Adding nodes and/or edges to an existing graph object")

test_that("adding a node to a graph is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add two nodes to the graph
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

  # Expect a specific message when more than a single node is specified
  expect_error(
    add_node(graph, node = c("y", "z"))
  )

  # Expect that attempting to add more than a single node will return an
  # unchanged graph
  expect_error(
    add_node(graph, node = c("y", "z"))
  )

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

  # Add a node to the graph that is joined from another
  graph_from <- add_node(graph_3,
                         node = "d",
                         from = "c")

  # Expect that 'edges_df' is not NULL
  expect_true(!is.null(graph_from$edges_df))

  # Expect that the new node is available in the graph
  expect_true(node_present(graph_from, "d"))

  # Expect that the edge from "c" to "d" is in the graph
  expect_true(edge_present(graph_from, from = "c", to = "d"))

  # Expect that the node label is the same as the ID, since the
  # default value for the 'label' argument is TRUE
  expect_true(
    node_info(graph_from)[which(node_info(graph_from)$node_ID == "d"),]$node_ID ==
      node_info(graph_from)[which(node_info(graph_from)$node_ID == "d"),]$label
  )

  # Expect that for node "d", the 'type' is not set since the
  # default value for the 'type' argument is NULL
  expect_true(is.na(node_type(graph_from, node = "d")))

  # Add a node to the graph that is joined to another
  graph_to <- add_node(graph_3,
                       node = "d",
                       to = "c")

  # Expect that 'edges_df' is not NULL
  expect_true(!is.null(graph_to$edges_df))

  # Expect that the new node is available in the graph
  expect_true(node_present(graph_to, "d"))

  # Expect that the edge from "d" to "c" is in the graph
  expect_true(edge_present(graph_to, from = "d", to = "c"))

  # Expect that the node label is the same as the ID, since the
  # default value for the 'label' argument is TRUE
  expect_true(
    node_info(graph_to)[
      which(node_info(graph_to)$node_ID == "d"),]$node_ID ==
      node_info(graph_to)[
        which(node_info(graph_to)$node_ID == "d"),]$label
  )

  # Expect that for node "d", the 'type' is not set since the
  # default value for the 'type' argument is NULL
  expect_true(is.na(node_type(graph_to, node = "d")))

  # Add a node to the graph that is joined from another and to another
  graph_to_from <- add_node(graph_3,
                            node = "d",
                            from = "a",
                            to = "b")

  # Expect that 'edges_df' is not NULL
  expect_true(!is.null(graph_to_from$edges_df))

  # Expect that the new node is available in the graph
  expect_true(node_present(graph_to_from, "d"))

  # Expect that the edge from "a" to "d" is in the graph
  expect_true(edge_present(graph_to_from, from = "a", to = "d"))

  # Expect that the edge from "d" to "b" is in the graph
  expect_true(edge_present(graph_to_from, from = "d", to = "b"))

  # Expect that the node label is the same as the ID, since the
  # default value for the 'label' argument is TRUE
  expect_true(
    node_info(graph_to_from)[
      which(node_info(graph_to_from)$node_ID == "d"),]$node_ID ==
      node_info(graph_to_from)[
        which(node_info(graph_to_from)$node_ID == "d"),]$label
  )

  # Expect that for node "d", the 'type' is not set since the
  # default value for the 'type' argument is NULL
  expect_true(is.na(node_type(graph_to_from, node = "d")))

  # Create an empty graph
  graph <- create_graph()

  # Add a node
  graph <- add_node(graph, node = "a")

  # Add another node, connecting with only a value provided for 'from' but
  # where the reference node is not in the graph
  expect_error(
    add_node(graph, node = "b", from = "c")
  )
})

test_that("adding an edge to a graph is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add two nodes
  graph <- add_node(graph, node = "a")
  graph <- add_node(graph, node = "b")

  # Add an edge
  graph <- add_edges(graph, from = "a", to = "b",
                     rel = "to_get")

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

  # Expect a message when adding an existing edge to the graph
  expect_error(
    add_edges(graph, from = "a", to = "b")
  )

  # Create another empty graph
  graph_empty <- create_graph()

  # Expect an error when calling 'add_edges' on an empty graph
  expect_error(add_edges(graph = graph_empty))
})

test_that("adding a node to a graph is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Create a node data frame
  nodes <-
    create_nodes(nodes = c("a", "b", "c", "d"),
                 label = FALSE,
                 type = "lower",
                 style = "filled",
                 shape = "circle")

  # Create a graph with nodes but no edges
  graph <- create_graph(nodes_df = nodes)

  # Create an edge data frame
  edges <-
    create_edges(from = c("a", "b", "c"),
                 to = c("d", "c", "a"),
                 rel = "leading_to")

  # Add the edges to the graph using 'add_edges'
  graph_edges_added <-
    add_edges(graph, edges_df = edges)

  # Expect a graph object of class 'dgr_graph'
  expect_true(class(graph_edges_added) == "dgr_graph")

  # Expect that the 'edges' object will be the same as in
  # 'graph_edges_added$edges_df'
  expect_equal(edges, graph_edges_added$edges_df)

  # Expect that the 'nodes' object will be the same as in
  # 'graph_edges_added$nodes_df'
  expect_equal(nodes, graph_edges_added$nodes_df)
})
