context("Adding nodes and/or edges to an existing graph object")

test_that("adding a node to a graph is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add two nodes to the graph
  graph <- add_node(graph)
  graph <- add_node(graph)

  # Expect that names in this graph object match a
  # prescribed set of names
  expect_true(
    all(names(graph) ==
          c("graph_name", "graph_time", "graph_tz",
            "nodes_df", "edges_df", "graph_attrs",
            "node_attrs", "edge_attrs", "directed",
            "last_node")))

  # Expect a graph object of class `dgr_graph`
  expect_is(graph, "dgr_graph")

  # Expect that some of the graph components are `NULL`
  expect_null(graph$graph_name)
  expect_null(graph$graph_time)
  expect_null(graph$graph_tz)
  expect_null(graph$edges_df)

  # Expect that some of the graph components are not `NULL`
  expect_true(!is.null(graph$graph_attrs))
  expect_true(!is.null(graph$node_attrs))
  expect_true(!is.null(graph$edge_attrs))

  # Expect that the `nodes_df` component is a data frame
  expect_is(graph$nodes_df, "data.frame")

  # Expect that the graph is a directed graph
  expect_true(graph$directed == TRUE)

  # Expect that the `nodes_df` data frame has 3 columns
  expect_true(ncol(graph$nodes_df) == 3)

  # Expect that the `nodes_df` data frame has 2 rows
  expect_true(nrow(graph$nodes_df) == 2)

  # Add a node with attributes to the graph
  graph_3 <-
    add_node(graph, type = "fresh")

  # Expect that there will be 3 nodes in the graph
  expect_equal(node_count(graph_3), 3)

  # Expect that the "type" value will be present for
  # the node in the new graph
  expect_equal(
    node_type(graph_3, node = 3), "fresh")

  # Expect that the other nodes in the graph will
  # still have unassigned `type` values
  expect_true(is.na(node_type(graph_3, node = 1)))
  expect_true(is.na(node_type(graph_3, node = 2)))

  # Create a graph with a single, unlabeled node
  graph_unlabeled <- create_graph()

  graph_unlabeled <-
    add_node(graph_unlabeled, label = FALSE)

  # Expect that the graph will have one unlabeled node
  expect_true(
    node_info(graph = graph_unlabeled)$label == "")

  # Add a node to the graph that is joined from another
  graph_from <-
    add_node(graph_3, from = 3)

  # Expect that `edges_df` is not NULL
  expect_true(!is.null(graph_from$edges_df))

  # Expect that the new node is available in the graph
  expect_true(node_present(graph_from, 4))

  # Expect that the edge from `3` to `4` is in the graph
  expect_true(edge_present(graph_from, from = 3, to = 4))

  # Expect that the node label is the same as the ID,
  # since the default value for `label` is TRUE
  expect_true(
    node_info(graph_from)[which(node_info(graph_from)$node == 4),]$node ==
      node_info(graph_from)[which(node_info(graph_from)$node == 4),]$label
  )

  # Expect that for node `4`, the `type` is not set
  # since the default value for `type` is `NULL`
  expect_true(is.na(node_type(graph_from, node = 4)))

  # Add a node to the graph that is joined to another
  graph_to <-
    add_node(graph_3, to = 3)

  # Expect that `edges_df` is not `NULL`
  expect_true(!is.null(graph_to$edges_df))

  # Expect that the new node is available in the graph
  expect_true(node_present(graph_to, 4))

  # Expect that the edge from `4` to `3` is in the graph
  expect_true(edge_present(graph_to, from = 4, to = 3))

  # Expect that the node label is the same as the ID,
  # since the default value for `label` is TRUE
  expect_true(
    node_info(graph_to)[
      which(node_info(graph_to)$node == 4),]$node ==
      node_info(graph_to)[
        which(node_info(graph_to)$node == 4),]$label
  )

  # Expect that for node `4`, the `type` is not set
  # since the default value for `type` is `NULL`
  expect_true(is.na(node_type(graph_to, node = 4)))

  # Add a node to the graph that is joined from
  # another and to another
  graph_to_from <-
    add_node(graph_3, from = 1, to = 2)

  # Expect that `edges_df` is not `NULL`
  expect_true(!is.null(graph_to_from$edges_df))

  # Expect that the new node is available in the graph
  expect_true(node_present(graph_to_from, 4))

  # Expect that the edge from `1` to `4` is in the graph
  expect_true(
    edge_present(graph_to_from, from = 1, to = 4))

  # Expect that the edge from `4` to `2` is in the graph
  expect_true(
    edge_present(graph_to_from, from = 4, to = 2))

  # Expect that the node label is the same as the ID,
  # since the default value for `label` is TRUE
  expect_true(
    node_info(graph_to_from)[
      which(node_info(graph_to_from)$node == 4),]$node ==
      node_info(graph_to_from)[
        which(node_info(graph_to_from)$node == 4),]$label)

  # Expect that for node `4`, the `type` is not set
  # since the default value for `type` is `NULL`
  expect_true(
    is.na(node_type(graph_to_from, node = 4)))

  # Create an empty graph
  graph <- create_graph()

  # Add a node
  graph <- add_node(graph)

  # Add another node, connecting with only a value
  # provided as `from` but where the referenced node
  # is not in the graph
  expect_error(add_node(graph, from = 3))
})

test_that("adding an edge to a graph is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add two nodes
  graph <- add_node(graph)
  graph <- add_node(graph)

  # Add an edge
  graph <-
    add_edge(graph, from = 1, to = 2, rel = "to_get")

  # Expect that names in this graph object match a
  # prescribed set of names
  expect_true(
    all(names(graph) ==
          c("graph_name", "graph_time", "graph_tz",
            "nodes_df", "edges_df", "graph_attrs",
            "node_attrs", "edge_attrs", "directed",
            "last_node")))

  # Expect a graph object of class `dgr_graph`
  expect_is(graph, "dgr_graph")

  # Expect that some of the graph components are NULL
  expect_null(graph$graph_name)
  expect_null(graph$graph_time)
  expect_null(graph$graph_tz)

  # Expect that some of the graph components are not NULL
  expect_true(!is.null(graph$graph_attrs))
  expect_true(!is.null(graph$node_attrs))
  expect_true(!is.null(graph$edge_attrs))

  # Expect that the `nodes_df` component is a data frame
  expect_is(graph$nodes_df, "data.frame")

  # Expect that the `edges_df` component is a data frame
  expect_is(graph$edges_df, "data.frame")

  # Expect that the graph is a directed graph
  expect_true(graph$directed == TRUE)

  # Expect that the `nodes_df` data frame has 3 columns
  expect_true(ncol(graph$edges_df) == 3)

  # Expect that the `nodes_df` data frame has 1 row
  expect_true(nrow(graph$edges_df) == 1)

  # Expect a message when adding an existing edge to the graph
  expect_error(add_edge(graph, from = 1, to = 2))

  # Expect an error when attempting to add more than 1 edge
  expect_error(add_edge(graph, from = c(1, 2), to = 2))

  # Expect an error when calling `add_edge()` on an empty graph
  expect_error(add_edge(graph = create_graph()))
})

test_that("adding several nodes to a graph at once is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add 10 nodes to the empty graph
  graph <- add_n_nodes(graph, 10)

  # Expect that 10 nodes were added to the empty graph
  expect_equal(node_count(graph), 10)

  # Expect monotonically-increasing node ID values
  # from 1 to 10
  expect_equal(
    get_node_ids(graph), c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

  # Expect that no `type` values have been set
  expect_equal(get_node_df(graph)[, 2],
               rep("", 10))

  # Expect that `label` values have been set
  expect_equal(get_node_df(graph)[, 3],
               as.character(seq(1, 10)))

  # Create a graph with 10 nodes of a specified type
  graph <- create_graph()
  graph <- add_n_nodes(graph, 10, type = "test_node")

  # Expect that 10 nodes were added to the empty graph
  expect_equal(node_count(graph), 10)

  # Expect monotonically-increasing node ID values
  # from `1` to `10`
  expect_equal(get_node_ids(graph), seq(1, 10))

  # Expect that a `type` value have been set
  # for all nodes
  expect_equal(get_node_df(graph)[, 2],
               rep("test_node", 10))

  # Expect that `label` values have been set
  expect_equal(get_node_df(graph)[, 3],
               as.character(seq(1, 10)))
})

test_that("adding several nodes from a selected node is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add 10 nodes to the empty graph
  graph <- add_n_nodes(graph, 10)

  # Select the node with ID of `5`
  graph <- select_nodes(graph, nodes = 5)

  # Add 10 nodes as successors to the selected node
  graph <- add_n_nodes_ws(graph, 10, "from")

  # Expect a total of 20 nodes in the graph
  expect_equal(node_count(graph), 20)

  # Expect monotonically-increasing node ID
  # values from `1` to `20`
  expect_equal(get_node_ids(graph), seq(1, 20))

  # Expect a total of 10 edges in the graph
  expect_equal(edge_count(graph), 10)

  # Expect that node IDs where edges are `from` belong
  # to the node with ID of `5`
  expect_equal(get_edge_df(graph)[, 1],
               rep(5, 10))

  # Expect that node IDs where edges are `to` increase
  # from `11` to `20`
  expect_equal(get_edge_df(graph)[, 2], seq(11, 20))

  # Expect that the edge relationship has not been set
  # for any of the edges
  expect_equal(get_edge_df(graph)[, 3],
               rep("", 10))

  # Create another empty graph
  graph <- create_graph()

  # Add 10 nodes to the empty graph
  graph <- add_n_nodes(graph, 10)

  # Select the node with ID of `5`
  graph <- select_nodes(graph, nodes = 5)

  # Add 10 nodes as successors to the selected node,
  # this time with a node `type` and edge `rel` set for
  # each of the new nodes and edges
  graph <-
    add_n_nodes_ws(
      graph, 10,
      direction = "from",
      type = "new",
      rel = "related")

  # Expect that all edges have a `rel` set
  expect_true(all(get_edge_df(graph)[, 3] == "related"))

  # Expect that all new nodes have a `type` set
  expect_true(all(get_node_df(graph)[11:20, 2] == "new"))
})

test_that("adding several nodes to a selected node is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add 10 nodes to the empty graph
  graph <- add_n_nodes(graph, 10)

  # Select the node with ID of `5`
  graph <- select_nodes(graph, nodes = 5)

  # Add 10 nodes as predecessors to the selected node
  graph <- add_n_nodes_ws(graph, 10, "to")

  # Expect a total of 20 nodes in the graph
  expect_equal(node_count(graph), 20)

  # Expect monotonically-increasing node ID values
  # from `1` to `20`
  expect_equal(get_node_ids(graph), seq(1, 20))

  # Expect a total of 10 edges in the graph
  expect_equal(edge_count(graph), 10)

  # Expect that node IDs where edges are `to` belong
  # to the node with ID of `5`
  expect_equal(get_edge_df(graph)[, 2], rep(5, 10))

  # Expect that node IDs where edges are `from`
  # increase from `11` to `20`
  expect_equal(get_edge_df(graph)[, 1], seq(11, 20))

  # Expect that the edge relationship has not been set
  # for any of the edges
  expect_equal(get_edge_df(graph)[, 3], rep("", 10))

  # Create another empty graph
  graph <- create_graph()

  # Add 10 nodes to the empty graph
  graph <- add_n_nodes(graph, 10)

  # Select the node with ID of `5`
  graph <- select_nodes(graph, nodes = 5)

  # Add 10 nodes as predecessors to the selected node,
  # this time with a node `type` and edge `rel` set for
  # each of the new nodes and edges
  graph <-
    add_n_nodes_ws(
      graph, 10,
      direction = "to",
      type = "new",
      rel = "related")

  # Expect that all edges have a `rel` set
  expect_true(all(get_edge_df(graph)[, 3] == "related"))

  # Expect that all new nodes have a `type` set
  expect_true(all(get_node_df(graph)[11:20, 2] == "new"))
})

test_that("adding several edges with a string is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add 10 nodes to the empty graph
  graph <- add_n_nodes(graph, 10)

  # Add edges via a string
  graph <-
    add_edges_w_string(
      graph,
      edges = "1->2 1->3 2->4 2->5 3->6 3->7 4->8 4->9 5->10")

  # Expect a total of 10 nodes in the graph
  expect_equal(node_count(graph), 10)

  # Expect monotonically-increasing node ID values
  # from 1 to 10
  expect_equal(get_node_ids(graph), seq(1, 10))

  # Expect a total of 9 edges in the graph
  expect_equal(edge_count(graph), 9)

  # Expect that the edge relationship has not been set
  # for any of the edges
  expect_equal(get_edge_df(graph)[, 3],
               rep("", 9))

  # Create another empty graph
  graph <- create_graph()

  # Add 10 nodes to the empty graph
  graph <- add_n_nodes(graph, 10)

  # Add edges via a string and add the rel value
  # `connected_to` to all new edges
  graph <-
    add_edges_w_string(
      graph,
      edges = "1->2 1->3 2->4 2->5 3->6 3->7 4->8 4->9 5->10",
      rel = "connected_to")

  # Expect that the edge relationship `connected_to` is
  # set for all edges
  expect_equal(get_edge_df(graph)[, 3],
               rep("connected_to", 9))

  # Create an empty graph, set as `undirected``
  graph <- create_graph(directed = FALSE)

  # Add 10 nodes to the empty graph
  graph <- add_n_nodes(graph, 10)

  # Add edges via a string and add the rel value
  # `connected_to` to all new edges
  graph <-
    add_edges_w_string(
      graph,
      edges = "1--2 1--3 2--4 2--5 3--6 3--7 4--8 4--9 5--10",
      rel = "connected_to")

  # Expect a total of 9 edges in the graph
  expect_equal(edge_count(graph), 9)
})
