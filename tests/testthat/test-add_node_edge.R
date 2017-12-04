context("Adding nodes and/or edges to an existing graph object")

test_that("adding a node to a graph is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add two nodes to the graph
  graph <- add_node(graph)
  graph <- add_node(graph)

  # Expect that names in this graph object match a
  # prescribed set of names
  expect_true(all(
    names(graph) ==
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
  ))

  # Expect a graph object of class `dgr_graph`
  expect_is(
    graph, "dgr_graph")

  # Expect that the `global_attrs` components is not NULL
  expect_true(
    !is.null(graph$global_attrs))

  # Expect that the `nodes_df` component is a data frame
  expect_is(
    graph$nodes_df, "data.frame")

  # Expect that the `edges_df` component is a data frame
  expect_is(
    graph$edges_df, "data.frame")

  # Expect that the graph is a directed graph
  expect_true(
    graph$directed == TRUE)

  # Expect that the `nodes_df` data frame has 3 columns
  expect_true(
    ncol(graph$nodes_df) == 3)

  # Expect that the `nodes_df` data frame has 2 rows
  expect_true(
    nrow(graph$nodes_df) == 2)

  # Add a node with attributes to the graph
  graph_3 <-
    add_node(
      graph = graph,
      type = "fresh")

  # Expect that there will be 3 nodes in the graph
  expect_equal(
    node_count(graph_3), 3)

  # Expect that the "type" value will be present for
  # the node in the new graph
  expect_equal(
    node_type(graph_3, node = 3),
    "fresh")

  # Expect that the other nodes in the graph will
  # still have unassigned `type` values
  expect_true(
    is.na(node_type(graph_3, node = 1)))

  expect_true(
    is.na(node_type(graph_3, node = 2)))

  # Create an empty graph
  graph_unlabeled <- create_graph()

  # Add a single, unlabeled node to the graph
  graph_unlabeled <- add_node(graph_unlabeled)

  # Expect that the graph will have one unlabeled node
  expect_true(
    is.na(graph_unlabeled$nodes_df$label))

  # Add a node to the graph that is joined from another
  graph_from <-
    add_node(
      graph = graph_3,
      from = 3)

  # Expect that `edges_df` is not NULL
  expect_true(
    !is.null(graph_from$edges_df))

  # Expect that the new node is available in the graph
  expect_true(
    is_node_present(
      graph = graph_from,
      node = 4))

  # Expect that the edge from `3` to `4` is in the graph
  expect_true(
    is_edge_present(
      graph = graph_from,
      from = 3,
      to = 4))

  # Expect that the node label is NA for node `4`
  expect_true(
    is.na(graph_from$nodes_df$label[4]))

  # Expect that for node `4`, the `type` is not set
  # since the default value for `type` is NULL
  expect_true(
    is.na(node_type(graph_from, node = 4)))

  # Add a node to the graph that is joined to another
  graph_to <-
    add_node(
      graph = graph_3,
      to = 3)

  # Expect that `edges_df` is not NULL
  expect_true(
    !is.null(graph_to$edges_df))

  # Expect that the new node is available in the graph
  expect_true(
    is_node_present(
      graph = graph_to,
      node = 4))

  # Expect that the edge from `4` to `3` is in the graph
  expect_true(
    is_edge_present(
      graph = graph_to,
      from = 4,
      to = 3))

  # Expect that the node label for all nodes is NA
  expect_true(
    all(is.na(graph_to$nodes_df$label)))

  # Expect that for node `4`, the `type` is not set
  # since the default value for `type` is NULL
  expect_true(
    is.na(node_type(graph_to, node = 4)))

  # Add a node to the graph that is joined from
  # another and to another
  graph_to_from <-
    add_node(
      graph = graph_3,
      from = 1,
      to = 2)

  # Expect that `edges_df` is not NULL
  expect_true(
    !is.null(graph_to_from$edges_df))

  # Expect that the new node is available in the graph
  expect_true(
    is_node_present(
      graph = graph_to_from,
      node = 4))

  # Expect that the edge from `1` to `4` is in the graph
  expect_true(
    is_edge_present(
      graph = graph_to_from,
      from = 1,
      to = 4))

  # Expect that the edge from `4` to `2` is in the graph
  expect_true(
    is_edge_present(
      graph = graph_to_from,
      from = 4,
      to = 2))

  # Expect that for node `4`, the `type` is not set
  # since the default value for `type` is NULL
  expect_true(
    is.na(
      node_type(
        graph = graph_to_from,
        node = 4)))

  # Create an empty graph
  graph <- create_graph()

  # Add a node
  graph <- add_node(graph)

  # Add another node, connecting with only a value
  # provided as `from` but where the referenced node
  # is not in the graph
  expect_error(
    add_node(
      graph = graph,
      from = 3))
})

test_that("adding an edge to a graph is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add two nodes
  graph <- add_node(graph)
  graph <- add_node(graph)

  # Add an edge
  graph <-
    add_edge(
      graph = graph,
      from = 1,
      to = 2,
      rel = "to_get")

  # Expect that names in this graph object match a
  # prescribed set of names
  expect_true(all(
    names(graph) ==
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
  ))

  # Expect a graph object of class `dgr_graph`
  expect_is(
    graph, "dgr_graph")

  # Expect that the `global_attrs` components is not NULL
  expect_true(
    !is.null(graph$global_attrs))

  # Expect that the `nodes_df` component is a data frame
  expect_is(
    graph$nodes_df, "data.frame")

  # Expect that the `edges_df` component is a data frame
  expect_is(
    graph$edges_df, "data.frame")

  # Expect that the graph is a directed graph
  expect_true(
    graph$directed == TRUE)

  # Expect that the `nodes_df` data frame has 4 columns
  expect_true(
    ncol(graph$edges_df) == 4)

  # Expect that the `nodes_df` data frame has 1 row
  expect_true(
    nrow(graph$edges_df) == 1)

  # Expect an error when attempting to add more than 1 edge
  expect_error(
    add_edge(graph, from = c(1, 2), to = 2))

  # Expect an error when calling `add_edge()` on an empty graph
  expect_error(
    add_edge(graph = create_graph()))

  # Create a graph with 4 nodes and labels for each
  graph_2 <-
    create_graph() %>%
    add_node(label = "one") %>%
    add_node(label = "two") %>%
    add_node(label = "three") %>%
    add_node(label = "four")

  # Add two edges to the graph using the node
  # `label` values
  graph_2 <-
    graph_2 %>%
    add_edge(
      from = "three",
      to = "four",
      rel = "L") %>%
    add_edge(
      from = "four",
      to = "one",
      rel = "L")

  # Expect that 2 edges are available in the graph
  expect_equal(
    edge_count(graph_2), 2)

  # Expect that the edges in the graph are:
  # `3->4` and `4->1`
  expect_true(
    all(
      c("3->4", "4->1") %in% get_edges(graph_2)))
})

test_that("adding several nodes to a graph at once is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add 10 nodes to the empty graph
  graph <-
    add_n_nodes(
      graph = graph,
      n = 10)

  # Expect that 10 nodes were added to the empty graph
  expect_equal(
    node_count(graph), 10)

  # Expect monotonically-increasing node ID values
  # from 1 to 10
  expect_equal(
    get_node_ids(graph),
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

  # Expect that no `type` values have been set
  expect_equal(
    get_node_df(graph)[, 2],
    rep(as.character(NA), 10))

  # Expect that no `label` values have been set
  expect_equal(
    get_node_df(graph)[, 3],
    rep(as.character(NA), 10))

  # Create a graph with 10 nodes of a specified type
  graph <- create_graph()

  graph <-
    add_n_nodes(
      graph = graph,
      n = 10,
      type = "test_node")

  # Expect that 10 nodes were added to the empty graph
  expect_equal(
    node_count(graph), 10)

  # Expect monotonically-increasing node ID values
  # from `1` to `10`
  expect_equal(
    get_node_ids(graph),
    seq(1, 10))

  # Expect that a `type` value have been set
  # for all nodes
  expect_equal(
    get_node_df(graph)[, 2],
    rep("test_node", 10))

  # Expect that `label` values have not been set
  expect_equal(
    get_node_df(graph)[, 3],
    rep(as.character(NA), 10))
})

test_that("adding several nodes to a graph at once (with extra attrs) is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add 10 nodes to the empty graph
  graph <-
    add_n_nodes(
      graph = graph,
      n = 10)

  # Expect that 10 nodes were added to the empty graph
  expect_equal(
    node_count(graph), 10)

  # Expect monotonically-increasing node ID values
  # from 1 to 10
  expect_equal(
    get_node_ids(graph),
    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

  # Expect that no `type` values have been set
  expect_equal(
    get_node_df(graph)[, 2],
    rep(as.character(NA), 10))

  # Expect that no `label` values have been set
  expect_equal(
    get_node_df(graph)[, 3],
    rep(as.character(NA), 10))
})

test_that("adding several nodes from a selected node is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add 10 nodes to the empty graph
  graph <-
    add_n_nodes(
      graph = graph,
      n = 10)

  # Select the node with ID of `5`
  graph <-
    select_nodes(
      graph = graph,
      nodes = 5)

  # Add 10 nodes as successors to the selected node
  graph <-
    add_n_nodes_ws(
      graph = graph,
      n = 10,
      direction = "from")

  # Expect a total of 20 nodes in the graph
  expect_equal(
    node_count(graph), 20)

  # Expect monotonically-increasing node ID
  # values from `1` to `20`
  expect_equal(
    get_node_ids(graph), seq(1, 20))

  # Expect a total of 10 edges in the graph
  expect_equal(
    edge_count(graph), 10)

  # Expect that node IDs where edges are `from` belong
  # to the node with ID of `5`
  expect_equal(
    graph$edges_df$from, rep(5, 10))

  # Expect that node IDs where edges are `to` increase
  # from `11` to `20`
  expect_equal(
    graph$edges_df$to, seq(11, 20))

  # Expect that the edge relationship has not been set
  # for any of the edges
  expect_equal(
    graph$edges_df$rel,
    rep(as.character(NA), 10))

  # Create another empty graph
  graph <- create_graph()

  # Add 10 nodes to the empty graph
  graph <-
    add_n_nodes(
      graph = graph,
      n = 10)

  # Select the node with ID of `5`
  graph <-
    select_nodes(
      graph = graph,
      nodes = 5)

  # Add 10 nodes as successors to the selected node,
  # this time with a node `type` and edge `rel` set for
  # each of the new nodes and edges
  graph <-
    add_n_nodes_ws(
      graph = graph,
      n = 10,
      direction = "from",
      type = "new",
      rel = "related")

  # Expect that all edges have a `rel` set
  expect_true(
    all(graph$edges_df$rel == "related"))

  # Expect that all new nodes have a `type` set
  expect_true(
    all(
      get_node_df(graph)[11:20, 2] == "new"))
})

test_that("adding several nodes to a selected node is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add 10 nodes to the empty graph
  graph <-
    add_n_nodes(
      graph = graph,
      n = 10)

  # Select the node with ID of `5`
  graph <-
    select_nodes(
      graph = graph,
      nodes = 5)

  # Add 10 nodes as predecessors to the selected node
  graph <-
    add_n_nodes_ws(
      graph = graph,
      n = 10,
      direction = "to")

  # Expect a total of 20 nodes in the graph
  expect_equal(
    node_count(graph), 20)

  # Expect monotonically-increasing node ID values
  # from `1` to `20`
  expect_equal(
    get_node_ids(graph),
    seq(1, 20))

  # Expect a total of 10 edges in the graph
  expect_equal(
    edge_count(graph), 10)

  # Expect that node IDs where edges are `to` belong
  # to the node with ID of `5`
  expect_equal(
    graph$edges_df$to, rep(5, 10))

  # Expect that node IDs where edges are `from`
  # increase from `11` to `20`
  expect_equal(
    graph$edges_df$from, seq(11, 20))

  # Expect that the edge relationship has not been set
  # for any of the edges
  expect_equal(
    graph$edges_df$rel,
    rep(as.character(NA), 10))

  # Create another empty graph
  graph <- create_graph()

  # Add 10 nodes to the empty graph
  graph <-
    add_n_nodes(
      graph = graph,
      n = 10)

  # Select the node with ID of `5`
  graph <-
    select_nodes(
      graph = graph,
      nodes = 5)

  # Add 10 nodes as predecessors to the selected node,
  # this time with a node `type` and edge `rel` set for
  # each of the new nodes and edges
  graph <-
    add_n_nodes_ws(
      graph = graph,
      n = 10,
      direction = "to",
      type = "new",
      rel = "related")

  # Expect that all edges have a `rel` set
  expect_true(
    all(graph$edges_df$rel == "related"))

  # Expect that all new nodes have a `type` set
  expect_true(
    all(graph$nodes_df$type[11:20] == "new"))
})

test_that("adding several edges with a string is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add 10 nodes to the empty graph
  graph <-
    add_n_nodes(
      graph = graph,
      n = 10)

  # Add edges via a string
  graph <-
    add_edges_w_string(
      graph = graph,
      edges = "1->2 1->3 2->4 2->5 3->6 3->7 4->8 4->9 5->10")

  # Expect a total of 10 nodes in the graph
  expect_equal(
    node_count(graph), 10)

  # Expect monotonically-increasing node ID values
  # from 1 to 10
  expect_equal(
    get_node_ids(graph), seq(1, 10))

  # Expect a total of 9 edges in the graph
  expect_equal(
    edge_count(graph), 9)

  # Expect that the edge relationship has not been set
  # for any of the edges
  expect_equal(
    graph$edges_df$rel,
    rep(as.character(NA), 9))

  # Create another empty graph
  graph <- create_graph()

  # Add 10 nodes to the empty graph
  graph <-
    add_n_nodes(
      graph = graph,
      n = 10)

  # Add edges via a string and add the rel value
  # `connected_to` to all new edges
  graph <-
    add_edges_w_string(
      graph = graph,
      edges = "1->2 1->3 2->4 2->5 3->6 3->7 4->8 4->9 5->10",
      rel = "connected_to")

  # Expect that the edge relationship `connected_to` is
  # set for all edges
  expect_equal(
    graph$edges_df$rel,
    rep("connected_to", 9))

  # Create an empty graph, set as `undirected`
  graph <- create_graph(directed = FALSE)

  # Add 10 nodes to the empty graph
  graph <-
    add_n_nodes(
      graph = graph,
      n = 10)

  # Add edges via a string and add the rel value
  # `connected_to` to all new edges
  graph <-
    add_edges_w_string(
      graph = graph,
      edges = "1--2 1--3 2--4 2--5 3--6 3--7 4--8 4--9 5--10",
      rel = "connected_to")

  # Expect a total of 9 edges in the graph
  expect_equal(edge_count(graph), 9)

  # Create a graph with 4 nodes and 4 distinct labels
  graph <-
    create_graph() %>%
    add_node(label = "one") %>%
    add_node(label = "two") %>%
    add_node(label = "three") %>%
    add_node(label = "four")

  # Add edges between nodes using a
  # character string with node label values
  # and setting `use_labels = TRUE`
  graph_node_label <-
    graph %>%
    add_edges_w_string(
      edges = "one->two one->three
               two->four two->three",
      use_labels = TRUE)

  # Expect a total of 4 edges in the graph
  expect_equal(
    edge_count(graph_node_label), 4)

  # Expect certain edges to be in the graph
  expect_equal(
    get_edges(graph_node_label),
    c("1->2", "1->3", "2->4", "2->3"))
})

test_that("adding node clones is possible", {

  # Create a graph with a path of
  # nodes having non-NA `label`, `type`,
  # and `value` node attributes
  graph <-
    create_graph() %>%
    add_path(
      n = 3,
      label = c("d", "g", "r"),
      type = c("a", "b", "c"))

  # Create 3 clones of node `1`
  # and assign new node label
  # values
  graph_1 <-
    graph %>%
    add_n_node_clones(
      n = 3,
      node = 1,
      label = c("x", "y", "z"))

  # Get the graph's node data frame
  ndf_1 <-
    graph_1 %>%
    get_node_df()

  # Expect that the last three nodes
  # will be have the same `type` value
  # as node `1`
  expect_equal(
    unique(ndf_1[c(1, 4:6), 2]), "a")

  # Expect that the new nodes will
  # have `label` values that were
  # specified in the function call
  expect_equal(
    ndf_1[4:6, 3], c("x", "y", "z"))

  # Create 3 clones of node `1`
  # and don't assign `label` values
  # (have them as NA values)
  graph_2 <-
    graph %>%
    add_n_node_clones(
      n = 3,
      node = 1,
      label = NULL)

  # Get the graph's node data frame
  ndf_2 <-
    graph_2 %>%
    get_node_df()

  # Expect that the last three nodes
  # will be have the same `type` value
  # as node `1`
  expect_equal(
    unique(ndf_2[c(1, 4:6), 2]), "a")

  # Expect that the new nodes will
  # have NA `label` values
  expect_equal(
    ndf_2[4:6, 3],
    rep(as.character(NA), 3))
})

test_that("adding node clones with a selection is possible", {

  # Create a graph with a path of
  # nodes; supply `label`, `type`,
  # and `value` node attributes,
  # and select the created nodes
  graph <-
    create_graph() %>%
    add_path(
      n = 3,
      label = c("d", "g", "r"),
      type = c("a", "b", "c")) %>%
    select_last_nodes_created()

  # Create clones of all nodes
  # in the selection
  graph_1 <-
    graph %>%
    add_node_clones_ws()

  # Expect 6 nodes in the graph
  expect_equal(
    count_nodes(graph_1), 6)

  # Expect nodes with ID values
  # from 1 to 6
  expect_equal(
    (graph_1 %>%
       get_node_df)[, 1], 1:6)

  # Expect cloned values for the
  # `type` attribute for the
  # last 3 nodes in the graph
  expect_equal(
    (graph_1 %>%
       get_node_df())[4:6, 2],
    c("a", "b", "c"))

  # Expect NA values for the
  # `label` attributes for the
  # last 3 nodes in the graph
  expect_true(
    all(
      is.na(
        graph_1 %>%
          get_node_df())[4:6, 3]))

  # Create clones of all nodes
  # in the selection but assign
  # new node label values
  graph_2 <-
    graph %>%
    add_node_clones_ws(
      label = c("a", "b", "v"))

  # Expect 6 nodes in the graph
  expect_equal(
    count_nodes(graph_2), 6)

  # Expect nodes with ID values
  # from 1 to 6
  expect_equal(
    (graph_2 %>%
       get_node_df)[, 1], 1:6)

  # Expect cloned values for the
  # `type` attribute for the
  # last 3 nodes in the graph
  expect_equal(
    (graph_2 %>%
       get_node_df())[4:6, 2],
    c("a", "b", "c"))

  # Expect specific values for the
  # `label` attributes for the
  # last 3 nodes in the graph
  expect_equal(
    (graph_2 %>%
       get_node_df())[4:6, 3],
    c("a", "b", "v"))

  # Create clones of all nodes
  # in the selection and
  # add edges from the orginal
  # nodes to the cloned nodes
  graph_3 <-
    graph %>%
    add_node_clones_ws(
      add_edges = TRUE,
      direction = "to")

  # Expect 6 nodes in the graph
  expect_equal(
    count_nodes(graph_3), 6)

  # Expect nodes with ID values
  # from 1 to 6
  expect_equal(
    (graph_3 %>%
       get_node_df)[, 1], 1:6)

  # Expect cloned values for the
  # `type` attribute for the
  # last 3 nodes in the graph
  expect_equal(
    (graph_3 %>%
       get_node_df())[4:6, 2],
    c("a", "b", "c"))

  # Expect NA values for the
  # `label` attributes for the
  # last 3 nodes in the graph
  expect_true(
    all(
      is.na(
        graph_3 %>%
          get_node_df())[4:6, 3]))

  # Expect 5 edges in the graph
  expect_equal(
    count_edges(graph_3), 5)

  # Expect edges with ID values
  # from 1 to 5
  expect_equal(
    (graph_3 %>%
       get_edge_df)[, 1], 1:5)

  # Expect that specific edges have
  # been created
  expect_equal(
    (graph_3 %>%
       get_edges)[3:5],
    c("1->4", "2->5", "3->6"))

  # Expect NA values for the
  # `rel` attributes for the
  # last 3 edges in the graph
  expect_true(
    all(
      is.na(
        (graph_3 %>%
          get_edge_df())[3:5, 4])))

  # Create clones of all nodes
  # in the selection and
  # add edges to the orginal
  # nodes from the cloned nodes
  graph_4 <-
    graph %>%
    add_node_clones_ws(
      add_edges = TRUE,
      direction = "from")

  # Expect 6 nodes in the graph
  expect_equal(
    count_nodes(graph_4), 6)

  # Expect nodes with ID values
  # from 1 to 6
  expect_equal(
    (graph_4 %>%
       get_node_df)[, 1], 1:6)

  # Expect cloned values for the
  # `type` attribute for the
  # last 3 nodes in the graph
  expect_equal(
    (graph_4 %>%
       get_node_df())[4:6, 2],
    c("a", "b", "c"))

  # Expect NA values for the
  # `label` attributes for the
  # last 3 nodes in the graph
  expect_true(
    all(
      is.na(
        graph_4 %>%
          get_node_df())[4:6, 3]))

  # Expect 5 edges in the graph
  expect_equal(
    count_edges(graph_4), 5)

  # Expect edges with ID values
  # from 1 to 5
  expect_equal(
    (graph_4 %>%
       get_edge_df)[, 1], 1:5)

  # Expect that specific edges have
  # been created
  expect_equal(
    (graph_4 %>%
       get_edges)[3:5],
    c("4->1", "5->2", "6->3"))

  # Expect NA values for the
  # `rel` attributes for the
  # last 3 edges in the graph
  expect_true(
    all(
      is.na(
        (graph_4 %>%
           get_edge_df())[3:5, 4])))
})

test_that("adding edge clones is possible", {

  # Create a graph with a path of
  # 2 nodes; supply a common `rel`
  # edge attribute for all edges
  # in this path and then add a
  # `color` edge attribute
  graph <-
    create_graph() %>%
    add_path(
      n = 2,
      rel = "a") %>%
    select_last_edges_created() %>%
    set_edge_attrs(
      edge_attr = color,
      values = "steelblue") %>%
    clear_selection() %>%
    add_node()

  # Create an edge clone (new edge
  # with attributes from edge `1`)
  graph_1 <-
    graph %>%
    add_edge_clone(
      edge = 1,
      from = 3,
        to = 1)

  # Get the graph's edge data frame
  edf_1 <-
    graph_1 %>%
    get_edge_df()

  # Expect 2 edges in the edf
  expect_equal(
    nrow(edf_1), 2)

  # Expect that the new edge has
  # definition `3` -> `1`
  expect_equal(
    c(edf_1[2, 2], edf_1[2, 3]),
    c(3, 1))

  # Expect that the new edge has
  # edge ID `2`
  expect_equal(
    edf_1[2, 1], 2)

  # Expect that both edges will be have
  # the same `rel` values
  expect_equal(
    unique(edf_1[, 4]), "a")

  # Expect that both edges will be have
  # the same `color` values
  expect_equal(
    unique(edf_1[, 5]), "steelblue")
})
