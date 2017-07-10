context("Getting or selection of last-created nodes or edges in a graph")

test_that("getting or selecting the last edges created is possible", {

  # Create a graph, add a cycle, and then
  # add a tree
  graph <-
    create_graph() %>%
    add_cycle(
      n = 3,
      type = "cycle",
      rel = "a") %>%
    add_balanced_tree(
      k = 2,
      h = 2,
      type = "tree",
      rel = "b")

  # Select the last edges created (all edges
  # from the tree)
  graph_e <-
    graph %>%
    select_last_edges_created()

  # Expect that the selection available in
  # the graph is an edge selection
  expect_true(
    nrow(graph_e$edge_selection) > 0)

  expect_true(
    nrow(graph_e$node_selection) == 0)

  # Expect that the edges selected are
  # those that have `rel == 'b'` as an
  # edge attribute (since that attribute
  # belongs to the balanced tree structure,
  # which was created last
  expect_identical(
    get_edge_df(graph_e)[which(get_edge_df(graph_e)$rel == "b"), 1],
    get_selection(graph_e))

  # Get the last edges created directly
  # with `get_last_edges_created()`
  last_edges_created <-
    graph %>%
    get_last_edges_created()

  # Expect the same edge ID values as
  # those that have `rel == 'b'` as an
  # edge attribute
  expect_identical(
    get_edge_df(graph_e)[which(get_edge_df(graph_e)$rel == "b"), 1],
    last_edges_created)
})

test_that("getting or selecting the last nodes created is possible", {

  # Create a graph, add a cycle, and then
  # add a tree
  graph <-
    create_graph() %>%
    add_cycle(
      n = 3,
      type = "cycle",
      rel = "a") %>%
    add_balanced_tree(
      k = 2,
      h = 2,
      type = "tree",
      rel = "b")

  # Select the last nodes created (all nodes
  # from the tree)
  graph_n <-
    graph %>%
    select_last_nodes_created()

  # Expect that the selection available in
  # the graph is a node selection
  expect_true(
    nrow(graph_n$node_selection) > 0)

  expect_true(
    nrow(graph_n$edge_selection) == 0)

  # Expect that the nodes selected are
  # those that have `type == 'tree'` as a
  # node attribute (since that attribute
  # belongs to the balanced tree structure,
  # which was created last
  expect_identical(
    get_node_df(graph_n)[which(get_node_df(graph_n)$type == "tree"), 1],
    get_selection(graph_n))

  # Get the last nodes created directly
  # with `get_last_nodes_created()`
  last_nodes_created <-
    graph %>%
    get_last_nodes_created()

  # Expect the same node ID values as
  # those that have `type == 'tree'` as a
  # node attribute
  expect_identical(
    get_node_df(graph_n)[which(get_node_df(graph_n)$type == "tree"), 1],
    last_nodes_created)
})
