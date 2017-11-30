context("Perform graph object transformations")

test_that("Converting to igraph is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 23)

  # Create an igraph object from a
  # DiagrammeR graph object
  igraph_graph <- to_igraph(graph)

  # Expect that the new object is an igraph object
  expect_is(
    igraph_graph, "igraph")
})

test_that("Changing to undirected mode is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 23,
      directed = TRUE)

  undirected_graph <-
    set_graph_undirected(graph)

  # Expect the the graph is undirected
  expect_true(
    graph$directed)
})

test_that("Reversing the graph edges is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 23,
      directed = TRUE)

  # Get a vector of `from` nodes
  from_nodes <- graph$edges_df$from

  # Get a vector of `to` nodes
  to_nodes <- graph$edges_df$to

  # Change the edge direction for all edges in
  # the graph
  graph <- rev_edge_dir(graph)

  # Expect that the `from` nodes in the new graph
  # object are identical to the `to` nodes before
  # reversing the edge direction
  expect_equal(
    graph$edges_df$from, to_nodes)

  # Expect that the `to` nodes in the new graph
  # object are identical to the `from` nodes before
  # reversing the edge direction
  expect_equal(
    graph$edges_df$to, from_nodes)

  # Expect an error if reversing edge direction
  # in an undirected graph
  expect_error(
    create_random_graph(
      n = 5, m = 5,
      directed = FALSE) %>%
      rev_edge_dir())
})

test_that("Reversing edges in a selection is possible", {

  # Create a graph with a directed tree
  graph <-
    create_graph() %>%
    add_balanced_tree(
      k = 2, h = 2)

  # Select all edges associated with
  # nodes `1` and `2` (this selects
  # 4 graph edges)
  graph <-
    select_edges_by_node_id(
      graph = graph,
      nodes = 1:2)

  # Reverse the edge directions of
  # the selected edges
  graph_reversed_edges <-
    graph %>%
    rev_edge_dir_ws()

  # Expect certain edge definitions
  # in the transformed graph
  expect_identical(
    graph_reversed_edges %>%
      get_edges(),
    c("2->1", "3->1", "4->2",
      "5->2", "3->6", "3->7"))

  # Expect an error if there is no
  # selection of edges
  expect_error(
    create_graph() %>%
      add_balanced_tree(
        k = 2, h = 2) %>%
      rev_edge_dir_ws())

  # Expect an error if the graph
  # is undirected
  expect_error(
    create_graph(
      directed = FALSE) %>%
      add_balanced_tree(
        k = 2, h = 2) %>%
      select_edges_by_node_id(
        nodes = 1:2) %>%
      rev_edge_dir())


})

test_that("Creating a complement graph is possible", {

  # Create a simple graph with a single cycle
  graph <-
    create_graph() %>%
    add_cycle(n = 4)

  # Create the complement of the graph
  graph_c <- create_complement_graph(graph)

  # Expect 8 edges in the complement graph
  expect_equal(
    graph_c %>%
      get_edge_df() %>%
      nrow(), 8)

  # Expect that there are no loops in the
  # complement graph
  expect_equal(
    graph_c %>%
      get_edge_df() %>%
      filter(from == to) %>%
      nrow(), 0)

  # Create the complement of the original graph
  # with loops created
  graph_cl <-
    create_complement_graph(
      graph = graph,
      loops = TRUE)

  # Expect 12 edges in this complement graph
  expect_equal(
    graph_cl %>%
      get_edge_df() %>%
      nrow(), 12)
})

test_that("Fully connecting selected nodes is possible", {

  # Create a directed graph with a path
  # of 3 nodes and two isolated nodes
  graph_directed <-
    create_graph() %>%
    add_path(n = 3) %>%
    add_n_nodes(n = 2)

  # Select a node in the path
  # of nodes (node `3`) and
  # the two isolated nodes (`4`
  # and `5`); then, and fully
  # connect these nodes together
  graph_directed <-
    graph_directed %>%
    select_nodes_by_id(
      nodes = 3:5) %>%
    fully_connect_nodes_ws()

  # Expect 8 edges in the new graph
  expect_equal(
    graph_directed %>%
      count_edges(), 8)

  # Expect certain edges in the graph
  expect_identical(
    graph_directed %>%
      get_edges(),
    c("1->2", "2->3", "3->4", "3->5",
      "4->5", "4->3", "5->3", "5->4"))

  # Create an undirected graph with a path
  # of 3 nodes and two isolated nodes
  graph_undirected <-
    create_graph(
      directed = FALSE) %>%
    add_path(n = 3) %>%
    add_n_nodes(n = 2)

  # Select a node in the path
  # of nodes (node `3`) and
  # the two isolated nodes (`4`
  # and `5`); then, and fully
  # connect these nodes together
  graph_undirected <-
    graph_undirected %>%
    select_nodes_by_id(
      nodes = 3:5) %>%
    fully_connect_nodes_ws()

  # Expect 8 edges in the new graph
  expect_equal(
    graph_undirected %>%
      count_edges(), 5)

  # Expect certain edges in the graph
  expect_identical(
    graph_undirected %>%
      get_edges(),
    c("1->2", "2->3", "3->4",
      "3->5", "4->5"))

  # Expect an error if there is no valid
  # selection of node
  expect_error(
    create_graph() %>%
      add_n_nodes(n = 2) %>%
      fully_connect_nodes_ws())
})

test_that("Fully disconnecting selected nodes is possible", {

  # Create a directed graph with a path
  # of 3 nodes and two isolated nodes
  graph_directed <-
    create_graph() %>%
    add_path(n = 3)

  # Select all nodes and fully
  # disconnect these nodes
  graph_directed <-
    graph_directed %>%
    select_nodes() %>%
    fully_disconnect_nodes_ws()

  # Expect no edges in the new graph
  expect_equal(
    graph_directed %>%
      count_edges(), 0)

  # Expect all nodes to be retained
  expect_equal(
    graph_directed %>%
      count_nodes(), 3)

  # Create an undirected graph with
  # a path of 3 nodes
  graph_undirected <-
    create_graph(
      directed = FALSE) %>%
    add_path(n = 3)

  # Select all nodes and fully
  # disconnect these nodes
  graph_undirected <-
    graph_undirected %>%
    select_nodes() %>%
    fully_disconnect_nodes_ws()

  # Expect no edges in the new graph
  expect_equal(
    graph_undirected %>%
      count_edges(), 0)

  # Expect an error if there is no valid
  # selection of node
  expect_error(
    create_graph() %>%
      add_path(n = 2) %>%
      fully_disconnect_nodes_ws())
})

test_that("Removing loop edges via a selection is possible", {

  # Create an undirected, full graph
  # of 5 nodes with loops retained
  graph <-
    create_graph(
      directed = FALSE) %>%
    add_full_graph(
      n = 5,
      keep_loops = TRUE)

  # Select nodes `3` and `4`
  # and remove the loop edges
  # associated with those nodes
  graph_loops_removed <-
    graph %>%
    select_nodes_by_id(
      nodes = c(3, 4)) %>%
    delete_loop_edges_ws()

  # Expect that there are 3 loops
  # remaining in the graph
  expect_equal(
    graph_loops_removed %>%
      get_edge_df() %>%
      filter(from == to) %>%
      nrow(), 3)

  # Expect an error if there is
  # isn't a valid node selection
  expect_error(
    graph %>%
    delete_loop_edges_ws())
})
