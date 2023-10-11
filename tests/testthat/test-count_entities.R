# Getting counts of nodes and edges in graph objects

test_that("getting a node count (with `count_nodes()`) is possible", {

  # Create a simple graph
  graph <-
    create_graph() %>%
    add_path(n = 5)

  # Obtain a total count of nodes
  n_nodes <-
    count_nodes(graph)

  # Expect that the `total_count_of_nodes` object
  # is a named vector
  expect_type(
    n_nodes, "integer")

  expect_equal(
    n_nodes, 5)
})

test_that("getting an edge count (with `count_edges()`) is possible", {

  # Create a simple graph
  graph <-
    create_graph() %>%
    add_path(n = 5)

  # Obtain a total count of edges
  n_edges <-
    count_edges(graph)

  # Expect that the `n_edges` object is
  # an integer
  expect_equal(n_edges, 4L)
})

test_that("getting a node/edge count for an empty graph is possible", {

  # Create an empty graph
  empty_graph <- create_graph()

  # Expect that a node count of an empty graph
  # will return `0`
  expect_equal(
    count_nodes(
      graph = empty_graph), 0)

  # Expect that an edge count of an empty graph
  # will return `0`
  expect_equal(
    count_edges(
      graph = empty_graph), 0)
})

test_that("counting the number of edges with the same definition is possible", {

  # Create a node data frame (ndf)
  ndf <-
    create_node_df(
      n = 5,
      label = TRUE)

  # Create an edge data frame (edf)
  edf <-
    create_edge_df(
      from = c(1, 4, 4, 3, 5, 1, 3, 4),
        to = c(4, 1, 1, 2, 2, 2, 2, 1))

  # Create a graph with the ndf and edf
  graph <-
    create_graph(
      nodes_df = ndf,
      edges_df = edf)

  # Get the total number of edge
  # definitions (e.g., `4` -> `1`) where
  # there are multiple edges (i.e.,
  # distinct edges with separate edge
  # ID values)
  edges_where_multiple_occur <-
    get_edge_count_w_multiedge(graph = graph)

  # Expect that there are 2 such
  # edge definitions
  expect_equal(
    edges_where_multiple_occur, 2)
})

test_that("counting the number of multiple edges between a specific node pair is possible", {

  # Create a node data frame (ndf)
  ndf <-
    create_node_df(
      n = 5,
      label = TRUE)

  # Create an edge data frame (edf)
  edf <-
    create_edge_df(
      from = c(1, 4, 4, 3, 5, 1, 3, 4),
      to = c(4, 1, 1, 2, 2, 2, 2, 1))

  # Create a graph with the ndf and edf
  graph <-
    create_graph(
      nodes_df = ndf,
      edges_df = edf)

  # Get the total number of multiple
  # edges (those edges that share an
  # edge definition) in the graph
  global_multiple_edges_count <-
    get_multiedge_count(graph = graph)

  # Expect that there are 3 edges that
  # serve as multiple edges
  expect_equal(
    global_multiple_edges_count, 3)
})

test_that("counting the number of strongly connected components is possible", {

  # Create a graph with several
  # graph islands
  graph <-
     create_graph() %>%
     add_islands_graph(
       n_islands = 4,
       island_size = 10,
       p = 1/5,
       edges_between = 1,
       set_seed = 23)

  # Expect that the count of strongly
  # connected components is 4
  expect_equal(
    graph %>% count_s_connected_cmpts(), 4)

  # Expect that an empty graph will
  # return NA
  expect_true(
    is.na(count_s_connected_cmpts(graph = create_graph())))
})

test_that("counting the number of weakly connected components is possible", {

  # Create a graph with 2 cycles
  graph <-
    create_graph() %>%
    add_cycle(n = 5) %>%
    add_cycle(n = 5)

  # Expect that the count of weakly
  # connected components is 2
  expect_equal(
    graph %>% count_w_connected_cmpts(), 2)

  # Expect that an empty graph will
  # return NA
  expect_true(
    is.na(count_w_connected_cmpts(graph = create_graph())))
})

test_that("counting the number of loop edges is possible", {

  # Create a full graph that
  # includes loops
  graph <-
    create_graph(
      directed = FALSE) %>%
    add_full_graph(
      n = 3,
      keep_loops = TRUE)

  # Expect that the number of
  # loop edges is 3
  expect_equal(
    graph %>% count_loop_edges(), 3)

  # Expect that an empty graph will
  # return 0
  expect_equal(
    count_loop_edges(graph = create_graph()), 0)
})

test_that("counting the unconnected graph nodes is possible", {

  # Create a graph with a
  # path of nodes and 3
  # unconnected nodes
  graph <-
    create_graph() %>%
    add_path(n = 3) %>%
    add_n_nodes(n = 3)

  # Expect that the count of
  # unconnected nodes is 3
  expect_equal(
    graph %>% count_unconnected_nodes(), 3)

  # Expect that an empty graph will
  # return 0
  expect_equal(
    count_unconnected_nodes(graph = create_graph()), 0)

  # Expect that a graph without unconnected
  # nodes will return 0
  expect_equal(
    create_graph() %>%
      add_path(n = 4) %>%
      count_unconnected_nodes(), 0)
})
