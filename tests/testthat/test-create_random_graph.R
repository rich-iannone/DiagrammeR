context("Creating a random graph")

test_that("a random graph can be made to various specifications", {

  # Create a random, directed graph with 10 nodes and 15 edges
  random_graph_10_15_directed <-
    create_random_graph(
      n = 10, m = 15,
      directed = TRUE)

  # Expect 10 nodes in graph
  expect_equal(
    count_nodes(graph = random_graph_10_15_directed), 10)

  # Expect 15 edges in graph
  expect_equal(
    count_edges(graph = random_graph_10_15_directed), 15)

  # Expect the graph is directed
  expect_true(
    is_graph_directed(graph = random_graph_10_15_directed))

  # Expect that all nodes have IDs from 1 to 10
  expect_true(
    all(get_node_ids(x = random_graph_10_15_directed) == 1:10))

  # Expect that labels are present are match node
  # IDs from `1` to `10`
  expect_true(
    all(
      get_node_df(graph = random_graph_10_15_directed)$nodes ==
        get_node_df(graph = random_graph_10_15_directed)$label))

  # Create a random, undirected graph with 10
  # nodes and 15 edges
  random_graph_10_15 <-
    create_random_graph(
      n = 10, m = 15)

  # Expect 10 nodes in graph
  expect_equal(
    count_nodes(graph = random_graph_10_15), 10)

  # Expect 15 edges in graph
  expect_equal(
    count_edges(graph = random_graph_10_15), 15)

  # Expect the graph is directed
  expect_true(
    is_graph_directed(graph = random_graph_10_15))

  # Expect that all nodes have IDs from 1 to 10
  expect_true(
    all(
      get_node_ids(x = random_graph_10_15) %in% 1:10))

  # Create a directed graph with a seed set so
  # that it's reproducible
  random_graph_10_15_seed_set <-
    create_random_graph(
      n = 10, m = 15,
      set_seed = 23)

  # Expect 10 nodes in graph
  expect_equal(
    count_nodes(graph = random_graph_10_15_seed_set), 10)

  # Expect 15 edges in graph
  expect_equal(
    count_edges(graph = random_graph_10_15_seed_set), 15)

  # Expect the graph is directed
  expect_true(
    is_graph_directed(graph = random_graph_10_15_seed_set))

  # Create a random, directed graph with 10 nodes
  # and 15 edges and no node labels
  random_graph_10_15_directed_no_labels <-
    create_random_graph(
      n = 10, m = 15,
      directed = TRUE,
      display_labels = FALSE)

  # Expect that labels are not present
  expect_equal(
    get_node_df(graph = random_graph_10_15_directed_no_labels)$label,
    rep(as.character(NA), 10))
})
