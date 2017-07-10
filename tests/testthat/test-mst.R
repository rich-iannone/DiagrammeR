context("Obtaining the minimum spanning tree of a graph")

test_that("the MST algorithm is functional", {

  # Create a random graph and obtain Jaccard
  # similarity values for each pair of nodes
  # as a square matrix
  j_sim_matrix <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 1) %>%
    get_jaccard_similarity()

  # Create a weighted, undirected graph from the
  # resultant matrix (effectively treating that
  # matrix as an adjacency matrix)
  graph <-
    j_sim_matrix %>%
    from_adj_matrix(weighted = TRUE)

  # The graph in this case is a fully connected graph
  # with loops, where jaccard similarity values are
  # assigned as edge weights (edge attribute `weight`);
  # The minimum spanning tree for this graph is the
  # connected subgraph where the edges retained have
  # the lowest similarity values possible
  min_spanning_tree_graph <-
    graph %>%
    get_min_spanning_tree()

  # Expect no loops in `min_spanning_tree_graph`
  expect_true(
    all(node_info(min_spanning_tree_graph)$loops == 0))

  # Expect that all nodes are connected
  expect_true(
    is_graph_connected(min_spanning_tree_graph))
})
