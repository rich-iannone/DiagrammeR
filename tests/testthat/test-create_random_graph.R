context("Creating a random graph")

test_that("a random graph can be made to various specifications", {

  # Create a random, directed graph with 10 nodes and 15 edges
  random_graph_10_15_directed <-
    create_random_graph(10, 15, directed = TRUE)

  # Expect 10 nodes in graph
  expect_equal(node_count(random_graph_10_15_directed), 10)

  # Expect 15 edges in graph
  expect_equal(edge_count(random_graph_10_15_directed), 15)

  # Expect the graph is directed
  expect_true(is_graph_directed(random_graph_10_15_directed))

  # Expect that all nodes have IDs from 1 to 10
  expect_true(all(get_nodes(random_graph_10_15_directed) == as.character(1:10)))

  # Expect that labels are present are match node IDs from 1 to 10
  expect_true(
    all(get_node_df(random_graph_10_15_directed)$nodes ==
          get_node_df(random_graph_10_15_directed)$label)
  )

  # Create a random, undirected graph with 10 nodes and 15 edges
  random_graph_10_15 <- create_random_graph(10, 15)

  # Expect 10 nodes in graph
  expect_equal(node_count(random_graph_10_15), 10)

  # Expect 15 edges in graph
  expect_equal(edge_count(random_graph_10_15), 15)

  # Expect the graph is not directed
  expect_false(is_graph_directed(random_graph_10_15))

  # Expect that all nodes have IDs from 1 to 10
  expect_true(all(get_nodes(random_graph_10_15) %in% as.character(1:10)))

  # Create a random, undirected graph that's fully connected
  random_graph_10_15_fully_connected <-
    create_random_graph(10, 15, fully_connected = TRUE)

  # Expect no loops in the fully connected graph
  expect_true(all(node_info(random_graph_10_15_fully_connected)$loops == 0))

  # Expect 10 nodes in graph
  expect_equal(node_count(random_graph_10_15_fully_connected), 10)

  # Expect the graph is directed
  expect_false(is_graph_directed(random_graph_10_15_fully_connected))

  # Create a directed graph with a seed set so that it's reproducible
  random_graph_10_15_seed_set <-
    create_random_graph(10, 15, set_seed = 50)

  # Expect 10 nodes in graph
  expect_equal(node_count(random_graph_10_15_seed_set), 10)

  # Expect 15 edges in graph
  expect_equal(edge_count(random_graph_10_15_seed_set), 15)

  # Expect the graph is not directed
  expect_false(is_graph_directed(random_graph_10_15_seed_set))

  # Expect the node attribute 'value' to sum to 44
  expect_equal(
    sum(as.numeric(get_node_df(random_graph_10_15_seed_set)[,4])),
    44
  )

  # Expect the last edge in the graph's edge list to be "1" -> "8"
  expect_equal(
    get_edges(random_graph_10_15_seed_set, return_type = "vector")[15],
    "1 -> 8"
  )

  # Create a random, directed graph with 10 nodes and 15 edges and no
  # node labels
  random_graph_10_15_directed_no_labels <-
    create_random_graph(10, 15, directed = TRUE, display_labels = FALSE)

  # Expect that labels are not present
  expect_true(
    all(get_node_df(random_graph_10_15_directed_no_labels)$label ==
          "")
  )

  # Create a directed, random graph with a supplied set of node IDs
  random_graph_10_15_directed_letters <-
    create_random_graph(10, 15,
                        directed = TRUE,
                        node_id = LETTERS)

  # Expect 10 nodes in graph
  expect_equal(node_count(random_graph_10_15_directed_letters), 10)

  # Expect 15 edges in graph
  expect_equal(edge_count(random_graph_10_15_directed_letters), 15)

  # Expect the graph is directed
  expect_true(is_graph_directed(random_graph_10_15_directed_letters))

  # Expect that all nodes have IDs from 1 to 10
  expect_true(all(get_nodes(random_graph_10_15_directed_letters) == LETTERS[1:10]))

  # Expect an error when creating a random graph that has more
  # edges that can be accommodated
  expect_error(
    create_random_graph(10, 50)
  )

  # Expect an error when creating a random graph with supplied node ID
  # values but those supplied values are not unique
  expect_error(
    create_random_graph(4, 4, node_id = c("A", "B", "B", "C"))
  )

  # Expect an error when creating a random graph with supplied node ID
  # values but number of those supplied values is less than the number of
  # nodes requested
  expect_error(
    create_random_graph(4, 4, node_id = c("A", "B"))
  )
})
