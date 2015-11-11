context("Getting node IDs in traversal paths")

test_that("getting lists of node IDs using `get_paths()` is possible", {

  library(magrittr)

  # Create a simple graph
  graph <-
    create_graph() %>%
    add_node_df(create_nodes(1:8)) %>%
    add_edge("1", "2") %>% add_edge("1", "3") %>%
    add_edge("3", "4") %>% add_edge("3", "5") %>%
    add_edge("4", "6") %>% add_edge("2", "7") %>%
    add_edge("7", "5") %>% add_edge("4", "8")

  # Get a list of all paths outward from node "1"
  paths_from_1 <- get_paths(graph, from = "1")

  # Expect a list of 4 components
  expect_equal(length(paths_from_1), 4)

  # Expect specific node IDs in each list
  expect_true(all(paths_from_1[[1]] == c("1", "3", "5")))
  expect_true(all(paths_from_1[[2]] == c("1", "2", "7", "5")))
  expect_true(all(paths_from_1[[3]] == c("1", "3", "4", "6")))
  expect_true(all(paths_from_1[[4]] == c("1", "3", "4", "8")))

  # Get a list of all paths leading to node "6"
  paths_to_6 <- get_paths(graph, to = "6")

  # Expect a list of 1 component
  expect_equal(length(paths_to_6), 1)

  # Expect specific node IDs in the list
  expect_true(all(paths_to_6[[1]] == c("1", "3", "4", "6")))

  #' # Get a list of all paths from "1" to "5"
  paths_from_1_to_5 <- get_paths(graph, from = "1", to = "5")

  # Expect a list of 2 components
  expect_equal(length(paths_from_1_to_5), 2)

  # Expect specific node IDs in each list
  expect_true(all(paths_from_1_to_5[[1]] == c("1", "3", "5")))
  expect_true(all(paths_from_1_to_5[[2]] == c("1", "2", "7", "5")))

  # Get a list of all paths from "1" up to a distance of 2 node traversals
  paths_from_1_dist_2 <- get_paths(graph, from = "1", distance = 2)

  # Expect a list of 3 components
  expect_equal(length(paths_from_1_dist_2), 3)

  # Expect specific node IDs in each list
  expect_true(all(paths_from_1_dist_2[[1]] == c("1", "3", "5")))
  expect_true(all(paths_from_1_dist_2[[2]] == c("1", "2", "7")))
  expect_true(all(paths_from_1_dist_2[[3]] == c("1", "3", "4")))

  # Get a list of the shortest paths from "1" to "5"
  paths_from_1_to_5_shortest <-
    get_paths(graph, from = "1", to = "5", shortest_path = TRUE)

  # Expect a list of 1 component
  expect_equal(length(paths_from_1_to_5_shortest), 1)

  # Expect specific node IDs for the one list
  expect_true(all(paths_from_1_to_5_shortest[[1]] == c("1", "3", "5")))

  # Get a list of the longest paths from "1" to "5"
  paths_from_1_to_5_longest <-
    get_paths(graph, from = "1", to = "5", longest_path = TRUE)

  # Expect a list of 1 component
  expect_equal(length(paths_from_1_to_5_longest), 1)

  # Expect specific node IDs for the one list
  expect_true(all(paths_from_1_to_5_longest[[1]] == c("1", "2", "7", "5")))
})
