context("Add graphs to graphs")

test_that("Adding a balanced tree is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add a balanced tree
  graph <-
    add_balanced_tree(
      graph, k = 3, h = 3, type = "a", rel = "z")

  # Expect 40 nodes to have been created
  expect_equal(node_count(graph), 40)

  # Expect 39 edges to have been created
  expect_equal(edge_count(graph), 39)

  # Expect node ID values from 1 to 40
  expect_identical(get_nodes(graph),
                   as.character(1:40))

  # Expect label values from 1 to 40
  expect_identical(graph$nodes_df$label,
                   as.character(1:40))

  # Expect type values to all be `a`
  expect_equal(unique(graph$nodes_df$type), "a")

  # Expect rel values to all be `z`
  expect_equal(unique(graph$edges_df$rel), "z")

  # Expect an error if k is <2
  expect_error(
    add_balanced_tree(
      graph, k = 1, h = 3, type = "a", rel = "z"))

  # Expect an error if h is <2
  expect_error(
    add_balanced_tree(
      graph, k = 3, h = 1, type = "a", rel = "z"))

  # Add another balanced tree
  graph <-
    add_balanced_tree(
      graph, k = 2, h = 2, type = "b", rel = "y")

  # Expect that 47 nodes are now in the graph
  expect_equal(node_count(graph), 47)

  # Expect 45 edges are now in the graph
  expect_equal(edge_count(graph), 45)

  # Expect node ID values from 1 to 47
  expect_identical(get_nodes(graph),
                   as.character(1:47))

  # Expect label values from 1 to 47
  expect_identical(graph$nodes_df$label,
                   as.character(1:47))

  # Expect type values to be either `a` or `b`
  expect_identical(unique(graph$nodes_df$type),
                   c("a", "b"))

  # Expect rel values to be either `a` or `b`
  expect_identical(unique(graph$edges_df$rel),
                   c("z", "y"))
})

test_that("Adding a cycle is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add a cycle
  graph <-
    add_cycle(
      graph, n = 3, type = "a", rel = "z")

  # Expect 3 nodes to have been created
  expect_equal(node_count(graph), 3)

  # Expect 3 edges to have been created
  expect_equal(edge_count(graph), 3)

  # Expect node ID values from 1 to 3
  expect_identical(get_nodes(graph),
                   as.character(1:3))

  # Expect label values from 1 to 3
  expect_identical(graph$nodes_df$label,
                   as.character(1:3))

  # Expect type values to all be `a`
  expect_equal(unique(graph$nodes_df$type), "a")

  # Expect rel values to all be `z`
  expect_equal(unique(graph$edges_df$rel), "z")

  # Expect an error if n is <3
  expect_error(
    add_cycle(
      graph, n = 2, type = "a", rel = "z"))

  # Add another cycle
  graph <-
    add_cycle(
      graph, n = 3, type = "b", rel = "y")

  # Expect that 6 nodes are now in the graph
  expect_equal(node_count(graph), 6)

  # Expect 6 edges are now in the graph
  expect_equal(edge_count(graph), 6)

  # Expect node ID values from 1 to 6
  expect_identical(get_nodes(graph),
                   as.character(1:6))

  # Expect label values from 1 to 6
  expect_identical(graph$nodes_df$label,
                   as.character(1:6))

  # Expect type values to be either `a` or `b`
  expect_identical(unique(graph$nodes_df$type),
                   c("a", "b"))

  # Expect rel values to be either `a` or `b`
  expect_identical(unique(graph$edges_df$rel),
                   c("z", "y"))
})

test_that("Adding a path is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add a path
  graph <-
    add_path(
      graph, n = 3, type = "a", rel = "z")

  # Expect 3 nodes to have been created
  expect_equal(node_count(graph), 3)

  # Expect 2 edges to have been created
  expect_equal(edge_count(graph), 2)

  # Expect node ID values from 1 to 3
  expect_identical(get_nodes(graph),
                   as.character(1:3))

  # Expect label values from 1 to 3
  expect_identical(graph$nodes_df$label,
                   as.character(1:3))

  # Expect type values to all be `a`
  expect_equal(unique(graph$nodes_df$type), "a")

  # Expect rel values to all be `z`
  expect_equal(unique(graph$edges_df$rel), "z")

  # Expect an error if n is <2
  expect_error(
    add_path(
      graph, n = 1, type = "a", rel = "z"))

  # Add another path
  graph <-
    add_path(
      graph, n = 3, type = "b", rel = "y")

  # Expect that 6 nodes are now in the graph
  expect_equal(node_count(graph), 6)

  # Expect 4 edges are now in the graph
  expect_equal(edge_count(graph), 4)

  # Expect node ID values from 1 to 6
  expect_identical(get_nodes(graph),
                   as.character(1:6))

  # Expect label values from 1 to 6
  expect_identical(graph$nodes_df$label,
                   as.character(1:6))

  # Expect type values to be either `a` or `b`
  expect_identical(unique(graph$nodes_df$type),
                   c("a", "b"))

  # Expect rel values to be either `a` or `b`
  expect_identical(unique(graph$edges_df$rel),
                   c("z", "y"))
})

test_that("Adding a prism is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add a prism
  graph <-
    add_prism(
      graph, n = 3, type = "a", rel = "z")

  # Expect 6 nodes to have been created
  expect_equal(node_count(graph), 6)

  # Expect 9 edges to have been created
  expect_equal(edge_count(graph), 9)

  # Expect node ID values from 1 to 6
  expect_identical(get_nodes(graph),
                   as.character(1:6))

  # Expect label values from 1 to 6
  expect_identical(graph$nodes_df$label,
                   as.character(1:6))

  # Expect type values to all be `a`
  expect_equal(unique(graph$nodes_df$type), "a")

  # Expect rel values to all be `z`
  expect_equal(unique(graph$edges_df$rel), "z")

  # Expect an error if n is <3
  expect_error(
    add_prism(
      graph, n = 2, type = "a", rel = "z"))

  # Add another prism
  graph <-
    add_prism(
      graph, n = 3, type = "b", rel = "y")

  # Expect that 12 nodes are now in the graph
  expect_equal(node_count(graph), 12)

  # Expect 18 edges are now in the graph
  expect_equal(edge_count(graph), 18)

  # Expect node ID values from 1 to 12
  expect_identical(get_nodes(graph),
                   as.character(1:12))

  # Expect label values from 1 to 12
  expect_identical(graph$nodes_df$label,
                   as.character(1:12))

  # Expect type values to be either `a` or `b`
  expect_identical(unique(graph$nodes_df$type),
                   c("a", "b"))

  # Expect rel values to be either `a` or `b`
  expect_identical(unique(graph$edges_df$rel),
                   c("z", "y"))
})

test_that("Adding a star is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add a star
  graph <-
    add_star(
      graph, n = 4, type = "a", rel = "z")

  # Expect 4 nodes to have been created
  expect_equal(node_count(graph), 4)

  # Expect 3 edges to have been created
  expect_equal(edge_count(graph), 3)

  # Expect node ID values from 1 to 4
  expect_identical(get_nodes(graph),
                   as.character(1:4))

  # Expect label values from 1 to 4
  expect_identical(graph$nodes_df$label,
                   as.character(1:4))

  # Expect type values to all be `a`
  expect_equal(unique(graph$nodes_df$type), "a")

  # Expect rel values to all be `z`
  expect_equal(unique(graph$edges_df$rel), "z")

  # Expect an error if n is <4
  expect_error(
    add_star(
      graph, n = 3, type = "a", rel = "z"))

  # Add another star
  graph <-
    add_star(
      graph, n = 4, type = "b", rel = "y")

  # Expect that 8 nodes are now in the graph
  expect_equal(node_count(graph), 8)

  # Expect 6 edges are now in the graph
  expect_equal(edge_count(graph), 6)

  # Expect node ID values from 1 to 8
  expect_identical(get_nodes(graph),
                   as.character(1:8))

  # Expect label values from 1 to 8
  expect_identical(graph$nodes_df$label,
                   as.character(1:8))

  # Expect type values to be either `a` or `b`
  expect_identical(unique(graph$nodes_df$type),
                   c("a", "b"))

  # Expect rel values to be either `a` or `b`
  expect_identical(unique(graph$edges_df$rel),
                   c("z", "y"))
})
