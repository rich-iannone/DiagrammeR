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
  expect_identical(get_node_ids(graph), 1:40)

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
  expect_identical(get_node_ids(graph), 1:47)

  # Expect label values from 1 to 47
  expect_equal(graph$nodes_df$label,
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
    add_cycle(graph, n = 3, type = "a", rel = "z")

  # Expect 3 nodes to have been created
  expect_equal(node_count(graph), 3)

  # Expect 3 edges to have been created
  expect_equal(edge_count(graph), 3)

  # Expect node ID values from 1 to 3
  expect_identical(get_node_ids(graph), 1:3)

  # Expect label values from 1 to 3
  expect_identical(graph$nodes_df$label,
                   as.character(1:3))

  # Expect type values to all be `a`
  expect_equal(unique(graph$nodes_df$type), "a")

  # Expect rel values to all be `z`
  expect_equal(unique(graph$edges_df$rel), "z")

  # Expect an error if n is <3
  expect_error(
    add_cycle(graph, n = 2, type = "a", rel = "z"))

  # Add another cycle
  graph <-
    add_cycle(graph, n = 3, type = "b", rel = "y")

  # Expect that 6 nodes are now in the graph
  expect_equal(node_count(graph), 6)

  # Expect 6 edges are now in the graph
  expect_equal(edge_count(graph), 6)

  # Expect node ID values from 1 to 6
  expect_identical(get_node_ids(graph), 1:6)

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
    add_path(graph, n = 3, type = "a", rel = "z")

  # Expect 3 nodes to have been created
  expect_equal(node_count(graph), 3)

  # Expect 2 edges to have been created
  expect_equal(edge_count(graph), 2)

  # Expect node ID values from 1 to 3
  expect_identical(get_node_ids(graph), 1:3)

  # Expect label values from 1 to 3
  expect_identical(graph$nodes_df$label,
                   as.character(1:3))

  # Expect type values to all be `a`
  expect_equal(unique(graph$nodes_df$type), "a")

  # Expect rel values to all be `z`
  expect_equal(unique(graph$edges_df$rel), "z")

  # Expect an error if n is <2
  expect_error(
    add_path(graph, n = 1, type = "a", rel = "z"))

  # Add another path
  graph <-
    add_path(graph, n = 3, type = "b", rel = "y")

  # Expect that 6 nodes are now in the graph
  expect_equal(node_count(graph), 6)

  # Expect 4 edges are now in the graph
  expect_equal(edge_count(graph), 4)

  # Expect node ID values from 1 to 6
  expect_identical(get_node_ids(graph), 1:6)

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
    add_prism(graph, n = 3, type = "a", rel = "z")

  # Expect 6 nodes to have been created
  expect_equal(node_count(graph), 6)

  # Expect 9 edges to have been created
  expect_equal(edge_count(graph), 9)

  # Expect node ID values from 1 to 6
  expect_identical(get_node_ids(graph), 1:6)

  # Expect label values from 1 to 6
  expect_identical(graph$nodes_df$label,
                   as.character(1:6))

  # Expect type values to all be `a`
  expect_equal(unique(graph$nodes_df$type), "a")

  # Expect rel values to all be `z`
  expect_equal(unique(graph$edges_df$rel), "z")

  # Expect an error if n is <3
  expect_error(
    add_prism(graph, n = 2, type = "a", rel = "z"))

  # Add another prism
  graph <-
    add_prism(graph, n = 3, type = "b", rel = "y")

  # Expect that 12 nodes are now in the graph
  expect_equal(node_count(graph), 12)

  # Expect 18 edges are now in the graph
  expect_equal(edge_count(graph), 18)

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
    add_star(graph, n = 4, type = "a", rel = "z")

  # Expect 4 nodes to have been created
  expect_equal(node_count(graph), 4)

  # Expect 3 edges to have been created
  expect_equal(edge_count(graph), 3)

  # Expect node ID values from 1 to 4
  expect_identical(get_node_ids(graph), 1:4)

  # Expect label values from 1 to 4
  expect_identical(graph$nodes_df$label,
                   as.character(1:4))

  # Expect type values to all be `a`
  expect_equal(unique(graph$nodes_df$type), "a")

  # Expect rel values to all be `z`
  expect_equal(unique(graph$edges_df$rel), "z")

  # Expect an error if n is <4
  expect_error(
    add_star(graph, n = 3, type = "a", rel = "z"))

  # Add another star
  graph <-
    add_star(graph, n = 4, type = "b", rel = "y")

  # Expect that 8 nodes are now in the graph
  expect_equal(node_count(graph), 8)

  # Expect 6 edges are now in the graph
  expect_equal(edge_count(graph), 6)

  # Expect node ID values from 1 to 8
  expect_identical(get_node_ids(graph), 1:8)

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

test_that("Adding a full graph is possible", {

  # Create a graph and add a full graph
  # with 3 nodes to it; keep loops
  graph <-
    create_graph() %>%
    add_full_graph(
      n = 3, keep_loops = TRUE)

  # Expect 3 nodes to have been created
  expect_equal(node_count(graph), 3)

  # Expect 9 edges to have been created
  expect_equal(edge_count(graph), 9)

  # Expect that there are 3 loops in the graph
  expect_equal(
    length(
      which(
        graph$edges_df$from ==
          graph$edges_df$to)), 3)

  # Expect node ID values from 1 to 3
  expect_identical(get_node_ids(graph), 1:3)

  # Expect type values to be character NA values
  expect_equal(unique(graph$nodes_df$type),
               as.character(NA))

  # Create a graph and add a full graph
  # with 3 nodes to it; discard loops
  graph <-
    create_graph() %>%
    add_full_graph(
      n = 3, keep_loops = FALSE)

  # Expect 3 nodes to have been created
  expect_equal(node_count(graph), 3)

  # Expect 6 edges to have been created
  expect_equal(edge_count(graph), 6)

  # Expect that there are no loops in the graph
  expect_equal(
    length(
      which(
        graph$edges_df$from ==
          graph$edges_df$to)), 0)

  # Create a graph, add a full graph
  # (with no loops), set values for the
  # node `label`, node `type`, and edge `rel`
  graph <-
    create_graph() %>%
    add_full_graph(
      n = 3,
      type = "connected",
      label = c("1st", "2nd", "3rd"),
      rel = "connected_to")

  # Expect 3 nodes to have been created
  expect_equal(node_count(graph), 3)

  # Expect 6 edges to have been created
  expect_equal(edge_count(graph), 6)

  # Expect that there are no loops in the graph
  expect_equal(
    length(
      which(
        graph$edges_df$from ==
          graph$edges_df$to)), 0)

  # Expect that the `type` node attr
  # has the value `connected` for all
  # nodes created
  expect_equal(
    unique(get_node_df(graph)[, 2]),
    "connected")

  # Expect that the `label` values
  # assigned to the nodes are in the
  # graph's internal node data frame
  expect_equal(
    graph$nodes_df$label,
    c("1st", "2nd", "3rd"))

  # Expect that the `label` edge attr
  # has the value `connected_to` for
  # all edges created
  expect_equal(
    unique(graph$edges_df$rel),
    "connected_to")

  # Create a fully-connected and directed
  # graph with 3 nodes, and, where a matrix
  # provides edge weights; first, create a
  # matrix to be used for edge weights
  # matrix (with row names to be used as
  # node labels)
  set.seed(23)

  edge_wt_matrix_rownames <-
    rnorm(100, 5, 2) %>%
    sample(9, FALSE) %>%
    round(2) %>%
    matrix(
      nc = 3, nr = 3,
      dimnames = list(c("a", "b", "c")))

  # Create a fully-connected graph without
  # loops and use the matrix to provide
  # values for the edge `weight` attribute
  graph <-
    create_graph() %>%
    add_full_graph(
      n = 3,
      type = "weighted",
      label = TRUE,
      rel = "related_to",
      edge_wt_matrix = edge_wt_matrix_rownames,
      keep_loops = FALSE)

  # Expect 3 nodes to have been created
  expect_equal(node_count(graph), 3)

  # Expect 6 edges to have been created
  expect_equal(edge_count(graph), 6)

  # Expect that there are no loops in the graph
  expect_equal(
    length(
      which(
        graph$edges_df$from ==
          graph$edges_df$to)), 0)

  # Expect that the `type` node attr
  # has the value `connected` for all
  # nodes created
  expect_equal(
    unique(get_node_df(graph)[, 2]),
    "weighted")

  # Expect that the `label` values
  # assigned to the nodes are the same
  # as the matrix rownames
  expect_equal(
    graph$nodes_df$label,
    rownames(edge_wt_matrix_rownames))

  # Expect that the `rel` edge attr
  # has the value `connected_to` for
  # all edges created
  expect_equal(
    unique(graph$edges_df$rel),
    "related_to")

  # Expect certain values for the
  # edge weight attribute
  expect_equal(
    graph$edges_df$weight,
    c(3.3, 5.02, 4.13,
      6.49, 6.03, 5.55))

  # Create a fully-connected but undirected
  # graph without loops and use the matrix
  # to provide values for the edge `weight`
  # attribute; in this case, the lower
  # triangle of the matrix will be used
  graph <-
    create_graph(directed = FALSE) %>%
    add_full_graph(
      n = 3,
      type = "weighted",
      label = TRUE,
      rel = "related_to",
      edge_wt_matrix = edge_wt_matrix_rownames,
      keep_loops = FALSE)

  # Expect 3 nodes to have been created
  expect_equal(node_count(graph), 3)

  # Expect 3 edges to have been created
  expect_equal(edge_count(graph), 3)

  # Expect that there are no loops in the graph
  expect_equal(
    length(
      which(
        graph$edges_df$from ==
          graph$edges_df$to)), 0)

  # Expect that the `type` node attr
  # has the value `connected` for all
  # nodes created
  expect_equal(
    unique(graph$nodes_df$type),
    "weighted")

  # Expect that the `label` values
  # assigned to the nodes are the same
  # as the matrix rownames
  expect_equal(
    graph$nodes_df$label,
    rownames(edge_wt_matrix_rownames))

  # Expect that the `rel` edge attr
  # has the value `connected_to` for
  # all edges created
  expect_equal(
    unique(graph$edges_df$rel),
    "related_to")

  # Expect certain values for the
  # edge weight attribute
  expect_equal(
    graph$edges_df$weight,
    c(3.3, 5.02, 6.49))

  # Create a fully-connected graph with
  # loop preserved and use the matrix to
  # provide values for the edge `weight`
  # attribute
  graph <-
    create_graph() %>%
    add_full_graph(
      n = 3,
      type = "weighted",
      label = TRUE,
      rel = "related_to",
      edge_wt_matrix = edge_wt_matrix_rownames,
      keep_loops = TRUE)

  # Expect 3 nodes to have been created
  expect_equal(node_count(graph), 3)

  # Expect 9 edges to have been created
  expect_equal(edge_count(graph), 9)

  # Expect that there are 3 loops in the graph
  expect_equal(
    length(
      which(
        graph$edges_df$from ==
          graph$edges_df$to)), 3)

  # Expect that the `type` node attr
  # has the value `connected` for all
  # nodes created
  expect_equal(
    unique(graph$nodes_df$type),
    "weighted")

  # Expect that the `label` values
  # assigned to the nodes are the same
  # as the matrix rownames
  expect_equal(
    graph$nodes_df$label,
    rownames(edge_wt_matrix_rownames))

  # Expect that the `rel` edge attr
  # has the value `connected_to` for
  # all edges created
  expect_equal(
    unique(graph$edges_df$rel),
    "related_to")

  # Expect certain values for the
  # edge weight attribute
  expect_equal(
    graph$edges_df$weight,
    c(8.66, 3.3, 5.02, 4.13, 6.83,
      6.49, 6.03, 5.55, 3.8))

  # Create a fully-connected and undirected
  # graph with loops preserved; use the
  # matrix to provide values for the edge
  # `weight` attribute
  graph <-
    create_graph(directed = FALSE) %>%
    add_full_graph(
      n = 3,
      type = "weighted",
      label = TRUE,
      rel = "related_to",
      edge_wt_matrix = edge_wt_matrix_rownames,
      keep_loops = TRUE)

  # Expect 3 nodes to have been created
  expect_equal(node_count(graph), 3)

  # Expect 6 edges to have been created
  expect_equal(edge_count(graph), 6)

  # Expect that there are 3 loops in the graph
  expect_equal(
    length(
      which(
        graph$edges_df$from ==
          graph$edges_df$to)), 3)

  # Expect that the `type` node attr
  # has the value `connected` for all
  # nodes created
  expect_equal(
    unique(graph$nodes_df$type),
    "weighted")

  # Expect that the `label` values
  # assigned to the nodes are the same
  # as the matrix rownames
  expect_equal(
    graph$nodes_df$label,
    rownames(edge_wt_matrix_rownames))

  # Expect that the `rel` edge attr
  # has the value `connected_to` for
  # all edges created
  expect_equal(
    unique(graph$edges_df$rel),
    "related_to")

  # Expect certain values for the
  # edge weight attribute
  expect_equal(
    graph$edges_df$weight,
    c(8.66, 3.3, 5.02, 6.83, 6.49, 3.8))

  # Create a fully-connected and undirected
  # graph with loops preserved; use the
  # matrix to provide values for the edge
  # `weight` attribute, however, do not add
  # labels to this graph
  graph <-
    create_graph(directed = FALSE) %>%
    add_full_graph(
      n = 3,
      type = "weighted",
      label = NULL,
      rel = "related_to",
      edge_wt_matrix = edge_wt_matrix_rownames,
      keep_loops = TRUE)

  # Expect 3 nodes to have been created
  expect_equal(node_count(graph), 3)

  # Expect 6 edges to have been created
  expect_equal(edge_count(graph), 6)

  # Expect that there are 3 loops in the graph
  expect_equal(
    length(
      which(
        graph$edges_df$from ==
          graph$edges_df$to)), 3)

  # Expect that the `type` node attr
  # has the value `connected` for all
  # nodes created
  expect_equal(
    unique(graph$nodes_df$type),
    "weighted")

  # Expect that the `label` values are
  # character NA values
  expect_equal(
    graph$nodes_df$label,
    rep(as.character(NA), 3))

  # Expect that the `rel` edge attr
  # has the value `connected_to` for
  # all edges created
  expect_equal(
    unique(graph$edges_df$rel),
    "related_to")

  # Expect certain values for the
  # edge weight attribute
  expect_equal(
    graph$edges_df$weight,
    c(8.66, 3.3, 5.02, 6.83, 6.49, 3.8))

  # Create a graph with a cycle then
  # and add a full graph
  # with 3 nodes to it; discard loops
  graph <-
    create_graph() %>%
    add_cycle(5, "cycle") %>%
    add_full_graph(
      n = 8, keep_loops = FALSE)

  # Expect 13 nodes to have been created
  expect_equal(node_count(graph), 13)

  # Expect 6 edges to have been created
  expect_equal(edge_count(graph), 61)
})
