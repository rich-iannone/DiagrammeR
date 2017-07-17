context("Add graphs to graphs")

test_that("Adding a balanced tree is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add a balanced tree
  graph <-
    add_balanced_tree(
      graph = graph,
      k = 3,
      h = 3,
      type = "a",
      rel = "z")

  # Expect 40 nodes to have been created
  expect_equal(
    node_count(graph), 40)

  # Expect 39 edges to have been created
  expect_equal(
    edge_count(graph), 39)

  # Expect node ID values from 1 to 40
  expect_identical(
    get_node_ids(graph), 1:40)

  # Expect label values from 1 to 40
  expect_identical(
    graph$nodes_df$label,
    as.character(1:40))

  # Expect type values to all be `a`
  expect_equal(
    unique(graph$nodes_df$type), "a")

  # Expect rel values to all be `z`
  expect_equal(
    unique(graph$edges_df$rel), "z")

  # Expect an error if k is <2
  expect_error(
    add_balanced_tree(
      graph = graph,
      k = 1,
      h = 3,
      type = "a",
      rel = "z"))

  # Expect an error if h is <2
  expect_error(
    add_balanced_tree(
      graph = graph,
      k = 3,
      h = 1,
      type = "a",
      rel = "z"))

  # Add another balanced tree to the graph
  graph <-
    add_balanced_tree(
      graph = graph,
      k = 2,
      h = 2,
      type = "b",
      rel = "y")

  # Expect that 47 nodes are now in the graph
  expect_equal(
    node_count(graph), 47)

  # Expect 45 edges are now in the graph
  expect_equal(
    edge_count(graph), 45)

  # Expect node ID values from 1 to 47
  expect_identical(
    get_node_ids(graph), 1:47)

  # Expect label values from 1 to 47
  expect_equal(
    graph$nodes_df$label,
    as.character(1:47))

  # Expect type values to be either `a` or `b`
  expect_identical(
    unique(graph$nodes_df$type),
    c("a", "b"))

  # Expect rel values to be either `a` or `b`
  expect_identical(
    unique(graph$edges_df$rel),
    c("z", "y"))
})

test_that("Adding a cycle is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add a cycle
  graph <-
    add_cycle(
      graph = graph,
      n = 3,
      type = "a",
      rel = "z")

  # Expect 3 nodes to have been created
  expect_equal(
    node_count(graph), 3)

  # Expect 3 edges to have been created
  expect_equal(
    edge_count(graph), 3)

  # Expect node ID values from 1 to 3
  expect_identical(
    get_node_ids(graph), 1:3)

  # Expect label values from 1 to 3
  expect_identical(
    graph$nodes_df$label,
    as.character(1:3))

  # Expect type values to all be `a`
  expect_equal(
    unique(graph$nodes_df$type), "a")

  # Expect rel values to all be `z`
  expect_equal(
    unique(graph$edges_df$rel), "z")

  # Expect an error if n is <3
  expect_error(
    add_cycle(
      graph = graph,
      n = 2,
      type = "a",
      rel = "z"))

  # Add another cycle to the graph
  graph <-
    add_cycle(
      graph = graph,
      n = 3,
      type = "b",
      rel = "y")

  # Expect that 6 nodes are now in the graph
  expect_equal(
    node_count(graph), 6)

  # Expect 6 edges are now in the graph
  expect_equal(
    edge_count(graph), 6)

  # Expect node ID values from 1 to 6
  expect_identical(
    get_node_ids(graph), 1:6)

  # Expect label values from 1 to 6
  expect_identical(
    graph$nodes_df$label,
    as.character(1:6))

  # Expect type values to be either `a` or `b`
  expect_identical(
    unique(graph$nodes_df$type),
    c("a", "b"))

  # Expect rel values to be either `a` or `b`
  expect_identical(
    unique(graph$edges_df$rel),
    c("z", "y"))

  # Create a graph with a cycle that
  # has different types of node and edge
  # attributes included
  graph_2 <-
    create_graph() %>%
    add_cycle(
      n = 6,
      label = c("one", "two",
                "three", "four",
                "five", "six"),
      type = c("a", "a",
               "b", "b",
               "c", "c"),
      value = c(1.2, 8.4,
                3.4, 5.2,
                6.1, 2.6),
      rel = c("cycle_a", "cycle_a",
              "cycle_b", "cycle_b",
              "cycle_c", "cycle_c"))

  # Get the graph's node data frame
  ndf <- get_node_df(graph_2)

  # Get the graph's edge data frame
  edf <- get_edge_df(graph_2)

  # Expect that 6 nodes are now in the graph
  expect_equal(
    node_count(graph_2), 6)

  # Expect 6 edges are now in the graph
  expect_equal(
    edge_count(graph_2), 6)

  # Expect node ID values from 1 to 6
  expect_identical(
    ndf$id, 1:6)

  # Expect specific node `label` values
  expect_identical(
    ndf$label,
    c("one", "two",
      "three", "four",
      "five", "six"))

  # Expect specific node `type` values
  expect_identical(
    ndf$type,
    c("a", "a", "b",
      "b", "c", "c"))

  # Expect specific node `value` values
  expect_identical(
    ndf$value,
    c(1.2, 8.4, 3.4, 5.2, 6.1, 2.6))

  # Expect specific edge `rel` values
  expect_identical(
    edf$rel,
    c("cycle_a", "cycle_a",
      "cycle_b", "cycle_b",
      "cycle_c", "cycle_c"))
})

test_that("Adding a path is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add a path
  graph <-
    add_path(
      graph = graph,
      n = 3,
      type = "a",
      rel = "z")

  # Expect 3 nodes to have been created
  expect_equal(
    node_count(graph), 3)

  # Expect 2 edges to have been created
  expect_equal(
    edge_count(graph), 2)

  # Expect node ID values from 1 to 3
  expect_identical(
    get_node_ids(graph), 1:3)

  # Expect label values from 1 to 3
  expect_identical(
    graph$nodes_df$label,
    as.character(1:3))

  # Expect type values to all be `a`
  expect_equal(
    unique(graph$nodes_df$type), "a")

  # Expect rel values to all be `z`
  expect_equal(
    unique(graph$edges_df$rel), "z")

  # Expect an error if n is <2
  expect_error(
    add_path(
      graph = graph,
      n = 1,
      type = "a",
      rel = "z"))

  # Add another path to the graph
  graph <-
    add_path(
      graph = graph,
      n = 3,
      type = "b",
      rel = "y")

  # Expect that 6 nodes are now in the graph
  expect_equal(
    node_count(graph), 6)

  # Expect 6 edges are now in the graph
  expect_equal(
    edge_count(graph), 4)

  # Expect node ID values from 1 to 6
  expect_identical(
    get_node_ids(graph), 1:6)

  # Expect label values from 1 to 6
  expect_identical(
    graph$nodes_df$label,
    as.character(1:6))

  # Expect type values to be either `a` or `b`
  expect_identical(
    unique(graph$nodes_df$type),
    c("a", "b"))

  # Expect rel values to be either `a` or `b`
  expect_identical(
    unique(graph$edges_df$rel),
    c("z", "y"))

  # Create a graph with a path that
  # has different types of node and edge
  # attributes included
  graph_2 <-
    create_graph() %>%
    add_path(
      n = 6,
      label = c("one", "two",
                "three", "four",
                "five", "six"),
      type = c("a", "a",
               "a", "b",
               "b", "b"),
      value = c(1.2, 8.4,
                3.4, 5.2,
                6.1, 2.6),
      rel = c("path_a", "path_b",
              "path_c", "path_d",
              "path_e"))

  # Get the graph's node data frame
  ndf <- get_node_df(graph_2)

  # Get the graph's edge data frame
  edf <- get_edge_df(graph_2)

  # Expect that 6 nodes are now in the graph
  expect_equal(
    node_count(graph_2), 6)

  # Expect 5 edges are now in the graph
  expect_equal(
    edge_count(graph_2), 5)

  # Expect node ID values from 1 to 6
  expect_identical(
    ndf$id, 1:6)

  # Expect specific node `label` values
  expect_identical(
    ndf$label,
    c("one", "two",
      "three", "four",
      "five", "six"))

  # Expect specific node `type` values
  expect_identical(
    ndf$type,
    c("a", "a", "a",
      "b", "b", "b"))

  # Expect specific node `value` values
  expect_identical(
    ndf$value,
    c(1.2, 8.4, 3.4, 5.2, 6.1, 2.6))

  # Expect specific edge `rel` values
  expect_identical(
    edf$rel,
    c("path_a", "path_b",
      "path_c", "path_d",
      "path_e"))
})

test_that("Adding a prism is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add a prism
  graph <-
    add_prism(
      graph = graph,
      n = 3,
      type = "a",
      rel = "z")

  # Expect 6 nodes to have been created
  expect_equal(
    node_count(graph), 6)

  # Expect 9 edges to have been created
  expect_equal(
    edge_count(graph), 9)

  # Expect node ID values from 1 to 6
  expect_identical(
    get_node_ids(graph), 1:6)

  # Expect label values from 1 to 6
  expect_identical(
    graph$nodes_df$label,
    as.character(1:6))

  # Expect type values to all be `a`
  expect_equal(
    unique(graph$nodes_df$type), "a")

  # Expect rel values to all be `z`
  expect_equal(
    unique(graph$edges_df$rel), "z")

  # Expect an error if n is <3
  expect_error(
    add_prism(
      graph = graph,
      n = 2,
      type = "a",
      rel = "z"))

  # Add another prism to the graph
  graph <-
    add_prism(
      graph = graph,
      n = 3,
      type = "b",
      rel = "y")

  # Expect that 12 nodes are now in the graph
  expect_equal(
    node_count(graph), 12)

  # Expect 18 edges are now in the graph
  expect_equal(
    edge_count(graph), 18)

  # Expect label values from 1 to 12
  expect_identical(
    graph$nodes_df$label,
    as.character(1:12))

  # Expect type values to be either `a` or `b`
  expect_identical(
    unique(graph$nodes_df$type),
    c("a", "b"))

  # Expect rel values to be either `a` or `b`
  expect_identical(
    unique(graph$edges_df$rel),
    c("z", "y"))

  # Create a graph with a prism that
  # has different types of node and edge
  # attributes included
  graph_2 <-
    create_graph() %>%
    add_prism(
      n = 3,
      label = c("one", "two",
                "three", "four",
                "five", "six"),
      type = c("a", "a",
               "a", "b",
               "b", "b"),
      value = c(1.2, 8.4,
                3.4, 5.2,
                6.1, 2.6),
      rel = c("prism_a", "prism_a",
              "prism_a", "prism_b",
              "prism_b", "prism_b",
              "prism_c", "prism_c",
              "prism_c"))

  # Get the graph's node data frame
  ndf <- get_node_df(graph_2)

  # Get the graph's edge data frame
  edf <- get_edge_df(graph_2)

  # Expect that 6 nodes are now in the graph
  expect_equal(
    node_count(graph_2), 6)

  # Expect 9 edges are now in the graph
  expect_equal(
    edge_count(graph_2), 9)

  # Expect node ID values from 1 to 6
  expect_identical(
    ndf$id, 1:6)

  # Expect specific node `label` values
  expect_identical(
    ndf$label,
    c("one", "two",
      "three", "four",
      "five", "six"))

  # Expect specific node `type` values
  expect_identical(
    ndf$type,
    c("a", "a", "a",
      "b", "b", "b"))

  # Expect specific node `value` values
  expect_identical(
    ndf$value,
    c(1.2, 8.4, 3.4, 5.2, 6.1, 2.6))

  # Expect specific edge `rel` values
  expect_identical(
    edf$rel,
    c("prism_a", "prism_a",
      "prism_a", "prism_b",
      "prism_b", "prism_b",
      "prism_c", "prism_c",
      "prism_c"))
})

test_that("Adding a star is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add a star
  graph <-
    add_star(
      graph = graph,
      n = 4,
      type = "a",
      rel = "z")

  # Expect 4 nodes to have been created
  expect_equal(
    node_count(graph), 4)

  # Expect 3 edges to have been created
  expect_equal(
    edge_count(graph), 3)

  # Expect node ID values from 1 to 4
  expect_identical(
    get_node_ids(graph), 1:4)

  # Expect label values from 1 to 4
  expect_identical(
    graph$nodes_df$label,
    as.character(1:4))

  # Expect type values to all be `a`
  expect_equal(
    unique(graph$nodes_df$type), "a")

  # Expect rel values to all be `z`
  expect_equal(
    unique(graph$edges_df$rel), "z")

  # Expect an error if n is <4
  expect_error(
    add_star(
      graph = graph,
      n = 3,
      type = "a",
      rel = "z"))

  # Add another star to the graph
  graph <-
    add_star(
      graph = graph,
      n = 4,
      type = "b",
      rel = "y")

  # Expect that 8 nodes are now in the graph
  expect_equal(
    node_count(graph), 8)

  # Expect 6 edges are now in the graph
  expect_equal(
    edge_count(graph), 6)

  # Expect node ID values from 1 to 8
  expect_identical(
    get_node_ids(graph), 1:8)

  # Expect label values from 1 to 8
  expect_identical(
    graph$nodes_df$label,
    as.character(1:8))

  # Expect type values to be either `a` or `b`
  expect_identical(
    unique(graph$nodes_df$type),
    c("a", "b"))

  # Expect rel values to be either `z` or `y`
  expect_identical(
    unique(graph$edges_df$rel),
    c("z", "y"))

  # Create a graph with a star that
  # has different types of node and edge
  # attributes included
  graph_2 <-
    create_graph() %>%
    add_star(
      n = 6,
      label = c("one", "two",
                "three", "four",
                "five", "six"),
      type = c("a", "a",
               "b", "b",
               "c", "c"),
      value = c(1.2, 8.4,
                3.4, 5.2,
                6.1, 2.6),
      rel = c("star_a", "star_b",
              "star_c", "star_d",
              "star_e"))

  # Get the graph's node data frame
  ndf <- get_node_df(graph_2)

  # Get the graph's edge data frame
  edf <- get_edge_df(graph_2)

  # Expect that 6 nodes are now in the graph
  expect_equal(
    node_count(graph_2), 6)

  # Expect 5 edges are now in the graph
  expect_equal(
    edge_count(graph_2), 5)

  # Expect node ID values from 1 to 6
  expect_identical(
    ndf$id, 1:6)

  # Expect specific node `label` values
  expect_identical(
    ndf$label,
    c("one", "two",
      "three", "four",
      "five", "six"))

  # Expect specific node `type` values
  expect_identical(
    ndf$type,
    c("a", "a", "b",
      "b", "c", "c"))

  # Expect specific node `value` values
  expect_identical(
    ndf$value,
    c(1.2, 8.4, 3.4, 5.2, 6.1, 2.6))

  # Expect specific edge `rel` values
  expect_identical(
    edf$rel,
    c("star_a", "star_b",
      "star_c", "star_d",
      "star_e"))
})

test_that("Adding a full graph is possible", {

  # Create a graph and add a full graph
  # with 3 nodes to it; keep loops
  graph <-
    create_graph() %>%
    add_full_graph(
      n = 3,
      keep_loops = TRUE)

  # Expect 3 nodes to have been created
  expect_equal(
    node_count(graph), 3)

  # Expect 9 edges to have been created
  expect_equal(
    edge_count(graph), 9)

  # Expect that there are 3 loops in the graph
  expect_equal(
    length(
      which(
        graph$edges_df$from ==
          graph$edges_df$to)), 3)

  # Expect node ID values from 1 to 3
  expect_identical(
    get_node_ids(graph), 1:3)

  # Expect type values to be character NA values
  expect_equal(
    unique(graph$nodes_df$type),
    as.character(NA))

  # Create a graph and add a full graph
  # with 3 nodes to it; discard loops
  graph <-
    create_graph() %>%
    add_full_graph(
      n = 3,
      keep_loops = FALSE)

  # Expect 3 nodes to have been created
  expect_equal(
    node_count(graph), 3)

  # Expect 6 edges to have been created
  expect_equal(
    edge_count(graph), 6)

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
  expect_equal(
    node_count(graph), 3)

  # Expect 6 edges to have been created
  expect_equal(
    edge_count(graph), 6)

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
    rnorm(n = 100, mean = 5, sd = 2) %>%
    sample(size = 9, replace = FALSE) %>%
    round(digits = 2) %>%
    matrix(
      ncol = 3,
      nrow = 3,
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
  expect_equal(
    node_count(graph), 3)

  # Expect 6 edges to have been created
  expect_equal(
    edge_count(graph), 6)

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
  expect_equal(
    node_count(graph), 3)

  # Expect 3 edges to have been created
  expect_equal(
    edge_count(graph), 3)

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
  expect_equal(
    node_count(graph), 3)

  # Expect 9 edges to have been created
  expect_equal(
    edge_count(graph), 9)

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
  expect_equal(
    node_count(graph), 3)

  # Expect 6 edges to have been created
  expect_equal(
    edge_count(graph), 6)

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
  expect_equal(
    node_count(graph), 3)

  # Expect 6 edges to have been created
  expect_equal(
    edge_count(graph), 6)

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
    add_cycle(
      n = 5,
      type = "cycle") %>%
    add_full_graph(
      n = 8,
      keep_loops = FALSE)

  # Expect 13 nodes to have been created
  expect_equal(
    node_count(graph), 13)

  # Expect 6 edges to have been created
  expect_equal(
    edge_count(graph), 61)
})
