# Add graphs to graphs

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
    count_nodes(graph = graph), 40)

  # Expect 39 edges to have been created
  expect_equal(
    count_edges(graph = graph), 39)

  # Expect node ID values from 1 to 40
  expect_identical(
    get_node_ids(graph), 1:40)

  # Expect label values from 1 to 40
  expect_identical(
    graph$nodes_df$label,
    as.character(1:40))

  # Expect type values to all be `a`
  expect_in(graph$nodes_df$type, "a")

  # Expect rel values to all be `z`
  expect_in(graph$edges_df$rel, "z")

  # Expect an error if h <2 or k <2
  expect_snapshot(error = TRUE, {
    # k < 2
    add_balanced_tree(
      graph = graph,
      k = 1,
      h = 3,
      type = "a",
      rel = "z")
    # h < 2
    add_balanced_tree(
      graph = graph,
      k = 3,
      h = 1,
      type = "a",
      rel = "z")
  }
  )

  # Add another balanced tree to the graph
  graph <-
    add_balanced_tree(
      graph = graph,
      k = 2,
      h = 2,
      type = "b",
      rel = "y")

  # Expect that 47 nodes are now in the graph
  expect_equal(count_nodes(graph = graph), 47)

  # Expect 45 edges are now in the graph
  expect_equal(count_edges(graph = graph), 45)

  # Expect node ID values from 1 to 47
  expect_identical(
    get_node_ids(graph), 1:47)

  # Expect label values from 1 to 47
  expect_equal(
    graph$nodes_df$label,
    as.character(1:47))

  # Expect type values to be either `a` or `b`
  expect_in(graph$nodes_df$type, c("a", "b"))

  # Expect rel values to be either `y` or `z`
  expect_in(graph$edges_df$rel, c("y", "z"))
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
    count_nodes(graph = graph), 3)

  # Expect 3 edges to have been created
  expect_equal(
    count_edges(graph = graph), 3)

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
    count_nodes(graph = graph), 6)

  # Expect 6 edges are now in the graph
  expect_equal(
    count_edges(graph = graph), 6)

  # Expect node ID values from 1 to 6
  expect_identical(
    get_node_ids(graph), 1:6)

  # Expect label values from 1 to 6
  expect_identical(
    graph$nodes_df$label,
    as.character(1:6))

  # Expect type values to be either `a` or `b`
  expect_in(graph$nodes_df$type, c("a", "b"))

  # Expect rel values to be either `y` or `z`
  expect_in(graph$edges_df$rel, c("y", "z"))

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
      rel = c("cycle_a", "cycle_a",
              "cycle_b", "cycle_b",
              "cycle_c", "cycle_c"))

  # Get the graph's node data frame
  ndf <- get_node_df(graph_2)

  # Get the graph's edge data frame
  edf <- get_edge_df(graph_2)

  # Expect that 6 nodes are now in the graph
  expect_equal(
    count_nodes(graph = graph_2), 6)

  # Expect 6 edges are now in the graph
  expect_equal(
    count_edges(graph = graph_2), 6)

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
    count_nodes(graph = graph), 3)

  # Expect 2 edges to have been created
  expect_equal(
    count_edges(graph = graph), 2)

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
  expect_snapshot(error = TRUE,
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
    count_nodes(graph = graph), 6)

  # Expect 6 edges are now in the graph
  expect_equal(
    count_edges(graph = graph), 4)

  # Expect node ID values from 1 to 6
  expect_identical(
    get_node_ids(graph), 1:6)

  # Expect label values from 1 to 6
  expect_identical(
    graph$nodes_df$label,
    as.character(1:6))

  # Expect type values to be either `a` or `b`
  expect_setequal(
    graph$nodes_df$type,
    c("a", "b"))

  # Expect rel values to be either `y` or `z`
  expect_setequal(
    graph$edges_df$rel,
    c("y", "z"))

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
      rel = c("path_a", "path_b",
              "path_c", "path_d",
              "path_e"))

  # Get the graph's node data frame
  ndf <- get_node_df(graph_2)

  # Get the graph's edge data frame
  edf <- get_edge_df(graph_2)

  # Expect that 6 nodes are now in the graph
  expect_equal(
    count_nodes(graph = graph_2), 6)

  # Expect 5 edges are now in the graph
  expect_equal(
    count_edges(graph = graph_2), 5)

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
    count_nodes(graph = graph), 6)

  # Expect 9 edges to have been created
  expect_equal(
    count_edges(graph = graph), 9)

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
    count_nodes(graph = graph), 12)

  # Expect 18 edges are now in the graph
  expect_equal(
    count_edges(graph = graph), 18)

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
    count_nodes(graph = graph_2), 6)

  # Expect 9 edges are now in the graph
  expect_equal(
    count_edges(graph = graph_2), 9)

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
    count_nodes(graph = graph), 4)

  # Expect 3 edges to have been created
  expect_equal(
    count_edges(graph = graph), 3)

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
    count_nodes(graph = graph), 8)

  # Expect 6 edges are now in the graph
  expect_equal(
    count_edges(graph = graph), 6)

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
      rel = c("star_a", "star_b",
              "star_c", "star_d",
              "star_e"))

  # Get the graph's node data frame
  ndf <- graph_2 %>% get_node_df()

  # Get the graph's edge data frame
  edf <- graph_2 %>% get_edge_df()

  # Expect that 6 nodes are now in the graph
  expect_equal(
    count_nodes(graph = graph_2), 6)

  # Expect 5 edges are now in the graph
  expect_equal(
    count_edges(graph = graph_2), 5)

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

  # Expect specific edge `rel` values
  expect_identical(
    edf$rel,
    c("star_a", "star_b",
      "star_c", "star_d",
      "star_e"))
})

test_that("Adding a 2D grid is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add a 2D grid
  graph <-
    add_grid_2d(
      graph = graph,
      x = 3,
      y = 3,
      type = "a",
      rel = "z")

  # Expect 9 nodes to have been created
  expect_equal(
    count_nodes(graph = graph), 9)

  # Expect 12 edges to have been created
  expect_equal(
    count_edges(graph = graph), 12)

  # Expect node ID values from 1 to 9
  expect_identical(
    get_node_ids(graph = graph), 1:9)

  # Expect label values from 1 to 9
  expect_identical(
    graph$nodes_df$label,
    as.character(1:9))

  # Expect type values to all be `a`
  expect_equal(
    unique(graph$nodes_df$type), "a")

  # Expect rel values to all be `z`
  expect_equal(
    unique(graph$edges_df$rel), "z")

  # Expect an error if x is <2
  expect_error(
    add_grid_2d(
      graph = graph,
      x = 1,
      y = 3,
      type = "a",
      rel = "z"))

  # Expect an error if y is <2
  expect_error(
    add_grid_2d(
      graph = graph,
      x = 3,
      y = 1,
      type = "a",
      rel = "z"))

  # Add another 2D grid to the graph
  graph <-
    add_grid_2d(
      graph = graph,
      x = 2,
      y = 2,
      type = "b",
      rel = "y")

  # Expect that 13 nodes are now in the graph
  expect_equal(
    count_nodes(graph), 13)

  # Expect 16 edges are now in the graph
  expect_equal(
    count_edges(graph), 16)

  # Expect label values from 1 to 13
  expect_identical(
    graph$nodes_df$label,
    as.character(1:13))

  # Expect type values to be either `a` or `b`
  expect_identical(
    unique(graph$nodes_df$type),
    c("a", "b"))

  # Expect rel values to be either `z` or `y`
  expect_identical(
    unique(graph$edges_df$rel),
    c("z", "y"))

  # Create a graph with a 2D grid that
  # has different types of node and edge
  # attributes included
  graph_2 <-
    create_graph() %>%
    add_grid_2d(
      x = 2,
      y = 2,
      label = c(
        "one", "two",
        "three", "four"),
      type = c(
        "a", "a",
        "b", "b"),
      rel = c(
        "grid_a", "grid_a",
        "grid_b", "grid_b"))

  # Get the graph's node data frame
  ndf <- get_node_df(graph_2)

  # Get the graph's edge data frame
  edf <- get_edge_df(graph_2)

  # Expect that 4 nodes are now in the graph
  expect_equal(
    count_nodes(graph_2), 4)

  # Expect 4 edges are now in the graph
  expect_equal(
    count_edges(graph_2), 4)

  # Expect node ID values from 1 to 6
  expect_identical(
    ndf$id, 1:4)

  # Expect specific node `label` values
  expect_identical(
    ndf$label,
    c("one", "two", "three", "four"))

  # Expect specific node `type` values
  expect_identical(
    ndf$type,
    c("a", "a", "b", "b"))

  # Expect specific edge `rel` values
  expect_identical(
    edf$rel,
    c("grid_a", "grid_a", "grid_b", "grid_b"))
})

test_that("Adding a 3D grid is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add a 3D grid
  graph <-
    add_grid_3d(
      graph = graph,
      x = 2,
      y = 2,
      z = 2,
      type = "a",
      rel = "z")

  # Expect 8 nodes to have been created
  expect_equal(
    count_nodes(graph), 8)

  # Expect 12 edges to have been created
  expect_equal(
    count_edges(graph), 12)

  # Expect node ID values from 1 to 8
  expect_identical(
    get_node_ids(graph), 1:8)

  # Expect label values from 1 to 8
  expect_identical(
    graph$nodes_df$label,
    as.character(1:8))

  # Expect type values to all be `a`
  expect_equal(
    unique(graph$nodes_df$type), "a")

  # Expect rel values to all be `z`
  expect_equal(
    unique(graph$edges_df$rel), "z")

  # Expect an error if x is <2
  expect_error(
    add_grid_3d(
      graph = graph,
      x = 1,
      y = 3,
      z = 3,
      type = "a",
      rel = "z"))

  # Expect an error if y is <2
  expect_error(
    add_grid_3d(
      graph = graph,
      x = 3,
      y = 1,
      z = 3,
      type = "a",
      rel = "z"))

  # Expect an error if z is <2
  expect_error(
    add_grid_3d(
      graph = graph,
      x = 3,
      y = 3,
      z = 1,
      type = "a",
      rel = "z"))

  # Add another 3D grid to the graph
  graph <-
    add_grid_3d(
      graph = graph,
      x = 2,
      y = 2,
      z = 2,
      type = "b",
      rel = "y")

  # Expect that 13 nodes are now in the graph
  expect_equal(
    count_nodes(graph), 16)

  # Expect 24 edges are now in the graph
  expect_equal(
    count_edges(graph), 24)

  # Expect label values from 1 to 16
  expect_identical(
    graph$nodes_df$label,
    as.character(1:16))

  # Expect type values to be either `a` or `b`
  expect_identical(
    unique(graph$nodes_df$type),
    c("a", "b"))

  # Expect rel values to be either `z` or `y`
  expect_identical(
    unique(graph$edges_df$rel),
    c("z", "y"))

  # Create a graph with a 2D grid that
  # has different types of node and edge
  # attributes included
  graph_2 <-
    create_graph() %>%
    add_grid_3d(
      x = 2,
      y = 2,
      z = 2,
      label = c(
        "one", "two",
        "three", "four",
        "five", "six",
        "seven", "eight"),
      type = c(
        "a", "a", "a", "a",
        "b", "b", "b", "b"),
      rel = c(
        "grid_a", "grid_a", "grid_a",
        "grid_a", "grid_a", "grid_a",
        "grid_b", "grid_b", "grid_b",
        "grid_b", "grid_b", "grid_b"))

  # Get the graph's node data frame
  ndf <- get_node_df(graph_2)

  # Get the graph's edge data frame
  edf <- get_edge_df(graph_2)

  # Expect that 8 nodes are now in the graph
  expect_equal(
    count_nodes(graph_2), 8)

  # Expect 12 edges are now in the graph
  expect_equal(
    count_edges(graph_2), 12)

  # Expect node ID values from 1 to 6
  expect_identical(
    ndf$id, 1:8)

  # Expect specific node `label` values
  expect_identical(
    ndf$label,
    c("one", "two", "three", "four",
      "five", "six", "seven", "eight"))

  # Expect specific node `type` values
  expect_identical(
    ndf$type,
    c("a", "a", "a", "a",
      "b", "b", "b", "b"))

  # Expect specific edge `rel` values
  expect_identical(
    edf$rel,
    c("grid_a", "grid_a", "grid_a",
      "grid_a", "grid_a", "grid_a",
      "grid_b", "grid_b", "grid_b",
      "grid_b", "grid_b", "grid_b"))
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
    count_nodes(graph = graph), 3)

  # Expect 9 edges to have been created
  expect_equal(
    count_edges(graph = graph), 9)

  # Expect that there are 3 loops in the graph
  expect_length(
    which(graph$edges_df$from == graph$edges_df$to),
    3)

  # Expect node ID values from 1 to 3
  expect_identical(
    get_node_ids(graph), 1:3)

  # Expect type values to be character NA values
  expect_equal(
    unique(graph$nodes_df$type),
    NA_character_)

  # Create a graph and add a full graph
  # with 3 nodes to it; discard loops
  graph <-
    create_graph() %>%
    add_full_graph(
      n = 3,
      keep_loops = FALSE)

  # Expect 3 nodes to have been created
  expect_equal(
    count_nodes(graph = graph), 3)

  # Expect 6 edges to have been created
  expect_equal(
    count_edges(graph = graph), 6)

  # Expect that there are no loops in the graph
  expect_length(
    which(graph$edges_df$from == graph$edges_df$to),
    0)

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
    count_nodes(graph = graph), 3)

  # Expect 6 edges to have been created
  expect_equal(
    count_edges(graph = graph), 6)

  # Expect that there are no loops in the graph
  expect_length(
    which(graph$edges_df$from == graph$edges_df$to),
    0)

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
  suppressWarnings(RNGversion("3.5.0"))
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
    count_nodes(graph = graph), 3)

  # Expect 6 edges to have been created
  expect_equal(
    count_edges(graph = graph), 6)

  # Expect that there are no loops in the graph
  expect_length(
    which(graph$edges_df$from == graph$edges_df$to),
    0)

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
    count_nodes(graph = graph), 3)

  # Expect 3 edges to have been created
  expect_equal(
    count_edges(graph = graph), 3)

  # Expect that there are no loops in the graph
  expect_length(
    which(graph$edges_df$from ==  graph$edges_df$to),
    0)

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
    count_nodes(graph = graph), 3)

  # Expect 9 edges to have been created
  expect_equal(
    count_edges(graph = graph), 9)

  # Expect that there are 3 loops in the graph
  expect_length(
    which(graph$edges_df$from == graph$edges_df$to),
    3)

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
    count_nodes(graph = graph), 3)

  # Expect 6 edges to have been created
  expect_equal(
    count_edges(graph = graph), 6)

  # Expect that there are 3 loops in the graph
  expect_length(
    which(graph$edges_df$from == graph$edges_df$to),
    3)

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
    count_nodes(graph = graph), 3)

  # Expect 6 edges to have been created
  expect_equal(
    count_edges(graph = graph), 6)

  # Expect that there are 3 loops in the graph
  expect_length(
    which(graph$edges_df$from == graph$edges_df$to),
    3)

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
    rep(NA_character_, 3))

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
    count_nodes(graph = graph), 13)

  # Expect 6 edges to have been created
  expect_equal(
    count_edges(graph = graph), 61)
})

test_that("Adding a G(n, m) Erdos-Renyi graph is possible", {

  # Create an undirected GNM
  # graph with 100 nodes and
  # 120 edges
  gnm_graph <-
    create_graph(
      directed = FALSE) %>%
    add_gnm_graph(
      n = 100,
      m = 120)

  # Expect 100 nodes in the graph
  expect_equal(
    gnm_graph %>%
      count_nodes(),
    100)

  # Expect 120 edges in the graph
  expect_equal(
    gnm_graph %>%
      count_edges(),
    120)

  # Create 2 graphs made with the same
  # `n`, `m`, and `set_seed` values
  graph_1 <-
    create_graph() %>%
    add_gnm_graph(
      n = 100,
      m = 120,
      set_seed = 23)

  graph_2 <-
    create_graph() %>%
    add_gnm_graph(
      n = 100,
      m = 120,
      set_seed = 23)

  # Expect that these graphs will
  # have the same node and edge definitions
  expect_identical(
    graph_1$nodes_df,
    graph_2$nodes_df)

  expect_identical(
    graph_1$edges_df,
    graph_2$edges_df)

  # Create a directed graph, add
  # a cycle of 100 nodes, and then
  # add a GNM graph with 100 nodes
  # and 120 edges
  gnm_graph_added <-
    create_graph() %>%
    add_cycle(
      n = 100,
      type = "cycle") %>%
    add_gnm_graph(
      n = 100,
      m = 120) %>%
    select_last_nodes_created() %>%
    set_node_attrs_ws(
      node_attr = type,
      value = "GNM") %>%
    clear_selection()

  # Expect that the first 100 nodes
  # belong to the `cycle` type
  expect_equal(
    gnm_graph_added$nodes_df$type[1:100] %>%
      unique(),
    "cycle")

  # Expect that the 200 nodes have a
  # node ID sequence from 1 to 200
  expect_identical(
    gnm_graph_added$nodes_df$id,
    1:200)

  # Expect an error if the value for
  # `n` is too small (< 1)
  expect_error(
    create_graph() %>%
    add_gnm_graph(n = 0, m = 5))
})

test_that("Adding a G(n, p) Erdos-Renyi graph is possible", {

  # Create an undirected GNM
  # graph with 100 nodes and
  # a probability of 0.05 for
  # creating edges
  gnp_graph <-
    create_graph(
      directed = FALSE) %>%
    add_gnp_graph(
      n = 100,
      p = 0.05,
      set_seed = 23)

  # Expect 100 nodes in the graph
  expect_equal(
    gnp_graph %>%
      count_nodes(),
    100)

  # Expect 233 edges in the graph
  expect_equal(
    gnp_graph %>%
      count_edges(),
    233)

  # Create 2 graphs made with the same
  # `n`, `p`, and `set_seed` values
  graph_1 <-
    create_graph() %>%
    add_gnp_graph(
      n = 100,
      p = 0.06,
      set_seed = 23)

  graph_2 <-
    create_graph() %>%
    add_gnp_graph(
      n = 100,
      p = 0.06,
      set_seed = 23)

  # Expect that these graphs will
  # have the same node and edge definitions
  expect_identical(
    graph_1$nodes_df,
    graph_2$nodes_df)

  expect_identical(
    graph_1$edges_df,
    graph_2$edges_df)

  # Create a directed graph, add
  # a cycle of 100 nodes, and then
  # add a GNP graph with 100 nodes
  gnp_graph_added <-
    create_graph() %>%
    add_cycle(
      n = 100,
      type = "cycle") %>%
    add_gnp_graph(
      n = 100,
      p = 0.06) %>%
    select_last_nodes_created() %>%
    set_node_attrs_ws(
      node_attr = type,
      value = "GNP") %>%
    clear_selection()

  # Expect that the first 100 nodes
  # belong to the `cycle` type
  expect_equal(
    gnp_graph_added$nodes_df$type[1:100] %>%
      unique(),
    "cycle")

  # Expect that the 200 nodes have a
  # node ID sequence from 1 to 200
  expect_identical(
    gnp_graph_added$nodes_df$id,
    1:200)

  # Expect an error if the value for
  # `n` is too small (< 1)
  expect_error(
    create_graph() %>%
      add_gnp_graph(n = 0, p = 0.05))
})

test_that("Adding a growing graph is possible", {

  # Create a random, growing
  # citation graph with 100
  # nodes, adding an edge after
  # each node addition
  growing_graph <-
    create_graph() %>%
    add_growing_graph(
      n = 100,
      m = 1,
      citation = TRUE,
      set_seed = 23)

  # Expect 100 nodes in the graph
  expect_equal(
    growing_graph %>%
      count_nodes(),
    100)

  # Expect 99 edges in the graph
  expect_equal(
    growing_graph %>%
      count_edges(),
    99)

  # Create 2 graphs made with the same
  # `n`, `m`, and `set_seed` values
  graph_1 <-
    create_graph() %>%
    add_growing_graph(
      n = 100,
      m = 1,
      citation = TRUE,
      set_seed = 23)

  graph_2 <-
    create_graph() %>%
    add_growing_graph(
      n = 100,
      m = 1,
      citation = TRUE,
      set_seed = 23)

  # Expect that these graphs will
  # have the same node and edge definitions
  expect_identical(
    graph_1$nodes_df,
    graph_2$nodes_df)

  expect_identical(
    graph_1$edges_df,
    graph_2$edges_df)

  # Create a directed graph, add
  # a cycle of 100 nodes, and then
  # add a growing graph with 100 nodes
  growing_graph_added <-
    create_graph() %>%
    add_cycle(
      n = 100,
      type = "cycle") %>%
    add_growing_graph(
      n = 100,
      m = 1) %>%
    select_last_nodes_created() %>%
    set_node_attrs_ws(
      node_attr = type,
      value = "growing") %>%
    clear_selection()

  # Expect that the first 100 nodes
  # belong to the `cycle` type
  expect_equal(
    growing_graph_added$nodes_df$type[1:100] %>%
      unique(),
    "cycle")

  # Expect that the 200 nodes have a
  # node ID sequence from 1 to 200
  expect_identical(
    growing_graph_added$nodes_df$id,
    1:200)

  # Expect an error if the value for
  # `n` is too small (< 1)
  expect_error(
    create_graph() %>%
      add_growing_graph(n = 0, m = 1))
})

test_that("Adding an islands graph is possible", {

  # Create an undirected islands
  # graph with standard parameters
  islands_graph <-
    create_graph(
      directed = FALSE) %>%
    add_islands_graph(
      n_islands = 4,
      island_size = 10,
      p = 0.5,
      edges_between = 1,
      set_seed = 23)

  # Expect 40 nodes in the graph
  expect_equal(
    islands_graph %>%
      count_nodes(),
    40)

  # Expect 98 edges in the graph
  expect_equal(
    islands_graph %>%
      count_edges(),
    98)

  # Create 2 graphs made with the same
  # parameters and `set_seed` values
  graph_1 <-
    create_graph() %>%
    add_islands_graph(
      n_islands = 5,
      island_size = 10,
      p = 0.5,
      edges_between = 1,
      set_seed = 23)

  graph_2 <-
    create_graph() %>%
    add_islands_graph(
      n_islands = 5,
      island_size = 10,
      p = 0.5,
      edges_between = 1,
      set_seed = 23)

  # Expect that these graphs will
  # have the same node and edge definitions
  expect_identical(
    graph_1$nodes_df,
    graph_2$nodes_df)

  expect_identical(
    graph_1$edges_df,
    graph_2$edges_df)

  # Create a directed graph, add
  # a cycle of 100 nodes, and then
  # add an islands graph with 100 nodes
  islands_graph_added <-
    create_graph() %>%
    add_cycle(
      n = 100,
      type = "cycle") %>%
    add_islands_graph(
      n_islands = 10,
      island_size = 10,
      p = 0.5,
      edges_between = 1) %>%
    select_last_nodes_created() %>%
    set_node_attrs_ws(
      node_attr = type,
      value = "islands") %>%
    clear_selection()

  # Expect that the first 100 nodes
  # belong to the `cycle` type
  expect_equal(
    islands_graph_added$nodes_df$type[1:100] %>%
      unique(),
    "cycle")

  # Expect that the 200 nodes have a
  # node ID sequence from 1 to 200
  expect_identical(
    islands_graph_added$nodes_df$id,
    1:200)
})

test_that("Adding a preferential attachment graph is possible", {

  # Create an undirected PA
  # graph with 100 nodes, creating
  # an edge for every node created
  pa_graph <-
    create_graph(
      directed = FALSE) %>%
    add_pa_graph(
      n = 100,
      m = 1)

  # Expect 100 nodes in the graph
  expect_equal(
    pa_graph %>%
      count_nodes(),
    100)

  # Expect 99 edges in the graph
  expect_equal(
    pa_graph %>%
      count_edges(),
    99)

  # Create 2 graphs made with the same
  # `n`, `m`, and `set_seed` values
  graph_1 <-
    create_graph() %>%
    add_pa_graph(
      n = 200,
      m = 1,
      set_seed = 23)

  graph_2 <-
    create_graph() %>%
    add_pa_graph(
      n = 200,
      p = 1,
      set_seed = 23)

  # Expect that these graphs will
  # have the same node and edge definitions
  expect_identical(
    graph_1$nodes_df,
    graph_2$nodes_df)

  expect_identical(
    graph_1$edges_df,
    graph_2$edges_df)

  # Create a directed graph, add
  # a cycle of 100 nodes, and then
  # add a PA graph with 100 nodes
  pa_graph_added <-
    create_graph() %>%
    add_cycle(
      n = 100,
      type = "cycle") %>%
    add_pa_graph(
      n = 100,
      m = 1) %>%
    select_last_nodes_created() %>%
    set_node_attrs_ws(
      node_attr = type,
      value = "PA") %>%
    clear_selection()

  # Expect that the first 100 nodes
  # belong to the `cycle` type
  expect_equal(
    pa_graph_added$nodes_df$type[1:100] %>%
      unique(),
    "cycle")

  # Expect that the 200 nodes have a
  # node ID sequence from 1 to 200
  expect_identical(
    pa_graph_added$nodes_df$id,
    1:200)


  expect_snapshot(error = TRUE, {
    # Expect an error if the value for
    # `n` is too small (< 1)
    create_graph() %>%
      add_pa_graph(n = 0, m = 1)

    # Expect an error if the value for
    # `algo` is not a valid value
    create_graph() %>%
      add_pa_graph(n = 0, m = 1, algo = "plumtree")
    })

})

test_that("Adding a small world graph is possible", {

  # Create an undirected small world
  # graph with standard parameters
  smallworld_graph <-
    create_graph(
      directed = FALSE) %>%
    add_smallworld_graph(
      dimension = 1,
      size = 50,
      neighborhood = 1,
      p = 0.05,
      set_seed = 23)

  # Expect 50 nodes in the graph
  expect_equal(
    smallworld_graph %>%
      count_nodes(),
    50)

  # Expect 50 edges in the graph
  expect_equal(
    smallworld_graph %>%
      count_edges(),
    50)

  # Create 2 graphs made with the same
  # parameters and `set_seed` values
  graph_1 <-
    create_graph() %>%
    add_smallworld_graph(
      dimension = 1,
      size = 50,
      neighborhood = 1,
      p = 0.05,
      set_seed = 23)

  graph_2 <-
    create_graph() %>%
    add_smallworld_graph(
      dimension = 1,
      size = 50,
      neighborhood = 1,
      p = 0.05,
      set_seed = 23)

  # Expect that these graphs will
  # have the same node and edge definitions
  expect_identical(
    graph_1$nodes_df,
    graph_2$nodes_df)

  expect_identical(
    graph_1$edges_df,
    graph_2$edges_df)

  # Create a directed graph, add
  # a cycle of 100 nodes, and then
  # add a small world graph
  smallworld_graph_added <-
    create_graph() %>%
    add_cycle(
      n = 100,
      type = "cycle") %>%
    add_smallworld_graph(
      dimension = 1,
      size = 50,
      neighborhood = 1,
      p = 0.05) %>%
    select_last_nodes_created() %>%
    set_node_attrs_ws(
      node_attr = type,
      value = "smallworld") %>%
    clear_selection()

  # Expect that the first 100 nodes
  # belong to the `cycle` type
  expect_equal(
    smallworld_graph_added$nodes_df$type[1:100] %>%
      unique(),
    "cycle")

  # Expect that the 150 nodes have a
  # node ID sequence from 1 to 150
  expect_identical(
    smallworld_graph_added$nodes_df$id,
    1:150)
})
