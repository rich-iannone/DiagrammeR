context("Selecting nodes or edges in a graph object")

test_that("selecting a node in a graph is possible", {

  library(magrittr)

  # Create a simple graph
  nodes <-
    create_node_df(
      n = 4,
      type = "letter",
      label = TRUE,
      value = c(3.5, 2.6, 9.4, 2.7))

  edges <-
    create_edge_df(
      from = c(1, 2, 3),
      to = c(4, 3, 1),
      rel = "leading_to",
      width = c(1, 2, 3))

  graph <-
    create_graph(
      nodes_df = nodes,
      edges_df = edges)

  # Select nodes `1` and `3`
  graph_a_c <-
    select_nodes(graph = graph, nodes = c(1, 3))

  # Expect that a node selection is available
  expect_true(nrow(graph_a_c$node_selection) > 0)

  # Expect that an edge selection is not available
  expect_true(nrow(graph_a_c$edge_selection) == 0)

  # Expect that nodes `1` and `3` are part of a selection
  # object in `nodes`
  expect_true(
    all(
      graph_a_c$node_selection$node == c(1, 3)))

  # Select nodes where `value` > 3
  graph_val_gt_3 <-
    graph %>%
    select_nodes("value > 3")

  # Expect that nodes `1` and `3` are part of a
  # selection object in `nodes`
  expect_true(
    all(
      graph_val_gt_3$node_selection$node == c(1, 3)))

  # Select nodes where `value` < 3
  graph_val_lt_3 <-
    graph %>%
    select_nodes("value < 3")

  # Expect that nodes `2` and `4` are part of a
  # selection object in `nodes`
  expect_true(
    all(
      graph_val_lt_3$node_selection$node == c(2, 4)))

  # Select nodes where `value` == 2.7
  graph_val_eq_2_7 <-
    graph %>%
    select_nodes("value == 2.7")

  # Expect that node `4` is part of a selection
  # object in `nodes`
  expect_true(
    all(graph_val_eq_2_7$node_selection$node == 4))

  # Select nodes where `value` != 2.7
  graph_val_neq_2_7 <-
    graph %>%
    select_nodes("value != 2.7")

  # Expect that nodes `1`, `2`, and `3` are part of
  # a selection object in `nodes`
  expect_true(
    all(
      graph_val_neq_2_7$node_selection$node == c(1, 2, 3)))

  # Select nodes where `type` is `letter`
  graph_val_letter <-
    graph %>%
    select_nodes("type == 'let'")

  # Expect that nodes `1`, `2`, `3`, and `4` are
  # part of a selection object in `nodes`
  expect_true(
    all(
      graph_val_letter$node_selection$node ==
        c(1, 2, 3, 4)))

  # Select nodes where `type` is `letter` and filter
  # to nodes `1` and `2`
  graph_val_letter_a_b <-
    graph %>%
    select_nodes("type == 'let'", nodes = 1:2)

  # Expect that nodes `1` and `2` are part of a
  # selection object in `nodes`
  expect_true(
    all(
      graph_val_letter_a_b$node_selection$node == c(1, 2)))

  # Create a union of selections in a magrittr pipeline
  graph_sel_union_a_b_c_d <-
    graph %>%
    select_nodes(nodes = c(1, 2)) %>%
    select_nodes(nodes = c(3, 4))

  # Expect that all 4 nodes are part of a selection
  # object in `nodes`
  expect_true(
    all(
      graph_sel_union_a_b_c_d$node_selection$node ==
        c(1, 2, 3, 4)))

  # Create a intersection of selections in a
  # magrittr pipeline
  graph_sel_intersect <-
    graph %>%
    select_nodes(nodes = c(1, 2, 3)) %>%
    select_nodes(
      nodes = c(2, 3, 4), set_op = "intersect")

  # Expect that nodes `2` and `3` are part of a
  # selection object in `nodes`
  expect_true(
    all(
      graph_sel_intersect$node_selection$node == c(2, 3)))

  # Create a selection that is a difference
  # of selections
  graph_sel_difference <-
    graph %>%
    select_nodes(nodes = 1:3) %>%
    select_nodes(
      nodes = 2:3, set_op = "difference")

  # Expect that node `1` is part of a selection
  # object in `nodes`
  expect_true(
    all(
      graph_sel_difference$node_selection$node == 1))

  # Expect an error if selecting nodes from
  # an empty graph
  expect_error(
    select_nodes(
      graph = create_graph(),
      nodes = 3))

  # Expect an error if specifying a node that
  # doesn't exist
  expect_error(
    select_nodes(
      graph = graph,
      nodes = 5))
})

test_that("selecting an edge in a graph is possible", {

  # Create a simple graph
  nodes <-
    create_node_df(
      n = 4,
      type = "letter",
      label = TRUE,
      value = c(3.5, 2.6, 9.4, 2.7))

  edges <-
    create_edge_df(
      from = c(1, 2, 3),
      to = c(4, 3, 1),
      rel = "leading_to",
      width = c(1, 2, 3))

  graph <-
    create_graph(
      nodes_df = nodes,
      edges_df = edges)

  # Select nodes `1`->`4` and `2`->`3`
  graph_ad_bc <-
    select_edges(
      graph = graph,
      from = c(1, 2),
      to = c(4, 3))

  # Expect that nodes `1` and `2` are part of a
  # selection object in `from`
  expect_true(
    all(graph_ad_bc$edge_selection$from == c(1, 2)))

  # Expect that nodes `4` and `3` are part of a
  # selection object in `to`
  expect_true(
    all(graph_ad_bc$edge_selection$to == c(4, 3)))

  # Select edges where `width` > 2
  graph_width_gt_2 <-
    graph %>%
    select_edges("width > 2")

  # Expect that node `3` is part of a selection
  # object in 'edges/from'
  expect_true(graph_width_gt_2$edge_selection$from == 3)

  # Expect that node `1` is part of a selection
  # object in 'edges/to'
  expect_true(graph_width_gt_2$edge_selection$to == 1)

  # Select nodes where `width` < 3
  graph_width_lt_3 <-
    graph %>%
    select_edges("width < 3")

  # Expect that nodes `1` and `2` are part of a
  # selection object in 'edges/from'
  expect_true(
    all(
      graph_width_lt_3$edge_selection$from == c(1, 2)))

  # Expect that nodes `4` and `3` are part of a
  # selection object in 'edges/to'
  expect_true(
    all(
      graph_width_lt_3$edge_selection$to == c(4, 3)))

  # Select nodes where `width` == 2
  graph_width_eq_2 <-
    graph %>%
    select_edges("width == 2")

  # Expect that node `2` is part of a selection
  # object in 'edges/from'
  expect_true(
    graph_width_eq_2$edge_selection$from == 2)

  # Expect that node `3` is part of a selection
  # object in 'edges/to'
  expect_true(
    graph_width_eq_2$edge_selection$to == 3)

  # Select nodes where `width` != 2
  graph_width_neq_2 <-
    graph %>%
    select_edges("width != 2")

  # Expect that nodes `1` and `3` are part of a
  # selection object in 'edges/from'
  expect_true(all(graph_width_neq_2$edge_selection$from == c(1, 3)))

  # Expect that nodes `4` and `1` are part of a
  # selection object in 'edges/to'
  expect_true(all(graph_width_neq_2$edge_selection$to == c(4, 1)))

  # Select nodes where `rel` is `leading_to`
  graph_val_leading_to <-
    graph %>%
    select_edges("rel == 'leading_to'")

  # Expect that nodes `1`, `2`, and `3` are part of a
  # selection object in 'edges/from'
  expect_true(
    all(
      graph_val_leading_to$edge_selection$from == c(1, 2, 3)))

  # Expect that nodes `4`, `3`, and `1` are part of a
  # selection object in 'edges/to'
  expect_true(
    all(
      graph_val_leading_to$edge_selection$to == c(4, 3, 1)))

  # Create a union of selections in a magrittr pipeline
  graph_sel_union_ab_bc <-
    graph %>%
    select_edges(from = 1, to = 4) %>%
    select_edges(from = 2, to = 3)

  # Expect that nodes `1` and `2` are part of a
  # selection object in `edges/from`
  expect_true(all(graph_sel_union_ab_bc$edge_selection$from == c(1, 2)))

  # Expect that nodes `4` and `3` are part of a
  # selection object in `edges/to`
  expect_true(all(graph_sel_union_ab_bc$edge_selection$to == c(4, 3)))

  # Create a intersection of selections in a
  # magrittr pipeline
  graph_sel_intersect_bc <-
    graph %>%
    select_edges(from = c(1, 2), to = c(4, 3)) %>%
    select_edges(
      from = c(2, 3), to = c(3, 1),
      set_op = "intersect")

  # Expect that node `2` is part of a selection
  # object in `edges/from`
  expect_true(graph_sel_intersect_bc$edge_selection$from == 2)

  # Expect that node `3` is part of a selection
  # object in `edges/to`
  expect_true(graph_sel_intersect_bc$edge_selection$to == 3)

  # Create a selection that is a difference
  # of selections
  graph_sel_edge_difference_ad <-
    graph %>%
    select_edges(
      from = c(1, 2, 3), to = c(4, 3, 1)) %>%
    select_edges(
      from = c(2, 3), to = c(3, 1),
      set_op = "difference")

  # Expect that node `1` is part of a selection
  # object in `edges/from`
  expect_true(graph_sel_edge_difference_ad$edge_selection$from == 1)

  # Expect that node `4` is part of a selection
  # object in `edges/to`
  expect_true(graph_sel_edge_difference_ad$edge_selection$to == 4)

  # Select edges, specifying only the `from` node
  graph_from_a <-
    select_edges(
      graph,
      from = 1)

  # Expect that only the edge `1`->`4` is selected
  expect_true(graph_from_a$edge_selection$from == 1)
  expect_true(graph_from_a$edge_selection$to == 4)

  # Expect an error if when only specifying the
  # `from` node, that node isn't present in the graph
  expect_error(
    select_edges(
      graph,
      from = 7))

  # Select edges, specifying only the `to` node
  graph_to_c <-
    select_edges(
      graph,
      to = 3)

  # Expect that only the edge `2`->`3` is selected
  expect_true(graph_to_c$edge_selection$from == 2)
  expect_true(graph_to_c$edge_selection$to == 3)

  # Expect an error if when only specifying the
  # `to` node, that node isn't present in the graph
  expect_error(
    select_edges(
      graph,
      to = 7))

  # Expect an error if selecting edges from an
  # empty graph
  expect_error(
    select_edges(
      graph = create_graph(),
      from = 1,
      to = 3))

  # Expect an error if selecting edges from a graph
  # with nodes but no edges
  expect_error(
    select_edges(
      graph = create_graph(
        create_node_df(1))))

  # Expect an error if selecting edges from a
  # graph that doesn't contain edges
  expect_error(
    create_graph() %>% add_n_nodes(2) %>% select_edges())
})

test_that("selecting edges via node IDs is possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_path(8)

  # Select all edges associated with nodes with
  # ID `3` and `4`
  graph <-
    select_edges_by_node_id(graph, nodes = c(3, 4))

  # Expect that certain edges with be available
  # in the selection
  expect_true(all(graph$edge_selection$from %in%
                    c(3, 4, 2)))
  expect_true(all(graph$edge_selection$to %in%
                    c(4, 5, 3)))
})

test_that("selecting edges via edge IDs is possible", {

  # Create a graph with 5 nodes
  graph <-
    create_graph() %>%
    add_path(5)

  # Create a graph selection by selecting edges
  # associated the edge IDs `1` and `2`
  graph <-
    graph %>%
    select_edges_by_edge_id(1:2)

  # Expect edges with IDs `1` and `2` in the
  # selection of edges
  expect_equal(
    graph %>%
      get_selection(), c(1, 2))
})

test_that("selecting nodes in a neighborhood is possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_path(12)

  # Create a selection of nodes centered around
  # node `6` and including those nodes a depth of 2
  # edges away
  graph_sel_1_dist_2 <-
    select_nodes_in_neighborhood(
      graph = graph,
      node = 6,
      distance = 2)

  # Expect that specific nodes are part of a selection
  # object in `nodes`
  expect_true(
    all(graph_sel_1_dist_2$node_selection$node ==
          c(6, 7, 5, 8, 4)))

  # Create a selection of nodes centered around node
  # `4` and including those nodes a distance of 4
  # nodes away
  graph_sel_1_sel_4_dist_2 <-
    select_nodes_in_neighborhood(
      graph = graph_sel_1_dist_2,
      node = 4,
      distance = 2)

  # Expect that specific nodes are part of a selection
  # object in `nodes`
  expect_true(
    all(graph_sel_1_sel_4_dist_2$node_selection$node ==
          c(6, 7, 5, 8, 4, 3, 2)))

  # Create a selection of nodes centered around node
  # `1` and including those nodes a distance of
  # 3 nodes away
  graph_sel_1_sel_4_dist_2_sel_A_dist_3 <-
    select_nodes_in_neighborhood(
      graph = graph_sel_1_sel_4_dist_2,
      node = 1,
      distance = 3,
      set_op = "intersect")

  # Expect that specific nodes are part of a selection
  # object in `nodes`
  expect_true(
    all(graph_sel_1_sel_4_dist_2_sel_A_dist_3$node_selection$node ==
          c(4, 3, 2)))

  # Create a selection of nodes centered around node
  # `7` and including those nodes a distance of 2
  # nodes away
  graph_sel_1_sel_4_dist_2_sel_A_dist_3 <-
    select_nodes_in_neighborhood(
      graph = graph_sel_1_sel_4_dist_2,
      node = 7,
      distance = 2,
      set_op = "difference")

  # Expect that specific nodes are part of a selection
  # object in `nodes`
  expect_true(
    all(graph_sel_1_sel_4_dist_2_sel_A_dist_3$node_selection$node ==
          c(4, 3, 2)))
})

test_that("getting a selection is possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_path(12)

  # Select all nodes in graph and get selection
  graph_node_selection_1 <-
    graph %>%
    select_nodes() %>%
    get_selection()

  # Expect that specific nodes are returned
  expect_true(
    all(graph_node_selection_1 ==
          c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)))

  # Select all edges in graph and get selection
  graph_edge_selection_1 <-
    graph %>%
    select_edges() %>%
    get_selection()

  # Expect that specific nodes are returned
  expect_true(
    all(graph_edge_selection_1 ==
          c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)))
})

test_that("inverting a selection is possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_path(12)

  # Select nodes `1` and `2` in the graph
  graph_select_1_2 <-
    graph %>%
    select_nodes(nodes = 1) %>%
    select_nodes(nodes = 2)

  # Invert the selection so that every other node
  # is selected
  graph_select_1_2_inverted <-
    graph_select_1_2 %>%
    invert_selection()

  # Expect that all nodes except `1` and `2` are
  # in the selection
  expect_true(
    all(graph_select_1_2_inverted$node_selection$node %in%
          3:12))

  # Select edges `1`->`2` and `2`->`3` in the graph
  graph_select_edges_1_2__2_3 <-
    graph %>%
    select_edges(from = 1, to = 2) %>%
    select_edges(from = 2, to = 3)

  # Invert the selection so that every other edge
  # is selected
  graph_select_edges_1_2__2_3_inverted <-
    graph_select_edges_1_2__2_3 %>%
    invert_selection()

  # Expect that every other edge is now in
  # the selection
  expect_true(
    all(
      graph_select_edges_1_2__2_3_inverted$edge_selection$from %in%
        c(3, 4, 5, 6, 7, 8, 9, 10, 11)))

  expect_true(
    all(
      graph_select_edges_1_2__2_3_inverted$edge_selection$to %in%
        c(4, 5, 6, 7, 8, 9, 10, 11, 12)))

  # Expect an error if inverting selection that
  # doesn't exist
  expect_error(invert_selection(graph))
})

test_that("getting/clearing a selection is possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node %>%
    add_node %>%
    add_edge(1, 2)

  # Select all nodes in the graph
  graph_select_all_nodes <-
    graph %>%
    select_nodes()

  # Get the selection and expect both nodes to
  # be present
  expect_true(
    all(c(1, 2) %in%
          get_selection(graph_select_all_nodes)))

  # Clear the selection
  graph_select_all_nodes_cleared <-
    clear_selection(graph_select_all_nodes)

  # Expect no selection to be present in the graph
  expect_true(nrow(graph_select_all_nodes_cleared$node_selection) == 0)

  # Expect an `NA` value returned when getting a
  # selection that is not present
  expect_true(
    is.na(
      get_selection(graph_select_all_nodes_cleared)))
})

test_that("selecting nodes by node degree is possible", {

  # Create a graph
  graph <-
    create_random_graph(
      n = 35, m = 125,
      set_seed = 23)

  # Expect specific nodes when making a selection
  # of nodes which have a total degree (indegree
  # + outdegree) of exactly 9
  expect_equal(
    graph %>%
      select_nodes_by_degree("deg == 9") %>%
      get_selection(),
    c(2, 9, 10, 14, 17, 19, 31, 33))

  # Expect specific nodes when making a selection
  # of nodes which have a total degree greater
  # than or equal to 9
  expect_equal(
    graph %>%
      select_nodes_by_degree("deg >= 9") %>%
      get_selection(),
    c(2, 6, 9, 10, 14, 17, 19, 22, 25, 29, 31, 33))
})
