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

  # Expect that a selection object is available
  expect_true(!is.null(graph_a_c$selection))

  # Expect that a list `nodes` is available
  # in `selection`
  expect_true(!is.null(graph_a_c$selection$nodes))

  # Expect that a list `edges` is not available
  # in `selection`
  expect_null(graph_a_c$selection$edges)

  # Expect that nodes `1` and `3` are part of a selection
  # object in `nodes`
  expect_true(
    all(
      graph_a_c$selection$nodes == c(1, 3)))

  # Select nodes where `value` > 3
  graph_val_gt_3 <-
    select_nodes(
      graph = graph,
      node_attr = "value",
      search = ">3")

  # Expect that nodes `1` and `3` are part of a
  # selection object in `nodes`
  expect_true(
    all(
      graph_val_gt_3$selection$nodes == c(1, 3)))

  # Select nodes where `value` < 3
  graph_val_lt_3 <-
    select_nodes(
      graph = graph,
      node_attr = "value",
      search = "<3")

  # Expect that nodes `2` and `4` are part of a
  # selection object in `nodes`
  expect_true(
    all(graph_val_lt_3$selection$nodes == c(2, 4)))

  # Select nodes where `value` == 2.7
  graph_val_eq_2_7 <-
    select_nodes(
      graph = graph,
      node_attr = "value",
      search = "==2.7")

  # Expect that node `4` is part of a selection
  # object in `nodes`
  expect_true(
    all(graph_val_eq_2_7$selection$nodes == 4))

  # Select nodes where `value` != 2.7
  graph_val_neq_2_7 <-
    select_nodes(
      graph = graph,
      node_attr = "value",
      search = "!=2.7")

  # Expect that nodes `1`, `2`, and `3` are part of
  # a selection object in `nodes`
  expect_true(
    all(
      graph_val_neq_2_7$selection$nodes == c(1, 2, 3)))

  # Select nodes where `type` is `letter`
  graph_val_letter <-
    select_nodes(
      graph = graph,
      node_attr = "type",
      search = "let")

  # Expect that nodes `1`, `2`, `3`, and `4` are
  # part of a selection object in `nodes`
  expect_true(
    all(
      graph_val_letter$selection$nodes ==
        c(1, 2, 3, 4)))

  # Select nodes where `type` is `letter` and filter
  # to nodes `1` and `2`
  graph_val_letter_a_b <-
    select_nodes(
      graph = graph,
      nodes = 1:2,
      node_attr = "type",
      search = "let")

  # Expect that nodes `1` and `2` are part of a
  # selection object in `nodes`
  expect_true(
    all(
      graph_val_letter_a_b$selection$nodes == c(1, 2)))

  # Create a union of selections in a magrittr pipeline
  graph_sel_union_a_b_c_d <-
    graph %>% select_nodes(nodes = c(1, 2)) %>%
    select_nodes(nodes = c(3, 4))

  # Expect that all 4 nodes are part of a selection
  # object in `nodes`
  expect_true(
    all(
      graph_sel_union_a_b_c_d$selection$nodes ==
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
      graph_sel_intersect$selection$nodes == c(2, 3)))

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
      graph_sel_difference$selection$nodes == 1))

  # Expect an error if selecting nodes from
  # an empty graph
  expect_error(
    select_nodes(
      graph = create_graph(),
      nodes = 3))

  # Expect an error if specifying more than
  # one attribute
  expect_error(
    select_nodes(
      graph = graph,
      node_attr = c("label", "value"),
      search = 1))

  # Expect an error if specifying an attribute that
  # doesn't exist
  expect_error(
    select_nodes(
      graph = graph,
      node_attr = "fontname",
      search = 1))

  # Expect an error if specifying a node that
  # doesn't exist
  expect_error(
    select_nodes(
      graph = graph,
      nodes = 5))

  expect_error(
    select_nodes(
      graph = graph,
      nodes = 5,
      node_attr = "value",
      search = ">0"))

  # Select the last node in a graph
  graph_last_node <- select_last_node(graph)

  # Expect that the node selected is the last
  # in the ndf
  expect_true(
    get_node_ids(graph_last_node)[length(get_node_ids(graph_last_node))] ==
      get_selection(graph_last_node))

  # Expect an error if trying to select the last node
  # from an empty graph
  expect_error(
    select_last_node(create_graph()))

  # Select the last edge in a graph
  graph_last_edge <- select_last_edge(graph)

  # Expect that the edge selected is the last
  # in the edf
  expect_true(
    get_edges(graph_last_edge, return_type = "vector")[
      length(get_edges(graph_last_edge, return_type = "vector"))] ==
      get_selection(graph_last_edge))

  # Expect an error if trying to select the last
  # edge from an empty graph
  expect_error(
    select_last_edge(create_graph()))

  # Expect an error if trying to select the last
  # edge from a graph with no edges
  expect_error(
    select_last_edge(
      create_graph(nodes_df = create_node_df(1))))
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

  # Select nodes `1` -> `4` and `2` -> `3`
  graph_ad_bc <-
    select_edges(
      graph = graph,
      from = c(1, 2),
      to = c(4, 3))

  # Expect that a selection object is available
  expect_true(!is.null(graph_ad_bc$selection))

  # Expect that a list `edges` is available
  # in `selection`
  expect_true(!is.null(graph_ad_bc$selection$edges))

  # Expect that a list `from` is available in
  # `selection/edges`
  expect_true(!is.null(graph_ad_bc$selection$edges$from))

  # Expect that a list `to` is available in
  # `selection/edges`
  expect_true(!is.null(graph_ad_bc$selection$edges$to))

  # Expect that a list `nodes` is not available
  # in `selection`
  expect_null(graph_ad_bc$selection$nodes)

  # Expect that nodes `1` and `2` are part of a
  # selection object in `from`
  expect_true(
    all(graph_ad_bc$selection$edges$from == c(1, 2)))

  # Expect that nodes `4` and `3` are part of a
  # selection object in `to`
  expect_true(
    all(graph_ad_bc$selection$edges$to == c(4, 3)))

  # Select edges where `width` > 2
  graph_width_gt_2 <-
    select_edges(
      graph = graph,
      edge_attr = "width",
      search = ">2")

  # Expect that node `3` is part of a selection
  # object in 'edges/from'
  expect_true(graph_width_gt_2$selection$edges$from == 3)

  # Expect that node `1` is part of a selection
  # object in 'edges/to'
  expect_true(graph_width_gt_2$selection$edges$to == 1)

  # Select nodes where `width` < 3
  graph_width_lt_3 <-
    select_edges(
      graph = graph,
      edge_attr = "width",
      search = "<3")

  # Expect that nodes `1` and `2` are part of a
  # selection object in 'edges/from'
  expect_true(
    all(
      graph_width_lt_3$selection$edges$from == c(1, 2)))

  # Expect that nodes `4` and `3` are part of a
  # selection object in 'edges/to'
  expect_true(
    all(
      graph_width_lt_3$selection$edges$to == c(4, 3)))

  # Select nodes where `width` == 2
  graph_width_eq_2 <-
    select_edges(
      graph = graph,
      edge_attr = "width",
      search = "== 2")

  # Expect that node `2` is part of a selection
  # object in 'edges/from'
  expect_true(
    graph_width_eq_2$selection$edges$from == 2)

  # Expect that node `3` is part of a selection
  # object in 'edges/to'
  expect_true(
    graph_width_eq_2$selection$edges$to == 3)

  # Select nodes where `width` != 2
  graph_width_neq_2 <-
    select_edges(
      graph = graph,
      edge_attr = "width",
      search = "!=2")

  # Expect that nodes `1` and `3` are part of a
  # selection object in 'edges/from'
  expect_true(all(graph_width_neq_2$selection$edges$from == c(1, 3)))

  # Expect that nodes `4` and `1` are part of a
  # selection object in 'edges/to'
  expect_true(all(graph_width_neq_2$selection$edges$to == c(4, 1)))

  # Select nodes where `rel` is `leading_to`
  graph_val_leading_to <-
    select_edges(
      graph = graph,
      edge_attr = "rel",
      search = "leading")

  # Expect that nodes `1`, `2`, and `3` are part of a
  # selection object in 'edges/from'
  expect_true(
    all(
      graph_val_leading_to$selection$edges$from == c(1, 2, 3)))

  # Expect that nodes `4`, `3`, and `1` are part of a
  # selection object in 'edges/to'
  expect_true(
    all(
      graph_val_leading_to$selection$edges$to == c(4, 3, 1)))

  # Create a union of selections in a magrittr pipeline
  graph_sel_union_ab_bc <-
    graph %>%
    select_edges(from = 1, to = 4) %>%
    select_edges(from = 2, to = 3)

  # Expect that nodes `1` and `2` are part of a
  # selection object in `edges/from`
  expect_true(all(graph_sel_union_ab_bc$selection$edges$from == c(1, 2)))

  # Expect that nodes `4` and `3` are part of a
  # selection object in `edges/to`
  expect_true(all(graph_sel_union_ab_bc$selection$edges$to == c(4, 3)))

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
  expect_true(graph_sel_intersect_bc$selection$edges$from == 2)

  # Expect that node `3` is part of a selection
  # object in `edges/to`
  expect_true(graph_sel_intersect_bc$selection$edges$to == 3)

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
  expect_true(graph_sel_edge_difference_ad$selection$edges$from == 1)

  # Expect that node `4` is part of a selection
  # object in `edges/to`
  expect_true(graph_sel_edge_difference_ad$selection$edges$to == 4)

  # Select edges, specifying only the `from` node
  graph_from_a <-
    select_edges(
      graph,
      from = 1)

  # Expect that only the edge `1` -> `4` is selected
  expect_true(graph_from_a$selection$edges$from == 1)
  expect_true(graph_from_a$selection$edges$to == 4)

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

  # Expect that only the edge `2` -> `3` is selected
  expect_true(graph_to_c$selection$edges$from == 2)
  expect_true(graph_to_c$selection$edges$to == 3)

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

  # Expect an error if specifying more than
  # one attribute
  expect_error(
    select_edges(
      graph = graph,
      edge_attr = c("rel", "width"),
      search = 1))

  # Expect an error if specifying an attribute
  # that doesn't exist
  expect_error(
    select_edges(
      graph = graph,
      edge_attr = "fontname",
      search = "a"))
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
  expect_true(all(graph$selection$edges$from %in%
                    c(3, 4, 2)))
  expect_true(all(graph$selection$edges$to %in%
                    c(4, 5, 3)))
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
    all(graph_sel_1_dist_2$selection$nodes ==
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
    all(graph_sel_1_sel_4_dist_2$selection$nodes ==
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
    all(graph_sel_1_sel_4_dist_2_sel_A_dist_3$selection$nodes ==
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
    all(graph_sel_1_sel_4_dist_2_sel_A_dist_3$selection$nodes ==
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
          c("1 -> 2", "2 -> 3", "3 -> 4", "4 -> 5",
            "5 -> 6", "6 -> 7", "7 -> 8", "8 -> 9",
            "9 -> 10", "10 -> 11", "11 -> 12")))
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
    all(graph_select_1_2_inverted$selection$nodes %in%
          3:12))

  # Select edges `1` -> `2` and `2` -> `3` in the graph
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
      graph_select_edges_1_2__2_3_inverted$selection$edges$from %in%
        c(3, 4, 5, 6, 7, 8, 9, 10, 11)))

  expect_true(
    all(
      graph_select_edges_1_2__2_3_inverted$selection$edges$to %in%
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
    select_nodes

  # Get the selection and expect both nodes to
  # be present
  expect_true(
    all(c(1, 2) %in%
          get_selection(graph_select_all_nodes)))

  # Clear the selection
  graph_select_all_nodes_cleared <-
    clear_selection(graph_select_all_nodes)

  # Expect no selection to be present in the graph
  expect_null(graph_select_all_nodes_cleared$selection)

  # Expect an `NA` value returned when getting a
  # selection that is not present
  expect_true(
    is.na(
      get_selection(graph_select_all_nodes_cleared)))
})
