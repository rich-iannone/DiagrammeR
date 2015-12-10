context("Selecting nodes or edges in a graph object")

test_that("selecting a node in a graph is possible", {

  library(magrittr)

  # Create a simple graph
  nodes <-
    create_nodes(nodes = c("a", "b", "c", "d"),
                 type = "letter",
                 label = TRUE,
                 value = c(3.5, 2.6, 9.4, 2.7))

  edges <-
    create_edges(from = c("a", "b", "c"),
                 to = c("d", "c", "a"),
                 rel = "leading_to",
                 width = c(1, 2, 3))

  graph <-
    create_graph(nodes_df = nodes,
                 edges_df = edges)

  # Select nodes "a" and "c"
  graph_a_c <- select_nodes(graph = graph, nodes = c("a", "c"))

  # Expect that a selection object is available
  expect_true(!is.null(graph_a_c$selection))

  # Expect that a list 'nodes' is available in 'selection'
  expect_true(!is.null(graph_a_c$selection$nodes))

  # Expect that a list 'edges' is not available in 'selection'
  expect_null(graph_a_c$selection$edges)

  # Expect that nodes "a" and "c" are part of a selection
  # object in 'nodes'
  expect_true(all(graph_a_c$selection$nodes == c("a", "c")))

  # Select nodes where `value` > 3
  graph_val_gt_3 <-
    select_nodes(graph = graph,
                 node_attr = "value",
                 search = ">3")

  # Expect that nodes "a" and "c" are part of a selection
  # object in 'nodes'
  expect_true(all(graph_val_gt_3$selection$nodes == c("a", "c")))

  # Select nodes where `value` < 3
  graph_val_lt_3 <-
    select_nodes(graph = graph,
                 node_attr = "value",
                 search = "<3")

  # Expect that nodes "a" and "c" are part of a selection
  # object in 'nodes'
  expect_true(all(graph_val_lt_3$selection$nodes == c("b", "d")))

  # Select nodes where `value` == 2.7
  graph_val_eq_2_7 <-
    select_nodes(graph = graph,
                 node_attr = "value",
                 search = "==2.7")

  # Expect that node "d" is part of a selection object in 'nodes'
  expect_true(all(graph_val_eq_2_7$selection$nodes == "d"))

  # Select nodes where `value` != 2.7
  graph_val_neq_2_7 <-
    select_nodes(graph = graph,
                 node_attr = "value",
                 search = "!=2.7")

  # Expect that nodes "a", "b", and "c" are part of a selection
  # object in 'nodes'
  expect_true(all(graph_val_neq_2_7$selection$nodes == c("a", "b", "c")))

  # Select nodes where `type` is `letter`
  graph_val_letter <-
    select_nodes(graph = graph,
                 node_attr = "type",
                 search = "let")

  # Expect that nodes "a", "b", "c", and "d" are part of a selection
  # object in 'nodes'
  expect_true(all(graph_val_letter$selection$nodes == c("a", "b", "c", "d")))

  # Select nodes where `type` is `letter` and filter to nodes "a" and "b"
  graph_val_letter_a_b <-
    select_nodes(graph = graph,
                 nodes = c("a", "b"),
                 node_attr = "type",
                 search = "let")

  # Expect that nodes "a", "b", "c", and "d" are part of a selection
  # object in 'nodes'
  expect_true(all(graph_val_letter_a_b$selection$nodes == c("a", "b")))

  # Create a union of selections in a magrittr pipeline
  graph_sel_union_a_b_c_d <-
    graph %>% select_nodes(nodes = c("a", "b")) %>%
    select_nodes(nodes = c("c", "d"))

  # Expect that nodes "a", "b", "c", and "d" are part of a selection
  # object in 'nodes'
  expect_true(all(graph_sel_union_a_b_c_d$selection$nodes == c("a", "b", "c", "d")))

  # Create a intersection of selections in a magrittr pipeline
  graph_sel_intersect <-
    graph %>% select_nodes(nodes = c("a", "b", "c")) %>%
    select_nodes(nodes = c("b", "c", "d"), set_op = "intersect")

  # Expect that nodes "b" and "c" are part of a selection object
  # in 'nodes'
  expect_true(all(graph_sel_intersect$selection$nodes == c("b", "c")))

  # Create a selection that is a difference of selections
  graph_sel_difference <-
    graph %>% select_nodes(nodes = c("a", "b", "c")) %>%
    select_nodes(nodes = c("b", "c"), set_op = "difference")

  # Expect that node "a" is part of a selection object in 'nodes'
  expect_true(all(graph_sel_difference$selection$nodes == "a"))

  # Expect an error if selecting nodes from an empty graph
  expect_error(
    select_nodes(graph = create_graph(),
                 nodes = "s")
  )

  # Expect an error if specifying more than one attribute
  expect_error(
    select_nodes(graph = graph,
                 node_attr = c("label", "value"),
                 search = "a")
  )

  # Expect an error if specifying an attribute that doesn't exist
  expect_error(
    select_nodes(graph = graph,
                 node_attr = "fontname",
                 search = "a")
  )

  # Expect an error if specifying a node that doesn't exist
  expect_error(
    select_nodes(graph = graph,
                 nodes = "e")
  )

  expect_error(
    select_nodes(graph = graph,
                 nodes = "e",
                 node_attr = "value",
                 search = ">0")
  )

  # Select the last node in a graph
  graph_last_node <-
    select_last_node(graph)

  # Expect that the node selected is the last in the ndf
  expect_true(
    get_nodes(graph_last_node)[length(get_nodes(graph_last_node))] ==
      get_selection(graph_last_node)[[1]]
  )

  # Expect an error if trying to select the last node from an empty graph
  expect_error(
    select_last_node(create_graph())
  )

  # Select the last edge in a graph
  graph_last_edge <-
    select_last_edge(graph)

  # Expect that the edge selected is the last in the edf
  expect_true(
    get_edges(graph_last_edge)[[1]][length(get_edges(graph_last_edge)[[1]])] ==
      get_selection(graph_last_edge)$edges$from
  )

  expect_true(
    get_edges(graph_last_edge)[[2]][length(get_edges(graph_last_edge)[[2]])] ==
      get_selection(graph_last_edge)$edges$to
  )

  # Expect an error if trying to select the last edge from an empty graph
  expect_error(
    select_last_edge(create_graph())
  )

  # Expect an error if trying to select the last edge from a graph with
  # no edges
  expect_error(
    select_last_edge(create_graph(nodes_df = create_nodes(1)))
  )
})

test_that("selecting an edge in a graph is possible", {

  # Create a simple graph
  nodes <-
    create_nodes(nodes = c("a", "b", "c", "d"),
                 type = "letter",
                 label = TRUE,
                 value = c(3.5, 2.6, 9.4, 2.7))

  edges <-
    create_edges(from = c("a", "b", "c"),
                 to = c("d", "c", "a"),
                 rel = "leading_to",
                 width = c(1, 2, 3))

  graph <-
    create_graph(nodes_df = nodes,
                 edges_df = edges)

  # Select nodes "a" -> "d" and "b" -> "c"
  graph_ad_bc <- select_edges(graph = graph,
                              from = c("a", "b"),
                              to = c("d", "c"))

  # Expect that a selection object is available
  expect_true(!is.null(graph_ad_bc$selection))

  # Expect that a list 'edges' is available in 'selection'
  expect_true(!is.null(graph_ad_bc$selection$edges))

  # Expect that a list 'from' is available in 'selection/edges'
  expect_true(!is.null(graph_ad_bc$selection$edges$from))

  # Expect that a list 'to' is available in 'selection/edges'
  expect_true(!is.null(graph_ad_bc$selection$edges$to))

  # Expect that a list 'nodes' is not available in 'selection'
  expect_null(graph_ad_bc$selection$nodes)

  # Expect that nodes "a" and "b" are part of a selection
  # object in 'from'
  expect_true(all(graph_ad_bc$selection$edges$from == c("a", "b")))

  # Expect that nodes "d" and "c" are part of a selection
  # object in 'to'
  expect_true(all(graph_ad_bc$selection$edges$to == c("d", "c")))

  # Select edges where `width` > 2
  graph_width_gt_2 <-
    select_edges(graph = graph,
                 edge_attr = "width",
                 search = ">2")

  # Expect that node "c" is part of a selection
  # object in 'edges/from'
  expect_true(graph_width_gt_2$selection$edges$from == "c")

  # Expect that node "a" is part of a selection
  # object in 'edges/to'
  expect_true(graph_width_gt_2$selection$edges$to == "a")

  # Select nodes where `width` < 3
  graph_width_lt_3 <-
    select_edges(graph = graph,
                 edge_attr = "width",
                 search = "<3")

  # Expect that nodes "a" and "b" are part of a selection
  # object in 'edges/from'
  expect_true(all(graph_width_lt_3$selection$edges$from == c("a", "b")))

  # Expect that nodes "d" and "c" are part of a selection
  # object in 'edges/to'
  expect_true(all(graph_width_lt_3$selection$edges$to == c("d", "c")))

  # Select nodes where `width` == 2
  graph_width_eq_2 <-
    select_edges(graph = graph,
                 edge_attr = "width",
                 search = "== 2")

  # Expect that node "b" is part of a selection
  # object in 'edges/from'
  expect_true(graph_width_eq_2$selection$edges$from == "b")

  # Expect that node "c" is part of a selection
  # object in 'edges/to'
  expect_true(graph_width_eq_2$selection$edges$to == "c")

  # Select nodes where `width` != 2
  graph_width_neq_2 <-
    select_edges(graph = graph,
                 edge_attr = "width",
                 search = "!=2")

  # Expect that nodes "a" and "c" are part of a selection
  # object in 'edges/from'
  expect_true(all(graph_width_neq_2$selection$edges$from == c("a", "c")))

  # Expect that nodes "d" and "a" are part of a selection
  # object in 'edges/to'
  expect_true(all(graph_width_neq_2$selection$edges$to == c("d", "a")))

  # Select nodes where `rel` is `leading_to`
  graph_val_leading_to <-
    select_edges(graph = graph,
                 edge_attr = "rel",
                 search = "leading")

  # Expect that nodes "a", "b", and "c" are part of a selection
  # object in 'edges/from'
  expect_true(all(graph_val_leading_to$selection$edges$from == c("a", "b", "c")))

  # Expect that nodes "d", "c", and "a" are part of a selection
  # object in 'edges/to'
  expect_true(all(graph_val_leading_to$selection$edges$to == c("d", "c", "a")))

  # Create a union of selections in a magrittr pipeline
  graph_sel_union_ab_bc <-
    graph %>% select_edges(from = "a", to = "d") %>%
    select_edges(from = "b", to = "c")

  # Expect that nodes "a" and "b" are part of a selection
  # object in 'edges/from'
  expect_true(all(graph_sel_union_ab_bc$selection$edges$from == c("a", "b")))

  # Expect that nodes "d" and "c" are part of a selection
  # object in 'edges/to'
  expect_true(all(graph_sel_union_ab_bc$selection$edges$to == c("d", "c")))

  # Create a intersection of selections in a magrittr pipeline
  graph_sel_intersect_bc <-
    graph %>% select_edges(from = c("a", "b"), to = c("d", "c")) %>%
    select_edges(from = c("b", "c"), to = c("c", "a"), set_op = "intersect")

  # Expect that node "b" is part of a selection
  # object in 'edges/from'
  expect_true(graph_sel_intersect_bc$selection$edges$from == "b")

  # Expect that node "c" is part of a selection
  # object in 'edges/to'
  expect_true(graph_sel_intersect_bc$selection$edges$to == "c")

  # Create a selection that is a difference of selections
  graph_sel_edge_difference_ad <-
    graph %>% select_edges(from = c("a", "b", "c"), to = c("d", "c", "a")) %>%
    select_edges(from = c("b", "c"), to = c("c", "a"), set_op = "difference")

  # Expect that node "c" is part of a selection
  # object in 'edges/from'
  expect_true(graph_sel_edge_difference_ad$selection$edges$from == "a")

  # Expect that node "a" is part of a selection
  # object in 'edges/to'
  expect_true(graph_sel_edge_difference_ad$selection$edges$to == "d")

  # Select edges, specifying only the 'from' node
  graph_from_a <-
    select_edges(graph,
                 from = "a")

  # Expect that only the edge "a" -> "d" is selected
  expect_true(graph_from_a$selection$edges$from == "a")
  expect_true(graph_from_a$selection$edges$to == "d")

  # Expect an error if when only specifying the 'from' node,
  # that node isn't present in the graph
  expect_error(
    select_edges(graph,
                 from = "g")
  )

  # Select edges, specifying only the 'to' node
  graph_to_c <-
    select_edges(graph,
                 to = "c")

  # Expect that only the edge "b" -> "c" is selected
  expect_true(graph_to_c$selection$edges$from == "b")
  expect_true(graph_to_c$selection$edges$to == "c")

  # Expect an error if when only specifying the 'to' node,
  # that node isn't present in the graph
  expect_error(
    select_edges(graph,
                 to = "g")
  )

  # Expect an error if selecting edges from an empty graph
  expect_error(
    select_edges(graph = create_graph(),
                 from = "a",
                 to = "c")
  )

  # Expect an error if selecting edges from a graph with nodes
  # but no edges
  expect_error(
    select_edges(graph = create_graph(create_nodes(nodes = "a")))
  )

  # Expect an error if specifying more than one attribute
  expect_error(
    select_edges(graph = graph,
                 edge_attr = c("rel", "width"),
                 search = "a")
  )

  # Expect an error if specifying an attribute that doesn't exist
  expect_error(
    select_edges(graph = graph,
                 edge_attr = "fontname",
                 search = "a")
  )
})

test_that("selecting edges via node IDs is possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node(node = "A") %>% add_node(node = "B") %>%
    add_node(node = "C") %>% add_node(node = "D") %>%
    add_node(node = "E") %>% add_node(node = "F") %>%
    add_node(node = "1") %>% add_node(node = "2") %>%
    add_node(node = "3") %>% add_node(node = "4") %>%
    add_node(node = "5") %>% add_node(node = "6") %>%
    add_node(node = "7") %>% add_node(node = "8") %>%
    add_edge("A", "1") %>%
    add_edge("B", "2") %>%
    add_edge("B", "3") %>%
    add_edge("B", "4") %>%
    add_edge("C", "A") %>%
    add_edge("1", "D") %>%
    add_edge("E", "A") %>%
    add_edge("2", "4") %>%
    add_edge("1", "5") %>%
    add_edge("1", "F") %>%
    add_edge("E", "6") %>%
    add_edge("4", "6") %>%
    add_edge("5", "7") %>%
    add_edge("6", "7") %>%
    add_edge("3", "8")

  # Select all edges associated with nodes with ID `C`, `A`, and `E`
  graph <- select_edges_by_node_id(graph, nodes = c("C", "A", "E"))

  # Expect that certain edges with be available in the selection
  expect_true(all(graph$selection$edges$from %in%
                    c("A", "C", "E", "E")))
  expect_true(all(graph$selection$edges$to %in%
                    c("1", "A", "A", "6")))
})

test_that("selecting nodes in a neighborhood is possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node(node = "A") %>% add_node(node = "B") %>%
    add_node(node = "C") %>% add_node(node = "D") %>%
    add_node(node = "E") %>% add_node(node = "F") %>%
    add_node(node = "1") %>% add_node(node = "2") %>%
    add_node(node = "3") %>% add_node(node = "4") %>%
    add_node(node = "5") %>% add_node(node = "6") %>%
    add_node(node = "7") %>% add_node(node = "8") %>%
    add_edge("A", "1") %>%
    add_edge("B", "2") %>%
    add_edge("B", "3") %>%
    add_edge("B", "4") %>%
    add_edge("C", "A") %>%
    add_edge("1", "D") %>%
    add_edge("E", "A") %>%
    add_edge("2", "4") %>%
    add_edge("1", "5") %>%
    add_edge("1", "F") %>%
    add_edge("E", "6") %>%
    add_edge("4", "6") %>%
    add_edge("5", "7") %>%
    add_edge("6", "7") %>%
    add_edge("3", "8")

  # Create a selection of nodes centered around node "U" and
  # including those nodes a depth of 2 edges away
  graph_sel_1_dist_2 <-
    select_nodes_in_neighborhood(graph = graph,
                                 node = "1",
                                 distance = 2)

  # Expect that specific nodes  are part of a selection
  # object in 'nodes'
  expect_true(all(graph_sel_1_dist_2$selection$nodes ==
                    c("D", "5", "F", "A", "1", "7", "C", "E")))

  # Create a selection of nodes centered around node "U" and
  # including those nodes a depth of 2 edges away
  graph_sel_1_sel_4_dist_2 <-
    select_nodes_in_neighborhood(graph = graph_sel_1_dist_2,
                                 node = "4",
                                 distance = 2)

  # Expect that specific nodes  are part of a selection
  # object in 'nodes'
  expect_true(all(graph_sel_1_sel_4_dist_2$selection$nodes ==
                    c("D", "5", "F", "A", "1", "7", "C",
                      "E", "6", "B", "2", "4", "3")))

  # Create a selection of nodes centered around node "U" and
  # including those nodes a depth of 2 edges away
  graph_sel_1_sel_4_dist_2_sel_A_dist_3 <-
    select_nodes_in_neighborhood(graph = graph_sel_1_sel_4_dist_2,
                                 node = "A",
                                 distance = 3,
                                 set_op = "intersect")

  # Expect that specific nodes  are part of a selection
  # object in 'nodes'
  expect_true(all(graph_sel_1_sel_4_dist_2_sel_A_dist_3$selection$nodes ==
                    c("D", "5", "F", "A", "1", "7", "C",
                      "E", "6", "4")))

  # Create a selection of nodes centered around node "U" and
  # including those nodes a depth of 2 edges away
  graph_sel_1_sel_4_dist_2_sel_A_dist_3 <-
    select_nodes_in_neighborhood(graph = graph_sel_1_sel_4_dist_2,
                                 node = "7",
                                 distance = 2,
                                 set_op = "difference")

  # Expect that specific nodes are part of a selection
  # object in 'nodes'
  expect_true(all(graph_sel_1_sel_4_dist_2_sel_A_dist_3$selection$nodes ==
                    c("D", "F", "A", "C", "B", "2", "3")))
})

test_that("getting a selection is possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node(node = "A") %>% add_node(node = "B") %>%
    add_node(node = "C") %>% add_node(node = "D") %>%
    add_node(node = "E") %>% add_node(node = "F") %>%
    add_node(node = "1") %>% add_node(node = "2") %>%
    add_node(node = "3") %>% add_node(node = "4") %>%
    add_node(node = "5") %>% add_node(node = "6") %>%
    add_node(node = "7") %>% add_node(node = "8") %>%
    add_edge("A", "1") %>%
    add_edge("B", "2") %>%
    add_edge("B", "3") %>%
    add_edge("B", "4") %>%
    add_edge("C", "A") %>%
    add_edge("1", "D") %>%
    add_edge("E", "A") %>%
    add_edge("2", "4") %>%
    add_edge("1", "5") %>%
    add_edge("1", "F") %>%
    add_edge("E", "6") %>%
    add_edge("4", "6") %>%
    add_edge("5", "7") %>%
    add_edge("6", "7") %>%
    add_edge("3", "8")

  # Select all nodes in graph and get selection
  graph_node_selection_1 <-
    graph %>% select_nodes() %>% get_selection()

  # Expect that specific nodes are returned
  expect_true(all(graph_node_selection_1$nodes ==
                    c("A", "B", "C", "D", "E", "F", "1",
                      "2", "3", "4", "5", "6", "7", "8")))

  # Select all edges in graph and get selection
  graph_edge_selection_1 <-
    graph %>% select_edges() %>% get_selection()

  # Expect that specific nodes are returned
  expect_true(all(graph_edge_selection_1$edges$from ==
                    c("A", "B", "B", "B", "C", "1", "E", "2",
                      "1", "1", "E", "4", "5", "6", "3")))
  # Expect that specific nodes are returned
  expect_true(all(graph_edge_selection_1$edges$to ==
                    c("1", "2", "3", "4", "A", "D", "A", "4",
                      "5", "F", "6", "6", "7", "7", "8")))
})

test_that("inverting a selection is possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node(node = "A") %>% add_node(node = "B") %>%
    add_node(node = "C") %>% add_node(node = "D") %>%
    add_node(node = "E") %>% add_node(node = "F") %>%
    add_node(node = "1") %>% add_node(node = "2") %>%
    add_node(node = "3") %>% add_node(node = "4") %>%
    add_node(node = "5") %>% add_node(node = "6") %>%
    add_node(node = "7") %>% add_node(node = "8") %>%
    add_edge("A", "1") %>%
    add_edge("B", "2") %>%
    add_edge("B", "3") %>%
    add_edge("B", "4") %>%
    add_edge("C", "A") %>%
    add_edge("1", "D") %>%
    add_edge("E", "A") %>%
    add_edge("2", "4") %>%
    add_edge("1", "5") %>%
    add_edge("1", "F") %>%
    add_edge("E", "6") %>%
    add_edge("4", "6") %>%
    add_edge("5", "7") %>%
    add_edge("6", "7") %>%
    add_edge("3", "8")

  # Select nodes "1" and "2" in the graph
  graph_select_1_2 <-
    graph %>% select_nodes(nodes = "1") %>% select_nodes(nodes = "2")

  # Invert the selection so that every other node is selected
  graph_select_1_2_inverted <-
    graph_select_1_2 %>% invert_selection()

  # Expect that all nodes except "1" and "2" are in selection
  expect_true(all(graph_select_1_2_inverted$selection$nodes %in%
                    c("A", "B", "C", "D", "E", "F",
                      "3","4", "5", "6", "7", "8")))

  # Select edges "1" -> "5" and "4" -> "6" in the graph
  graph_select_edges_1_5__4_6 <-
    graph %>% select_edges(from = "1", to = "5") %>%
    select_edges(from = "4", to = "6")

  # Invert the selection so that every other edge is selected
  graph_select_edges_1_5__4_6_inverted <-
    graph_select_edges_1_5__4_6 %>% invert_selection()

  # Expect that every other edge is now in the selection
  expect_true(all(graph_select_edges_1_5__4_6_inverted$selection$edges$from %in%
                    c("A", "B", "B", "B", "C", "1", "E",
                      "2", "1", "E", "5", "6", "3")))
  expect_true(all(graph_select_edges_1_5__4_6_inverted$selection$edges$to %in%
                    c("1", "2", "3", "4", "A", "D", "A",
                      "4", "F", "6", "7", "7", "8")))

  # Expect an error if inverting selection that doesn't exist
  expect_error(
    invert_selection(graph)
  )
})

test_that("getting/clearing a selection is possible", {

  # Create a graph
  graph <-
    create_graph() %>% add_node %>% add_node %>%
    add_edge(1, 2)

  # Select all nodes in the graph
  graph_select_all_nodes <-
    graph %>% select_nodes()

  # Get the selection and expect both nodes to be present
  expect_true(all(c("1", "2") %in%
                    get_selection(graph_select_all_nodes)[[1]]))

  # Clear the selection
  graph_select_all_nodes_cleared <-
    clear_selection(graph_select_all_nodes)

  # Expect no selection to be present in the graph
  expect_null(graph_select_all_nodes_cleared$selection)

  # Expect an NA value returned when getting a selection that
  # is not present
  expect_true(is.na(get_selection(graph_select_all_nodes_cleared)))
})
