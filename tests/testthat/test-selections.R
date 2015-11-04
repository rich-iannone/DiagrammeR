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
                 rel = "leading_to")

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
                 comparison = ">3")

  # Expect that nodes "a" and "c" are part of a selection
  # object in 'nodes'
  expect_true(all(graph_val_gt_3$selection$nodes == c("a", "c")))

  # Select nodes where `value` < 3
  graph_val_lt_3 <-
    select_nodes(graph = graph,
                 node_attr = "value",
                 comparison = "<3")

  # Expect that nodes "a" and "c" are part of a selection
  # object in 'nodes'
  expect_true(all(graph_val_lt_3$selection$nodes == c("b", "d")))

  # Select nodes where `value` == 2.7
  graph_val_eq_2_7 <-
    select_nodes(graph = graph,
                 node_attr = "value",
                 comparison = "==2.7")

  # Expect that node "d" is part of a selection object in 'nodes'
  expect_true(all(graph_val_eq_2_7$selection$nodes == "d"))

  # Select nodes where `value` != 2.7
  graph_val_neq_2_7 <-
    select_nodes(graph = graph,
                 node_attr = "value",
                 comparison = "!=2.7")

  # Expect that nodes "a", "b", and "c" are part of a selection
  # object in 'nodes'
  expect_true(all(graph_val_neq_2_7$selection$nodes == c("a", "b", "c")))

  # Select nodes where `type` is `letter`
  graph_val_letter <-
    select_nodes(graph = graph,
                 node_attr = "type",
                 regex = "let")

  # Expect that nodes "a", "b", "c", and "d" are part of a selection
  # object in 'nodes'
  expect_true(all(graph_val_letter$selection$nodes == c("a", "b", "c", "d")))

  # Create a union of selections in a magrittr pipeline
  graph_sel_union_a_b_c_d <-
    graph %>% select_nodes(c("a", "b")) %>% select_nodes(c("c", "d"))

  # Expect that nodes "a", "b", "c", and "d" are part of a selection
  # object in 'nodes'
  expect_true(all(graph_sel_union_a_b_c_d$selection$nodes == c("a", "b", "c", "d")))

  # Create a intersection of selections in a magrittr pipeline
  graph_sel_intersect <-
    graph %>% select_nodes(c("a", "b", "c")) %>%
    select_nodes(c("b", "c", "d"), set_op = "intersect")

  # Expect that nodes "b" and "c" are part of a selection object
  # in 'nodes'
  expect_true(all(graph_sel_intersect$selection$nodes == c("b", "c")))

  # Create a selection that is a difference of selections
  graph_sel_difference <-
    graph %>% select_nodes(c("a", "b", "c")) %>%
    select_nodes(c("b", "c"), set_op = "difference")

  # Expect that node "a" is part of a selection object in 'nodes'
  expect_true(all(graph_sel_difference$selection$nodes == "a"))

  # Expect an error if a comparison and regex are used together
  expect_error(
    select_nodes(graph = graph,
                 node_attr = "value",
                 comparison = ">3",
                 regex = "3")
  )

  # Expect an error if selecting nodes from an empty graph
  expect_error(
    select_nodes(graph = create_graph(),
                 nodes = "s")
  )

  # Expect an error if specifying more than one attribute
  expect_error(
    select_nodes(graph = graph,
                 node_attr = c("label", "value"),
                 regex = "a")
  )

  # Expect an error if specifying an attribute that doesn't exist
  expect_error(
    select_nodes(graph = graph,
                 nodes = "e")
  )

  expect_error(
    select_nodes(graph = graph,
                 nodes = "e",
                 node_attr = "value",
                 comparison = ">0")
  )
})
