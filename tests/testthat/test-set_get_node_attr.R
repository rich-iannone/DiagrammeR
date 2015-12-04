context("Setting and getting node attributes")

test_that("setting node attributes is possible", {

  # Create a graph
  graph <- create_graph()
  graph <- add_node(graph, node = "A")
  graph <- add_node(graph, node = "B")
  graph <- add_node(graph, node = "C")
  graph <- add_node(graph, node = "D")
  graph <- add_node(graph, node = "E")
  graph <- add_node(graph, node = "F")
  graph <- add_node(graph, node = "1")
  graph <- add_node(graph, node = "2")
  graph <- add_node(graph, node = "3")
  graph <- add_node(graph, node = "4")
  graph <- add_node(graph, node = "5")
  graph <- add_node(graph, node = "6")
  graph <- add_node(graph, node = "7")
  graph <- add_node(graph, node = "8")
  graph <- add_edge(graph, "A", "1")
  graph <- add_edge(graph, "B", "2")
  graph <- add_edge(graph, "B", "3")
  graph <- add_edge(graph, "B", "4")
  graph <- add_edge(graph, "C", "A")
  graph <- add_edge(graph, "1", "D")
  graph <- add_edge(graph, "E", "A")
  graph <- add_edge(graph, "2", "4")
  graph <- add_edge(graph, "1", "5")
  graph <- add_edge(graph, "1", "F")
  graph <- add_edge(graph, "E", "6")
  graph <- add_edge(graph, "4", "6")
  graph <- add_edge(graph, "5", "7")
  graph <- add_edge(graph, "6", "7")
  graph <- add_edge(graph, "3", "8")

  # Set attribute for named node "A"
  graph_set_a <- set_node_attr(graph,
                               nodes = "A",
                               node_attr = "value",
                               values = 5)

  # Expect that node "A" has node attr set for `value`
  expect_equal(graph_set_a$nodes_df[which(graph_set_a$nodes_df$nodes == "A"), 4],
               "5")

  # Expect that node "A" has node attr set for `value`
  expect_equal(withdraw_values(deposit_node_attr(graph_set_a,
                                                 node_attr = "value",
                                                 nodes = "A")), "5")

  # Set attribute for named node "A" with a different value
  graph_set_a <-
    set_node_attr(graph,
                  nodes = "A",
                  node_attr = "value",
                  values = 8)

  # Expect that node "A" has node attr set for `value`
  expect_equal(withdraw_values(deposit_node_attr(graph_set_a,
                                                 node_attr = "value",
                                                 nodes = "A")), "8")

  # Select node "A"
  graph_select_a <-
    select_nodes(graph, nodes = "A")

  # Set attribute for selected node "A"
  graph_select_a <-
    set_node_attr_with_selection(graph_select_a,
                                 node_attr = "value",
                                 value = 5)

  # Expect that node "A" has node attr set for `value`
  expect_equal(graph_select_a$nodes_df[which(graph_select_a$nodes_df$nodes == "A"), 4],
               "5")

  # Set attribute for all nodes
  graph_set_all <-
    set_node_attr(graph,
                  node_attr = "value",
                  values = 5)

  # Expect that all nodes have the attribute set
  expect_true(all(graph_set_all$nodes_df$value == "5"))

  # Select node "A" and apply a node attribute using that
  # node selection
  graph_node_selection <-
    graph %>% select_nodes(nodes = "A") %>%
    set_node_attr_with_selection(node_attr = "value", value = 5)

  # Expect that node "A" has node attr set for `value`
  expect_equal(graph_node_selection$nodes_df[which(graph_node_selection$nodes_df$nodes == "A"), 4],
               "5")

  # Expect that getting the node attribute from a
  # selection works in the same way
  expect_equal(
    withdraw_values(
      deposit_node_attr_from_selection(graph_node_selection, node_attr = "value")),
    "5")

  # Get the node data frame from the graph as a separate object
  graph_node_df <- graph$nodes_df

  # Set attribute for named node "A" in the ndf
  graph_node_df_set_a <-
    set_node_attr(graph_node_df,
                  nodes = "A",
                  node_attr = "value",
                  values = 5)

  # Expect that node "A" has node attr set for `value`
  expect_equal(graph_node_df_set_a[which(graph_node_df_set_a$nodes == "A"), 4],
               "5")

  # Set attribute for named node "A" with a different value
  graph_node_df_set_a_node_attr_df <-
    set_node_attr(graph_node_df_set_a,
                  nodes = "A",
                  node_attr = "value",
                  values = 8)

  # Expect that node "A" in the ndf has node attr set for `value`
  expect_equal(graph_node_df_set_a_node_attr_df[which(graph_node_df_set_a_node_attr_df$nodes == "A"), 4],
               "8")

  # Set attribute for all nodes in the ndf
  graph_node_df_set_all <-
    set_node_attr(graph_node_df,
                  node_attr = "value",
                  values = 5)

  # Expect that all nodes in the ndf will have the attribute set
  expect_true(all(graph_node_df_set_all$value == "5"))

  # Expect that getting the node attribute from a graph without
  # a selection will result in an error
  expect_error(deposit_node_attr_from_selection(graph))

  # Expect an error if the attribute selected is `nodes`
  expect_error(
    set_node_attr(graph, nodes = "A",
                  node_attr = "nodes", values = "B")
  )

  # Expect an error if the attribute selected is `nodes`
  expect_error(
    set_node_attr(graph, nodes = "A",
                  node_attr = "value", values = c("1", "2"))
  )
})

test_that("setting edge attributes is possible", {

  # Create a graph
  graph <- create_graph()
  graph <- add_node(graph, node = "A")
  graph <- add_node(graph, node = "B")
  graph <- add_node(graph, node = "C")
  graph <- add_node(graph, node = "D")
  graph <- add_node(graph, node = "E")
  graph <- add_node(graph, node = "F")
  graph <- add_node(graph, node = "1")
  graph <- add_node(graph, node = "2")
  graph <- add_node(graph, node = "3")
  graph <- add_node(graph, node = "4")
  graph <- add_node(graph, node = "5")
  graph <- add_node(graph, node = "6")
  graph <- add_node(graph, node = "7")
  graph <- add_node(graph, node = "8")
  graph <- add_edge(graph, "A", "1")
  graph <- add_edge(graph, "B", "2")
  graph <- add_edge(graph, "B", "3")
  graph <- add_edge(graph, "B", "4")
  graph <- add_edge(graph, "C", "A")
  graph <- add_edge(graph, "1", "D")
  graph <- add_edge(graph, "E", "A")
  graph <- add_edge(graph, "2", "4")
  graph <- add_edge(graph, "1", "5")
  graph <- add_edge(graph, "1", "F")
  graph <- add_edge(graph, "E", "6")
  graph <- add_edge(graph, "4", "6")
  graph <- add_edge(graph, "5", "7")
  graph <- add_edge(graph, "6", "7")
  graph <- add_edge(graph, "3", "8")

  # Set edge attribute for edge "A" -> "1"
  graph_set_a_1 <-
    set_edge_attr(graph,
                  from = "A",
                  to = "1",
                  edge_attr = "value",
                  values = 5)

  # Expect that edge "A" -> "1" has edge attr set for `value`
  expect_equal(
    graph_set_a_1$edges_df[which(graph_set_a_1$edges_df$from == "A" &
                                   graph_set_a_1$edges_df$to == "1"), 4],
    "5")

  # Get edge attribute for edge "A" -> "1"
  graph_set_a_1_edge_attr <-
    withdraw_values(deposit_edge_attr(graph_set_a_1,
                                      edge_attr = "value",
                                      from = "A",
                                      to = "1"))

  # Expect that edge "A" -> "1" has edge attr set for `value`
  expect_equal(graph_set_a_1_edge_attr, "5")

  # Set attribute for named edge "A" -> "1" with a different value
  graph_set_a_1 <-
    set_edge_attr(graph_set_a_1,
                  from = "A",
                  to = "1",
                  edge_attr = "value",
                  values = 8)

  # Expect that edge "A" -> "1" has edge attr set for `value`
  expect_equal(
    withdraw_values(deposit_edge_attr(graph_set_a_1,
                                      edge_attr = "value",
                                      from = "A",
                                      to = "1")),
    "8")

  # Select edge "A" -> "1"
  graph_select_a_1 <-
    select_edges(graph, from = "A", to = "1")

  # Set attribute for selected node "A"
  graph_select_a_1 <-
    set_edge_attr_with_selection(graph = graph_select_a_1,
                                 edge_attr = "value",
                                 value = 5)

  # Expect that edge "A" -> "1" has edge attr set for `value`
  expect_equal(
    withdraw_values(deposit_edge_attr(graph_select_a_1,
                                      edge_attr = "value",
                                      from = "A",
                                      to = "1")),
    "5")

  # Set attribute for all edges
  graph_set_all <-
    set_edge_attr(graph,
                  edge_attr = "value",
                  values = 5)

  # Expect that all edges have the attribute set
  expect_true(all(graph_set_all$edges_df$value == "5"))

  # Select edge "A" -> "1" and apply an edge attribute using that
  # edge selection
  graph_edge_selection <-
    graph %>% select_edges(from = "A", to = "1") %>%
    set_edge_attr_with_selection(edge_attr = "value", value = 5)

  # Expect that edge "A" -> "1" has edge attr set for `value`
  expect_equal(
    graph_edge_selection$edges_df[which(graph_edge_selection$edges_df$from == "A" &
                                          graph_edge_selection$edges_df$to == "1"), 4],
    "5")
})
