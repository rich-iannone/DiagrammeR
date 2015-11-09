context("Setting and getting node attributes")

test_that("setting node attributes is possible", {

  library(magrittr)

  # Create a graph
  graph <-
    create_graph() %>%
    add_node("A") %>% add_node("B") %>% add_node("C") %>%
    add_node("D") %>% add_node("E") %>% add_node("F") %>%
    add_node("1") %>% add_node("2") %>% add_node("3") %>%
    add_node("4") %>% add_node("5") %>% add_node("6") %>%
    add_node("7") %>% add_node("8") %>%
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

  # Set attribute for named node "A"
  graph_set_a <-
    set_node_attr(graph,
                  nodes = "A",
                  node_attr = "value",
                  value = 5)

  # Expect that node "A" has node attr set for `value`
  expect_equal(graph_set_a$nodes_df[which(graph_set_a$nodes_df$nodes == "A"), 4],
               "5")

  # Get node attribute for named node "A"
  graph_set_a_node_attr_df <-
    get_node_attr(graph_set_a,
                  nodes = "A")

  # Expect that node "A" has node attr set for `value`
  expect_equal(graph_set_a_node_attr_df$value, "5")

  # Set attribute for named node "A" with a different value
  graph_set_a <-
    set_node_attr(graph,
                  nodes = "A",
                  node_attr = "value",
                  value = 8)

  # Expect that node "A" has node attr set for `value`
  expect_equal(graph_set_a$nodes_df[which(graph_set_a$nodes_df$nodes == "A"), 4],
               "8")

  # Select node "A"
  graph_select_a <-
    select_nodes(graph, "A")

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
                  value = 5)

  # Expect that all nodes have the attribute set
  expect_true(all(graph_set_all$nodes_df$value == "5"))

  # Select node "A" and apply a node attribute using that
  # node selection
  graph_node_selection <-
    graph %>% select_nodes("A") %>%
    set_node_attr_with_selection(node_attr = "value", value = 5)

  # Expect that node "A" has node attr set for `value`
  expect_equal(graph_node_selection$nodes_df[which(graph_node_selection$nodes_df$nodes == "A"), 4],
               "5")

  # Expect that getting the node attribute from a
  # selection works in the same way
  expect_equal(get_node_attr_from_selection(graph_node_selection)$value,
               "5")

  # Get the node data frame from the graph as a separate object
  graph_node_df <- graph$nodes_df

  # Set attribute for named node "A" in the ndf
  graph_node_df_set_a <-
    set_node_attr(graph_node_df,
                  nodes = "A",
                  node_attr = "value",
                  value = 5)

  # Expect that node "A" has node attr set for `value`
  expect_equal(graph_node_df_set_a[which(graph_node_df_set_a$nodes == "A"), 4],
               "5")

  # Get node attribute for named node "A"
  graph_node_df_set_a_node_attr_df <-
    get_node_attr(graph_node_df_set_a,
                  nodes = "A")

  # Expect that node "A" has node attr set for `value`
  expect_equal(graph_node_df_set_a_node_attr_df$value, "5")

  # Set attribute for named node "A" with a different value
  graph_node_df_set_a_node_attr_df <-
    set_node_attr(graph_node_df_set_a,
                  nodes = "A",
                  node_attr = "value",
                  value = 8)

  # Expect that node "A" in the ndf has node attr set for `value`
  expect_equal(graph_node_df_set_a_node_attr_df[which(graph_node_df_set_a_node_attr_df$nodes == "A"), 4],
               "8")

  # Set attribute for all nodes in the ndf
  graph_node_df_set_all <-
    set_node_attr(graph_node_df,
                  node_attr = "value",
                  value = 5)

  # Expect that all nodes in the ndf will have the attribute set
  expect_true(all(graph_node_df_set_all$value == "5"))

  # Expect that getting the node attribute from a graph without
  # a selection will result in an error
  expect_error(get_node_attr_from_selection(graph))

  # Expect an error if the attribute selected is `nodes`
  expect_error(
    set_node_attr(graph, nodes = "A",
                  node_attr = "nodes", value = "B")
  )

  # Expect an error if the attribute selected is `nodes`
  expect_error(
    set_node_attr(graph, nodes = "A",
                  node_attr = "value", value = c("1", "2"))
  )
})

test_that("setting edge attributes is possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node("A") %>% add_node("B") %>% add_node("C") %>%
    add_node("D") %>% add_node("E") %>% add_node("F") %>%
    add_node("1") %>% add_node("2") %>% add_node("3") %>%
    add_node("4") %>% add_node("5") %>% add_node("6") %>%
    add_node("7") %>% add_node("8") %>%
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

  # Set edge attribute for edge "A" -> "1"
  graph_set_a_1 <-
    set_edge_attr(graph,
                  from = "A",
                  to = "1",
                  edge_attr = "value",
                  value = 5)

  # Expect that edge "A" -> "1" has edge attr set for `value`
  expect_equal(
    graph_set_a_1$edges_df[which(graph_set_a_1$edges_df$from == "A" &
                                   graph_set_a_1$edges_df$to == "1"), 4],
    "5")

  # Get edge attribute for edge "A" -> "1"
  graph_set_a_1_edge_attr_df <-
    get_edge_attr(graph_set_a_1,
                  from = "A",
                  to = "1")

  # Expect that edge "A" -> "1" has edge attr set for `value`
  expect_equal(
    graph_set_a_1_edge_attr_df[which(graph_set_a_1_edge_attr_df$from == "A" &
                                       graph_set_a_1_edge_attr_df$to == "1"), 4],
    "5")

  # Set attribute for named edge "A" -> "1" with a different value
  graph_set_a_1 <-
    set_edge_attr(graph_set_a_1,
                  from = "A",
                  to = "1",
                  edge_attr = "value",
                  value = 8)

  # Expect that edge "A" -> "1" has edge attr set for `value`
  expect_equal(
    graph_set_a_1$edges_df[which(graph_set_a_1$edges_df$from == "A" &
                                   graph_set_a_1$edges_df$to == "1"), 4],
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
    graph_select_a_1$edges_df[which(graph_select_a_1$edges_df$from == "A" &
                                      graph_select_a_1$edges_df$to == "1"), 4],
    "5")

  # Set attribute for all edges
  graph_set_all <-
    set_edge_attr(graph,
                  edge_attr = "value",
                  value = 5)

  # Expect that all edges have the attribute set
  expect_true(all(graph_set_all$edges_df$value == "5"))

  # Select edge "A" -> "1" and apply an edge attribute using that
  # edge selection
  graph_edge_selection <-
    graph %>% select_edges("A", "1") %>%
    set_edge_attr_with_selection(edge_attr = "value", value = 5)

  # Expect that edge "A" -> "1" has edge attr set for `value`
  expect_equal(
    graph_edge_selection$edges_df[which(graph_edge_selection$edges_df$from == "A" &
                                          graph_edge_selection$edges_df$to == "1"), 4],
    "5")

  # Expect that getting the node attribute from a
  # selection works in the same way
  expect_equal(get_edge_attr_from_selection(graph_edge_selection)$value,
               "5")
})
