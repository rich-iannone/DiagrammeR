context("Setting and getting node attributes")

test_that("setting node attrs", {

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
})
