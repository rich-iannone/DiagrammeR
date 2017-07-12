context("Setting node positions")

test_that("setting a single node's position is possible", {

  # Create a simple graph with 4 nodes
  graph <-
    create_graph() %>%
    add_node(label = "one") %>%
    add_node(label = "two") %>%
    add_node(label = "three") %>%
    add_node(label = "four")

  # Add position information to each of
  # the graph's nodes
  graph_1 <-
    graph %>%
    set_node_position(
      node = 1, x = 1, y = 1) %>%
    set_node_position(
      node = 2, x = 2, y = 2) %>%
    set_node_position(
      node = 3, x = 3, y = 3) %>%
    set_node_position(
      node = 4, x = 4, y = 4)

  # Expect that the `x` and `y` node
  # attributes columns are available
  expect_true(
    all(c("x", "y") %in% colnames(graph_1$nodes_df)))

  # Expect specific values in the `x` column
  expect_equal(
    graph_1$nodes_df$x, c(1, 2, 3, 4))

  # Expect specific values in the `y` column
  expect_equal(
    graph_1$nodes_df$y, c(1, 2, 3, 4))

  # Add position information to each of the
  # graph's nodes using node `label` values
  graph_2 <-
    graph %>%
    set_node_position(
      node = "one",
      x = 1, y = 1,
      use_labels = TRUE) %>%
    set_node_position(
      node = "two",
      x = 2, y = 2,
      use_labels = TRUE) %>%
    set_node_position(
      node = "three",
      x = 3, y = 3,
      use_labels = TRUE) %>%
    set_node_position(
      node = "four",
      x = 4, y = 4,
      use_labels = TRUE)

  # Expect that the `x` and `y` node
  # attributes columns are available
  expect_true(
    all(c("x", "y") %in% colnames(graph_2$nodes_df)))

  # Expect specific values in the `x` column
  expect_equal(
    graph_2$nodes_df$x, c(1, 2, 3, 4))

  # Expect specific values in the `y` column
  expect_equal(
    graph_2$nodes_df$y, c(1, 2, 3, 4))
})

test_that("altering several nodes' positions is possible", {

  # Create a simple graph with 4 nodes
  graph <-
    create_graph() %>%
    add_node(type = "a", label = "one") %>%
    add_node(type = "a", label = "two") %>%
    add_node(type = "b", label = "three") %>%
    add_node(type = "b", label = "four")

  # Add position information to each of
  # the graph's nodes
  graph_1 <-
    graph %>%
    set_node_position(
      node = 1, x = 1, y = 1) %>%
    set_node_position(
      node = 2, x = 2, y = 2) %>%
    set_node_position(
      node = 3, x = 3, y = 3) %>%
    set_node_position(
      node = 4, x = 4, y = 4)

  # Select all of the graph's nodes using the
  # `select_nodes()` function (and only specifying
  # the graph object)
  graph_1 <- select_nodes(graph_1)

  # Move the selected nodes (all the nodes,
  # in this case) 5 units to the right
  graph_1 <-
    graph_1 %>%
    nudge_node_positions_ws(
      dx = 5, dy = 0)

  # Expect that the `x` values have specific values
  expect_equal(
    graph_1$nodes_df$x, c(6, 7, 8, 9))

  # Expect that the `y` values have specific values
  expect_equal(
    graph_1$nodes_df$y, c(1, 2, 3, 4))

  # Now select nodes that have `type == "b"`
  # and move them in the `y` direction 2 units
  graph_2 <-
    graph %>%
    select_nodes(conditions = "type == 'b'") %>%
    set_node_position(
      node = 1, x = 1, y = 1) %>%
    set_node_position(
      node = 2, x = 2, y = 2) %>%
    set_node_position(
      node = 3, x = 3, y = 3) %>%
    set_node_position(
      node = 4, x = 4, y = 4) %>%
    nudge_node_positions_ws(
      dx = 0, dy = 2)

  # Expect that the `x` values have specific values
  expect_equal(
    graph_2$nodes_df$x, c(1, 2, 3, 4))

  # Expect that the `y` values have specific values
  expect_equal(
    graph_2$nodes_df$y, c(1, 2, 5, 6))
})

test_that("setting positions with a text string is possible", {

  # Create a graph with unique labels and
  # several node `type` groupings
  graph <-
    create_graph() %>%
    add_node(type = "a", label = "a") %>%
    add_node(type = "a", label = "b") %>%
    add_node(type = "b", label = "c") %>%
    add_node(type = "b", label = "d") %>%
    add_node(type = "b", label = "e") %>%
    add_node(type = "c", label = "f") %>%
    add_node(type = "c", label = "g")

  # Define a 'layout' for groups of nodes
  # using a text string
  layout <-
    "
1--------
1--------
---222---
--------3
--------3
"

  # Use the `layout` along with what nodes
  # the numbers correspond to in the graph
  # with the `nodes` named vectors
  graph <-
    graph %>%
    layout_nodes_w_string(
      layout = layout,
      nodes = c("1" = "type:a",
                "2" = "type:b",
                "3" = "type:c"),
      sort = c("1" = "label:asc",
               "2" = "label:desc",
               "3" = "label:desc"))

  # Expect that the `x` and `y` node
  # attributes columns are available
  expect_true(
    all(c("x", "y") %in% colnames(graph$nodes_df)))

  # Expect specific values in the `x` column
  expect_equal(
    graph$nodes_df$x, c(0, 0, 5, 4, 3, 8, 8))

  # Expect specific values in the `y` column
  expect_equal(
    graph$nodes_df$y, c(8, 6, 4, 4, 4, 0, 2))
})
