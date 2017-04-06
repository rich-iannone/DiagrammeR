context("Selection of last-created nodes or edges in a graph")

test_that("selected that last edges created is possible", {

  # Create a graph and add a cycle and then
  # a tree in 2 separate function calls
  graph <-
    create_graph() %>%
    add_cycle(3, rel = "a") %>%
    add_balanced_tree(2, 2, rel = "b")

  # Select the last edges created (all edges
  # from the tree) and then set their edge
  # color to be `red`
  graph_e <-
    graph %>%
    select_last_edges_created() %>%
    set_edge_attrs_ws("color", "red") %>%
    clear_selection()

  # Expect that the `red` attribute value
  # for the `color` column is only applied
  # the last 6 edges
  expect_identical(
    get_edge_df(graph_e)$color,
    c(rep(as.character(NA), 3),
      rep("red", 6)))

  # Create a graph and add a cycle and then
  # a tree in 2 separate function calls
  graph <-
    create_graph() %>%
    add_cycle(3, rel = "a") %>%
    add_balanced_tree(2, 2, rel = "b")

  # Select the last nodes created (all nodes
  # from the tree) and then set their node
  # color to be `red`
  graph_n <-
    graph %>%
    select_last_nodes_created() %>%
    set_node_attrs_ws("color", "red") %>%
    clear_selection()

  # Expect that the `red` attribute value
  # for the `color` column is only applied
  # the last 7 nodes
  expect_identical(
    get_node_df(graph_n)$color,
    c(rep(as.character(NA), 3),
      rep("red", 7)))
})
