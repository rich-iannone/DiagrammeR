context("Setting and getting node attributes")

test_that("setting node attributes is possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_path(8)

  # Set attribute for node `1`
  graph_set_a <-
    set_node_attrs(
      graph,
      nodes = 1,
      node_attr = "value",
      values = 5)

  # Expect that node `1` has node attr set for `value`
  expect_equal(
    graph_set_a$nodes_df[
      which(graph_set_a$nodes_df$id == 1), 4], 5)

  # Expect that node `1` has node attr set for `value`
  expect_equal(
    get_cache(
      cache_node_attrs(
        graph_set_a,
        node_attr = "value",
        nodes = 1)), 5)

  # Set attribute for node `1` with a different value
  graph_set_a <-
    set_node_attrs(
      graph,
      nodes = 1,
      node_attr = "value",
      values = 8)

  # Expect that node `1` has node attr set for `value`
  expect_equal(
    get_cache(
      cache_node_attrs(
        graph_set_a,
        node_attr = "value",
        nodes = 1)), 8)

  # Select node `1`
  graph_select_a <- select_nodes(graph, nodes = 1)

  # Set attribute for selected node `1`
  graph_select_a <-
    set_node_attrs_ws(
      graph_select_a,
      node_attr = "value",
      value = 5)

  # Expect that node `1` has node attr set for `value`
  expect_equal(
    graph_select_a$nodes_df[
      which(graph_select_a$nodes_df$id == 1), 4], 5)

  # Set attribute for all nodes
  graph_set_all <-
    set_node_attrs(
      graph,
      node_attr = "value",
      values = 5)

  # Expect that all nodes have the attribute set
  expect_true(
    all(graph_set_all$nodes_df$value == 5))

  # Select node `1` and apply a node attribute
  # using that node selection
  graph_node_selection <-
    graph %>%
    select_nodes(nodes = 1) %>%
    set_node_attrs_ws(node_attr = "value", value = 5)

  # Expect that node `1` has node attr set for `value`
  expect_equal(
    graph_node_selection$nodes_df[
      which(graph_node_selection$nodes_df[, 1] == 1), 4], 5)

  # Expect that getting the node attribute from a
  # selection works in the same way
  expect_equal(
    get_cache(
      cache_node_attrs_ws(
        graph_node_selection, node_attr = "value")), 5)

  # Get the node data frame from the graph as a separate object
  graph_node_df <- graph$nodes_df

  # Set attribute for named node `1` in the ndf
  graph_node_df_set_a <-
    set_node_attrs(
      graph_node_df,
      nodes = 1,
      node_attr = "value",
      values = 5)

  # Expect that node `1` has node attr set for `value`
  expect_equal(
    graph_node_df_set_a[
      which(graph_node_df_set_a[, 1] == 1), 4], 5)

  # Set attribute for named node `1` with a different value
  graph_node_df_set_a_node_attr_df <-
    set_node_attrs(
      graph_node_df_set_a,
      nodes = 1,
      node_attr = "value",
      values = 8)

  # Expect that node `1` in the ndf has node attr set for `value`
  expect_equal(
    graph_node_df_set_a_node_attr_df[
      which(graph_node_df_set_a_node_attr_df[, 1] == 1), 4], 8)

  # Set attribute for all nodes in the ndf
  graph_node_df_set_all <-
    set_node_attrs(
      graph_node_df,
      node_attr = "value",
      values = 5)

  # Expect that all nodes in the ndf will have the attribute set
  expect_true(all(graph_node_df_set_all$value == 5))

  # Expect that getting the node attribute from a graph without
  # a selection will result in an error
  expect_error(cache_node_attrs_ws(graph))

  # Expect an error if the attribute selected is `id`
  expect_error(
    set_node_attrs(
      graph,
      nodes = 1,
      node_attr = "id",
      values = "B"))

  # Expect an error if the length of `value` is greater than 1
  expect_error(
    set_node_attrs(
      graph,
      nodes = 1,
      node_attr = "value",
      values = c(1, 2)))
})

test_that("setting edge attributes is possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_path(8)

  # Set edge attribute for edge `1`->`2`
  graph_set_a_1 <-
    set_edge_attrs(
      graph,
      from = 1,
      to = 2,
      edge_attr = "value",
      values = 5)

  # Expect that edge `1`->`2` has edge attr set for `value`
  expect_equal(
    graph_set_a_1$edges_df[
      which(graph_set_a_1$edges_df$from == 1 &
              graph_set_a_1$edges_df$to == 2), 5], 5)

  # Get edge attribute for edge `1`->`2`
  graph_set_a_1_edge_attr <-
    get_cache(
      cache_edge_attrs(
        graph_set_a_1,
        edge_attr = "value",
        from = 1,
        to = 2))

  # Expect that edge `1`->`2` has edge attr set for `value`
  expect_equal(graph_set_a_1_edge_attr, 5)

  # Set attribute for named edge `1`->`2` with a different value
  graph_set_a_1 <-
    set_edge_attrs(
      graph_set_a_1,
      from = 1,
      to = 2,
      edge_attr = "value",
      values = 8)

  # Expect that edge `1`->`2` has edge attr set for `value`
  expect_equal(
    get_cache(
      cache_edge_attrs(
        graph_set_a_1,
        edge_attr = "value",
        from = 1,
        to = 2)), 8)

  # Select edge `1`->`2`
  graph_select_a_1 <- select_edges(graph, from = 1, to = 2)

  # Set attribute for selected edge `1`->`2`
  graph_select_a_1 <-
    set_edge_attrs_ws(
      graph_select_a_1,
      edge_attr = "value",
      value = 5)

  # Expect that edge `1`->`2` has edge attr set for `value`
  expect_equal(
    get_cache(
      cache_edge_attrs(
        graph_select_a_1,
        edge_attr = "value",
        from = 1,
        to = 2)), 5)

  # Set attribute for all edges
  graph_set_all <-
    set_edge_attrs(
      graph,
      edge_attr = "value",
      values = 5)

  # Expect that all edges have the attribute set
  expect_true(all(graph_set_all$edges_df$value == 5))

  # Select edge `1`->`2` and apply an edge attribute using that
  # edge selection
  graph_edge_selection <-
    graph %>%
    select_edges(from = 1, to = 2) %>%
    set_edge_attrs_ws(
      edge_attr = "value", value = 5)

  # Expect that edge `1`->`2` has edge attr set for `value`
  expect_equal(
    graph_edge_selection$edges_df[
      which(graph_edge_selection$edges_df$from == 1 &
              graph_edge_selection$edges_df$to == 2), 5], 5)
})

test_that("Getting node attributes is possible", {

  # Create a random graph with 4 nodes and 4 edges
  random_graph <-
    create_random_graph(
      n = 4, m = 4,
      set_seed = 23)

  # Get node attributes for all nodes with the
  # `value` attribute
  all_nodes <- get_node_attrs(random_graph, "value")

  # Expect a numeric vector
  expect_is(all_nodes, "numeric")

  # Expect the vector to have length 4
  expect_equal(length(all_nodes), 4)

  # Expect certain names to be in the vector
  expect_true(all(1:4 %in% names(all_nodes)))

  # Expect certain values to be in the vector
  expect_equal(all_nodes[[1]], 6.0)
  expect_equal(all_nodes[[2]], 2.5)
  expect_equal(all_nodes[[3]], 3.5)
  expect_equal(all_nodes[[4]], 7.5)

  # Get node attributes for nodes `1` and `3`
  nodes_1_3 <-
    get_node_attrs(
      random_graph,
      "value",
      nodes = c(1, 3))

  # Expect the vector to have length 2
  expect_equal(length(nodes_1_3), 2)

  # Expect certain names to be in the vector
  expect_true(all(c(1, 3) %in% names(nodes_1_3)))

  # Expect certain values to be in the vector
  expect_equal(nodes_1_3[[1]], 6.0)
  expect_equal(nodes_1_3[[2]], 3.5)
})

test_that("Getting edge attributes is possible", {

  # Create a simple graph where edges have an edge
  # attribute named `value`
  graph <-
    create_graph() %>%
    add_n_nodes(4) %>%
    {
      edges <-
        create_edge_df(
          from = c(1, 2, 1, 4),
          to = c(2, 3, 4, 3),
          rel = "rel")
      add_edge_df(., edges)
    } %>%
    set_edge_attrs(
      "value", 1.6, 1, 2) %>%
    set_edge_attrs(
      "value", 4.3, 1, 4) %>%
    set_edge_attrs(
      "value", 2.9, 2, 3) %>%
    set_edge_attrs(
      "value", 8.4, 4, 3)

  # Get node attributes for all nodes with the
  # `value` attribute
  all_edges <- get_edge_attrs(graph, "value")

  # Expect a numeric vector
  expect_is(all_edges, "numeric")

  # Expect the vector to have length 4
  expect_equal(length(all_edges), 4)

  # Expect certain names to be in the vector
  expect_true(
    all(c("1->2", "2->3", "1->4", "4->3") %in%
          names(all_edges)))

  # Expect certain values to be in the vector
  expect_equal(all_edges[[1]], 1.6)
  expect_equal(all_edges[[2]], 2.9)
  expect_equal(all_edges[[3]], 4.3)
  expect_equal(all_edges[[4]], 8.4)

  # Get only edge attribute values for specified
  # edges using the `from` and `to` arguments
  some_edges <-
    get_edge_attrs(
      graph,
      "value",
      c(1, 2), c(2, 3))

  # Expect the vector to have length 2
  expect_equal(length(some_edges), 2)

  # Expect certain names to be in the vector
  expect_true(
    all(c("1->2", "2->3") %in%
          names(some_edges)))

  # Expect certain values to be in the vector
  expect_equal(some_edges[[1]], 1.6)
  expect_equal(some_edges[[2]], 2.9)
})
