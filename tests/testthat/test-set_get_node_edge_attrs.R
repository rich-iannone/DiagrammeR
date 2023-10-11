# Setting and getting node attributes

test_that("setting node attributes is possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_path(n = 8)

  # Set attribute for node `1`
  graph_set_a <-
    set_node_attrs(
      graph = graph,
      nodes = 1,
      node_attr = value,
      values = 5)

  # Expect that node `1` has node attr set for `value`
  expect_equal(
    graph_set_a$nodes_df[
      which(graph_set_a$nodes_df$id == 1), 4], 5)

  # Set attribute for node `1` with a different value
  graph_set_a <-
    set_node_attrs(
      graph = graph,
      nodes = 1,
      node_attr = value,
      values = 8)

  # Expect that node `1` has node attr set for `value`
  expect_equal(
    graph_set_a$nodes_df[
      which(graph_set_a$nodes_df$id == 1), 4], 8)

  # Select node `1`
  graph_select_a <-
    select_nodes(
      graph = graph,
      nodes = 1)

  # Set attribute for selected node `1`
  graph_select_a <-
    set_node_attrs_ws(
      graph = graph_select_a,
      node_attr = value,
      value = 5)

  # Expect that node `1` has node attr set for `value`
  expect_equal(
    graph_select_a$nodes_df[
      which(graph_select_a$nodes_df$id == 1), 4], 5)

  # Set attribute for all nodes
  graph_set_all <-
    set_node_attrs(
      graph = graph,
      node_attr = value,
      values = 5)

  # Expect that all nodes have the attribute set
  expect_in(graph_set_all$nodes_df$value, 5)

  # Select node `1` and apply a node attribute
  # using that node selection
  graph_node_selection <-
    graph %>%
    select_nodes(nodes = 1) %>%
    set_node_attrs_ws(
      node_attr = value,
      value = 5)

  # Expect that node `1` has node attr set for `value`
  expect_equal(
    graph_node_selection$nodes_df[
      which(graph_node_selection$nodes_df[, 1] == 1), 4], 5)

  # Expect an error if the length of `value` is greater than 1
  expect_error(
    set_node_attrs(
      graph = graph,
      nodes = 1,
      node_attr = value,
      values = c(1, 2)))
})

test_that("setting edge attributes is possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_path(n = 8)

  # Set edge attribute for edge `1`->`2`
  graph_set_a_1 <-
    set_edge_attrs(
      graph = graph,
      from = 1,
      to = 2,
      edge_attr = value,
      values = 5)

  # Expect that edge `1`->`2` has edge attr set for `value`
  expect_equal(
    graph_set_a_1$edges_df[
      which(graph_set_a_1$edges_df$from == 1 &
              graph_set_a_1$edges_df$to == 2), 5], 5)

  # Set attribute for named edge `1`->`2` with a different value
  graph_set_a_1 <-
    set_edge_attrs(
      graph = graph_set_a_1,
      from = 1,
      to = 2,
      edge_attr = value,
      values = 8)

  # Expect that edge `1`->`2` has edge attr set for `value`
  expect_equal(
    graph_set_a_1$edges_df[
      which(graph_set_a_1$edges_df$from == 1 &
              graph_set_a_1$edges_df$to == 2), 5], 8)

  # Select edge `1`->`2`
  graph_select_a_1 <-
    select_edges(
      graph = graph,
      from = 1,
      to = 2)

  # Set attribute for selected edge `1`->`2`
  graph_select_a_1 <-
    set_edge_attrs_ws(
      graph = graph_select_a_1,
      edge_attr = value,
      value = 5)

  # Expect that edge `1`->`2` has edge attr set for `value`
  expect_equal(
    graph_select_a_1$edges_df[
      which(graph_select_a_1$edges_df$from == 1 &
              graph_select_a_1$edges_df$to == 2), 5], 5)

  # Set attribute for all edges
  graph_set_all <-
    set_edge_attrs(
      graph = graph,
      edge_attr = value,
      values = 5)

  # Expect that all edges have the attribute set
  expect_in(graph_set_all$edges_df$value, 5)

  # Select edge `1`->`2` and apply an edge attribute using that
  # edge selection
  graph_edge_selection <-
    graph %>%
    select_edges(
      from = 1,
      to = 2) %>%
    set_edge_attrs_ws(
      edge_attr = value,
      value = 5)

  # Expect that edge `1`->`2` has edge attr set for `value`
  expect_equal(
    graph_edge_selection$edges_df[
      which(graph_edge_selection$edges_df$from == 1 &
              graph_edge_selection$edges_df$to == 2), 5], 5)
})

test_that("Getting node attributes is possible", {

  # Create a randomized graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 4,
      m = 4,
      node_data = node_data(
        value = 1:4),
      set_seed = 23)

  # Get node attributes for all nodes with the
  # `value` attribute
  all_nodes <-
    get_node_attrs(
      graph = graph,
      node_attr = value)

  # Expect a vector of integer values (length 4)
  expect_type(all_nodes, "integer")
  expect_length(all_nodes, 4)
  # Expect certain names to be in the vector

  expect_named(all_nodes, as.character(1:4))

  # Expect certain values to be in the vector
  expect_equal(
    all_nodes[[1]], 1)

  expect_equal(
    all_nodes[[2]], 2)

  expect_equal(
    all_nodes[[3]], 3)

  expect_equal(
    all_nodes[[4]], 4)

  # Get node attributes for nodes `1` and `3`
  nodes_1_3 <-
    get_node_attrs(
      graph = graph,
      node_attr = value,
      nodes = c(1, 3))

  # Expect the vector to have length 2 (named 1, 3)
  expect_length(nodes_1_3, 2)
  expect_named(nodes_1_3, c("1", "3"))

  # Expect certain values to be in the vector
  expect_equal(
    nodes_1_3[[1]], 1)

  expect_equal(
    nodes_1_3[[2]], 3)

  # Expect an error if referencing `id`
  expect_error(
    get_node_attrs(
      graph = graph,
      node_attr = id))
})

test_that("Getting edge attributes is possible", {

  # Create a simple graph where edges have an edge
  # attribute named `value`
  graph <-
    create_graph() %>%
    add_n_nodes(n = 4) %>%
    {
      edges <-
        create_edge_df(
          from = c(1, 2, 1, 4),
            to = c(2, 3, 4, 3),
           rel = "rel")
      add_edge_df(
        graph = .,
        edge_df = edges)
    } %>%
    set_edge_attrs(
      edge_attr = value,
      values = 1.6,
      from = 1,
        to = 2) %>%
    set_edge_attrs(
      edge_attr = value,
      values = 4.3,
      from = 1,
        to = 4) %>%
    set_edge_attrs(
      edge_attr = value,
      values = 2.9,
      from = 2,
        to = 3) %>%
    set_edge_attrs(
      edge_attr = value,
      values = 8.4,
      from = 4,
        to = 3)

  # Get node attributes for all nodes with the
  # `value` attribute
  all_edges <-
    get_edge_attrs(
      graph = graph,
      edge_attr = value)

  # Expect a numeric vector
  expect_type(
    all_edges, "double")

  # Expect the vector to have length 4
  expect_length(all_edges, 4)

  # Expect certain names to be in the vector
  expect_contains(
    names(all_edges),
    c("1->2", "2->3", "1->4", "4->3")
    )

  # Expect certain values to be in the vector
  expect_equal(
    all_edges[[1]], 1.6)

  expect_equal(
    all_edges[[2]], 2.9)

  expect_equal(
    all_edges[[3]], 4.3)

  expect_equal(
    all_edges[[4]], 8.4)

  # Get only edge attribute values for specified
  # edges using the `from` and `to` arguments
  some_edges <-
    get_edge_attrs(
      graph = graph,
      edge_attr = value,
      from = c(1, 2),
        to = c(2, 3))

  # Expect the vector to have length 2
  expect_length(some_edges, 2)

  # Expect certain names to be in the vector
  expect_contains(
    names(some_edges),
    c("1->2", "2->3")
  )

  # Expect certain values to be in the vector
  expect_equal(
    some_edges[[1]], 1.6)

  expect_equal(
    some_edges[[2]], 2.9)

  # Expect an error if referencing `from`
  expect_error(
    get_edge_attrs(
      graph = graph,
      edge_attr = from))

  # Expect an error if referencing `to`
  expect_error(
    get_edge_attrs(
      graph = graph,
      edge_attr = to))

  # Expect an error if unequal vector lengths
  # provided for `from` and `to`
  expect_error(
    get_edge_attrs(
      graph = graph,
      edge_attr = value,
      from = c(1, 2),
        to = 2))
})

test_that("Getting node attributes with a selection is possible", {

  # Create a random graph and
  # select nodes `1` and `3`
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 4, m = 4,
      node_data = node_data(value = 1:4),
      set_seed = 23
    ) %>%
    select_nodes_by_id(nodes = c(1, 3))

  # Get node attributes for all
  # selected nodes with the
  # `value` attribute
  nodes_1_3 <-
    get_node_attrs_ws(
      graph = graph,
      node_attr = value)

  # Expect a vector of integer values
  expect_type(nodes_1_3, "integer")

  # Expect the vector to have length 2
  expect_length(nodes_1_3, 2)

  # Expect certain names to be in the vector
  expect_named(
    nodes_1_3,
    c("1", "3")
  )

  # Expect certain values to be in the vector
  expect_equal(
    nodes_1_3[[1]], 1)

  expect_equal(
    nodes_1_3[[2]], 3)

  # Expect an error if there is no
  # node selection
  expect_error(
    get_node_attrs_ws(
      graph = graph %>%
        clear_selection(),
      node_attr = value))

  # Expect an error if referencing
  # `id` column
  expect_error(
    get_node_attrs_ws(
      graph = graph,
      node_attr = id))
})

test_that("Getting edge attributes with a selection is possible", {

  # Create a simple graph where
  # edges have an edge attribute
  # named `value`; select the edges
  # defined as `1`->`3` and `2`->`3`
  graph <-
    create_graph() %>%
    add_n_nodes(n = 4) %>%
    {
      edges <-
        create_edge_df(
          from = c(1, 2, 1, 4),
          to = c(2, 3, 4, 3),
          rel = "rel")
      add_edge_df(
        graph = .,
        edge_df = edges)
    } %>%
    set_edge_attrs(
      edge_attr = value,
      values = 1.6,
      from = 1,
        to = 2) %>%
    set_edge_attrs(
      edge_attr = value,
      values = 4.3,
      from = 1,
        to = 4) %>%
    set_edge_attrs(
      edge_attr = value,
      values = 2.9,
      from = 2,
        to = 3) %>%
    set_edge_attrs(
      edge_attr = value,
      values = 8.4,
      from = 4,
        to = 3) %>%
    select_edges(
      from = c(1, 2),
      to = c(2, 3))

  # Get edge attributes for all
  # selected edge with the
  # `value` attribute
  edge_attr_values <-
    graph %>%
    get_edge_attrs_ws(
      edge_attr = value)

  # Expect a numeric vector
  expect_type(
    edge_attr_values, "double")

  # Expect the vector to have length 2
  expect_length(edge_attr_values, 2)

  # Expect certain names to be in the vector
  # expect_contains == expect_true(all(expected %in% object))
  expect_named(
    edge_attr_values,
    c("1->2", "2->3"),
    ignore.order = TRUE
    )

  # Expect certain values to be in the vector
  expect_equal(
    edge_attr_values[[1]], 1.6)

  expect_equal(
    edge_attr_values[[2]], 2.9)

  # Expect an error if there is no
  # edge selection
  expect_error(
    get_edge_attrs_ws(
      graph = graph %>%
        clear_selection(),
      edge_attr = value))
})
