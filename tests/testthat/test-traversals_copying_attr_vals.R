context("Copying attribute values as part of a traversal")

test_that("copying values with `trav_out_edge()` works", {

  # Create a graph with a path
  graph <-
    create_graph() %>%
    add_path(
      n = 3,
      node_data = node_data(
        data = c(1.0, 2.0, 3.0)),
      edge_data = edge_data(
        data = c(1.5, 2.5)))

  # Select node ID `1`, traverse to
  # outward edges and copy the `data`
  # data from nodes to edges traversed to
  graph <-
    graph %>%
    select_nodes_by_id(nodes = 1) %>%
    trav_out_edge(copy_attrs_from = data)

  # Expect that there are 5 columns in the edf
  expect_equal(
    ncol(graph$edges_df), 5)

  # Expect that the `data` column is
  # the last column in edf
  expect_equal(
    colnames(graph$edges_df)[length(colnames(graph$edges_df))],
    "data")

  # Expect certain values in the
  # `value` column of the edf
  expect_equal(
    graph$edges_df$data,
    c(1.0, 2.5))

  #
  # Test where there are no common column names
  #

  # Create a graph with a path
  graph <-
    create_graph() %>%
    add_path(
      n = 3,
      node_data = node_data(
        node_value = c(1.0, 2.0, 3.0)),
      edge_data = edge_data(
        edge_value = c(1.5, 2.5)))

  # Select node ID `1`, traverse to
  # outward edges, copy value from
  # ndf to edf
  graph <-
    graph %>%
    select_nodes_by_id(nodes = 1) %>%
    trav_out_edge(copy_attrs_from = node_value)

  # Expect that there are 6 columns in the edf
  expect_equal(
    ncol(graph$edges_df), 6)

  # Expect that the `edge_value`
  # column in the last column in
  # the edge data frame
  expect_equal(
    colnames(graph$edges_df)[length(colnames(graph$edges_df))],
    "edge_value")

  # Expect certain values in the `node_value` column of the
  # edge data frame
  expect_equal(
    graph$edges_df$node_value,
    c(1, NA))
})


test_that("copying values with `trav_in_edge()` works", {

  # Create a graph with a path
  graph <-
    create_graph() %>%
    add_path(
      n = 3,
      node_data = node_data(
        data = c(1.0, 2.0, 3.0)),
      edge_data = edge_data(
        data = c(1.5, 2.5)))

  # Select node ID `3`, traverse to
  # inward edges and copy the `data`
  # data from nodes to edges traversed to
  graph <-
    graph %>%
    select_nodes_by_id(nodes = 3) %>%
    trav_in_edge(copy_attrs_from = data)

  # Expect that there are 5 columns in the edf
  expect_equal(
    ncol(graph$edges_df), 5)

  # Expect that the `data` column is
  # the last column in edf
  expect_equal(
    colnames(graph$edges_df)[length(colnames(graph$edges_df))],
    "data")

  # Expect certain values in the
  # `value` column of the edf
  expect_equal(
    graph$edges_df$data,
    c(1.5, 3.0))

  #
  # Test where there are no common column names
  #

  # Create a graph with a path
  graph <-
    create_graph() %>%
    add_path(
      n = 3,
      node_data = node_data(
        node_value = c(1.0, 2.0, 3.0)),
      edge_data = edge_data(
        edge_value = c(1.5, 2.5)))

  # Select node ID `3`, traverse to
  # outward edges, copy value from
  # ndf to edf
  graph <-
    graph %>%
    select_nodes_by_id(nodes = 3) %>%
    trav_in_edge(copy_attrs_from = node_value)

  # Expect that there are 6 columns in the edf
  expect_equal(
    ncol(graph$edges_df), 6)

  # Expect that the `edge_value`
  # column in the last column in
  # the edge data frame
  expect_equal(
    colnames(graph$edges_df)[length(colnames(graph$edges_df))],
    "edge_value")

  # Expect certain values in the `node_value` column of the
  # edge data frame
  expect_equal(
    graph$edges_df$node_value,
    c(NA, 3))
})


test_that("copying values with `trav_both_edge()` works", {

  # Create a graph with a path
  graph <-
    create_graph() %>%
    add_path(
      n = 3,
      node_data = node_data(
        data = c(1.0, 2.0, 3.0)),
      edge_data = edge_data(
        data = c(1.5, 2.5)))

  # Select node ID `2`, traverse to
  # inward/outward edges and copy the `data`
  # data from nodes to edges traversed to
  graph <-
    graph %>%
    select_nodes_by_id(nodes = 2) %>%
    trav_both_edge(copy_attrs_from = data)

  # Expect that there are 5 columns in the edf
  expect_equal(
    ncol(graph$edges_df), 5)

  # Expect that the `data` column is
  # the last column in edf
  expect_equal(
    colnames(graph$edges_df)[length(colnames(graph$edges_df))],
    "data")

  # Expect certain values in the
  # `value` column of the edf
  expect_equal(
    graph$edges_df$data,
    c(3.5, 4.5))

  #
  # Test where there are no common column names
  #

  # Create a graph with a path
  graph <-
    create_graph() %>%
    add_path(
      n = 3,
      node_data = node_data(
        node_value = c(1.0, 2.0, 3.0)),
      edge_data = edge_data(
        edge_value = c(1.5, 2.5)))

  # Select node ID `2`, traverse to
  # inward/outward edges, copy value from
  # ndf to edf
  graph <-
    graph %>%
    select_nodes_by_id(nodes = 2) %>%
    trav_both_edge(copy_attrs_from = node_value)

  # Expect that there are 6 columns in the edf
  expect_equal(
    ncol(graph$edges_df), 6)

  # Expect that the `edge_value`
  # column in the last column in
  # the edge data frame
  expect_equal(
    colnames(graph$edges_df)[length(colnames(graph$edges_df))],
    "edge_value")

  # Expect certain values in the `node_value` column of the
  # edge data frame
  expect_equal(
    graph$edges_df$node_value,
    c(2, 2))
})


test_that("copying values with `trav_both()` works", {

  # Create a graph with a path
  graph <-
    create_graph() %>%
    add_path(
      n = 3,
      node_data = node_data(
        data = c(1.0, 2.0, 3.0)),
      edge_data = edge_data(
        data = c(1.5, 2.5)))

  # Select node ID `2`, traverse to
  # inward/outward nodes and copy the `data`
  # data from nodes to edges traversed to
  graph <-
    graph %>%
    select_nodes_by_id(nodes = 2) %>%
    trav_both(copy_attrs_from = data)

  # Expect certain values in the `node_value` column of the
  # edge data frame
  expect_equal(
    graph$nodes_df$data,
    c(2, 2, 2))
})

test_that("copying values with `trav_in()` works", {

  # Create a graph with a path
  graph <-
    create_graph() %>%
    add_path(
      n = 3,
      node_data = node_data(
        data = c(1.0, 2.0, 3.0)),
      edge_data = edge_data(
        data = c(1.5, 2.5)))

  # Select node ID `3`, traverse to
  # inward/outward edges and copy the `data`
  # data from nodes to edges traversed to
  graph <-
    graph %>%
    select_nodes_by_id(nodes = 3) %>%
    trav_in(copy_attrs_from = data)

  # Expect that there are 4 columns in the edf
  expect_equal(
    ncol(graph$nodes_df), 4)

  # Expect that the `data` column is
  # the last column in ndf
  expect_equal(
    colnames(graph$nodes_df)[length(colnames(graph$nodes_df))],
    "data")

  # Expect certain values in the
  # `value` column of the edf
  expect_equal(
    graph$nodes_df$data,
    c(1, 3, 3))
})

test_that("copying values with `trav_out()` works", {

  # Create a graph with a path
  graph <-
    create_graph() %>%
    add_path(
      n = 3,
      node_data = node_data(
        data = c(1.0, 2.0, 3.0)),
      edge_data = edge_data(
        data = c(1.5, 2.5)))

  # Select node ID `1`, traverse to
  # outward nodes and copy the `data`
  # data to nodes traversed to
  graph <-
    graph %>%
    select_nodes_by_id(nodes = 1) %>%
    trav_out(copy_attrs_from = data)

  # Expect that there are 4 columns in the ndf
  expect_equal(
    ncol(graph$nodes_df), 4)

  # Expect that the `data` column is
  # the last column in ndf
  expect_equal(
    colnames(graph$nodes_df)[length(colnames(graph$nodes_df))],
    "data")

  # Expect certain values in the
  # `value` column of the edf
  expect_equal(
    graph$nodes_df$data,
    c(1, 1, 3))
})


test_that("copying values with `trav_out_node()` works", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node() %>%
    select_nodes() %>%
    add_n_nodes_ws(
      n = 2,
      direction = "from") %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 2) %>%
    set_node_attrs_ws(
      node_attr = value,
      value = 8) %>%
    clear_selection() %>%
    select_edges_by_edge_id(edges = 1) %>%
    set_edge_attrs_ws(
      edge_attr = value,
      value = 5) %>%
    clear_selection() %>%
    select_edges_by_edge_id(edges = 2) %>%
    set_edge_attrs_ws(
      edge_attr = value,
      value = 5) %>%
    clear_selection() %>%
    select_edges()

  # Expect that specific values will be copied
  # from selected nodes and passed to adjacent nodes
  expect_equal(
    graph %>%
      trav_out_node(
        copy_attrs_from = value,
        agg = "sum") %>%
      get_node_attrs(node_attr = value) %>%
      as.numeric(),
    c(10, 8, NA))
})

test_that("copying values with `trav_in_node()` works", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node() %>%
    select_nodes() %>%
    add_n_nodes_ws(
      n = 2,
      direction = "to") %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 2) %>%
    set_node_attrs_ws(
      node_attr = value,
      value = 8) %>%
    clear_selection() %>%
    select_edges_by_edge_id(edges = 1) %>%
    set_edge_attrs_ws(
      edge_attr = value,
      value = 5) %>%
    clear_selection() %>%
    select_edges_by_edge_id(edges = 2) %>%
    set_edge_attrs_ws(
      edge_attr = value,
      value = 5) %>%
    clear_selection() %>%
    select_edges()

  # Expect that specific values will be copied
  # from selected nodes and passed to adjacent nodes
  expect_equal(
    graph %>%
      trav_in_node(
        copy_attrs_from = value,
        agg = "sum") %>%
      get_node_attrs(node_attr = value) %>%
      as.numeric(),
    c(10, 8, NA))
})
