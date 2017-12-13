context("Copying attribute values as part of a traversal")

test_that("copying values with `trav_out_edge()` works", {

  # Create a node data frame
  ndf <-
    tibble::tribble(
      ~id, ~type, ~label, ~data,
      1,   "A",    "1",   6.0,
      2,   "A",    "2",   2.5,
      3,   "A",    "3",   3.5,
      4,   "A",    "4",   7.5,
      5,   "A",    "5",   8.5,
      6,   "A",    "6",   4.5,
      7,   "A",    "7",  10.0,
      8,   "A",    "8",  10.0,
      9,   "A",    "9",   8.5,
      10,   "A",   "10",  10.0)

  # Create an edge data frame
  edf <-
    tibble::tribble(
      ~id, ~from, ~to, ~rel,
      1,     6,  10,   NA,
      2,     5,   6,   NA,
      3,     2,  10,   NA,
      4,     2,   7,   NA,
      5,     5,  10,   NA,
      6,     1,   7,   NA,
      7,     3,   7,   NA,
      8,     3,   9,   NA,
      9,     7,   9,   NA,
      10,     6,   9,   NA,
      11,     1,   2,   NA,
      12,     1,   3,  "A",
      13,     2,   4,  "B",
      14,     2,   5,  "C",
      15,     3,   5,  "D")

  # Create a graph using the `ndf`
  # and `edf` tables
  graph <-
    create_graph(
      nodes_df = ndf,
      edges_df = edf)

  # Select node ID `3`, traverse to out edges and copy
  # `data` value from ndf (3.5) to edges traversed to
  graph_1 <-
    graph %>%
    select_nodes_by_id(nodes = 3) %>%
    trav_out_edge(copy_attrs_from = data)

  # Expect that there are 5 columns in the edf
  expect_equal(
    ncol(graph_1$edges_df), 5)

  # Expect that the `data` column in the last column in
  # the edge data frame
  expect_equal(
    colnames(graph_1$edges_df)[length(colnames(graph_1$edges_df))],
    "data")

  # Expect certain values in the `data` column of the
  # edge data frame
  expect_equal(
    graph_1$edges_df$data,
    c(NA, NA, NA, NA, NA, NA,
      3.5, 3.5, NA, NA, NA, NA,
      NA, NA, 3.5))

  #
  # Test where there are no common column names
  #

  # Set a seed
  set.seed(23)

  # Create a simple graph
  graph <-
    create_random_graph(
      n = 10, m = 10,
      directed = TRUE) %>%
    add_edges_w_string(
      edges = "1->2 1->3 2->4 2->5 3->5",
      rel = c(NA, "A", "B", "C", "D")) %>%
    rename_node_attrs(
      node_attr_from = value,
      node_attr_to = data)

  # Create a data frame with node ID values
  # representing the graph edges (with `from`
  # and `to` columns), and, a set of numeric values
  df <-
    data.frame(
      from = c(1, 1, 2, 2, 3),
      to = c(2, 3, 4, 5, 5))

  # Join the data frame to the graph's internal
  # edge data frame (edf)
  graph_2 <-
    graph %>%
    join_edge_attrs(
      df = df)

  # Select node ID `3`, traverse to in/out edges,
  # copy `data` value from ndf (3.5) to edges traversed to
  graph_2 <-
    graph_2 %>%
    select_nodes_by_id(nodes = 3) %>%
    trav_out_edge(copy_attrs_from = data)

  # Expect that there are 5 columns in the edf
  expect_equal(
    ncol(graph_2$edges_df), 5)

  # Expect that the `data` column in the last column in
  # the edge data frame
  expect_equal(
    colnames(graph_2$edges_df)[length(colnames(graph_2$edges_df))],
    "data")

  # Expect certain values in the `data` column of the
  # edge data frame
  expect_equal(
    graph_2$edges_df$data,
    c(NA, NA, NA, NA, NA, NA, NA,
      3.5, 3.5, 3.5, NA, NA, NA, NA, NA))
})

test_that("copying values with `trav_in_edge()` works", {

  # Set a seed
  set.seed(23)

  # Create a simple graph
  graph <-
    create_random_graph(
      n = 10, m = 10,
      directed = TRUE) %>%
    add_edges_w_string(
      edges = "1->2 1->3 2->4 2->5 3->5",
      rel = c(NA, "A", "B", "C", "D"))

  # Create a data frame with node ID values
  # representing the graph edges (with `from`
  # and `to` columns), and, a set of numeric values
  df <-
    data.frame(
      from = c(1, 1, 2, 2, 3),
      to = c(2, 3, 4, 5, 5),
      data = round(rnorm(5, 5), 2))

  # Join the data frame to the graph's internal
  # edge data frame (edf)
  graph_1 <-
    graph %>%
    join_edge_attrs(
      df = df)

  # Change the `value` column in ndf to `data` (both
  # ndf and edf have common column names now)
  graph_1 <-
    graph_1 %>%
    rename_node_attrs("value", "data")

  # Select node ID `3`, traverse to out edges and copy
  # `data` value from ndf (3.5) to edges traversed to
  graph_1 <-
    graph_1 %>%
    select_nodes_by_id(nodes = 3) %>%
    trav_in_edge(copy_attrs_from = data)

  # Expect that there are 5 columns in the edf
  expect_equal(
    ncol(graph_1$edges_df), 5)

  # Expect that the `data` column in the last column in
  # the edge data frame
  expect_equal(
    colnames(graph_1$edges_df)[length(colnames(graph_1$edges_df))],
    "data")

  # Expect certain values in the `data` column of the
  # edge data frame
  expect_equal(
    graph_1$edges_df$data,
    c(3.78, 3.50, NA, 4.48, 4.56, NA, NA,
      4.40, NA, NA, NA, NA, NA, NA, NA))

  #
  # Test where there are no common column names
  #

  # Set a seed
  set.seed(23)

  # Create a simple graph
  graph <-
    create_random_graph(
      n = 10, m = 10,
      directed = TRUE) %>%
    add_edges_w_string(
      edges = "1->2 1->3 2->4 2->5 3->5",
      rel = c(NA, "A", "B", "C", "D")) %>%
    rename_node_attrs(
      node_attr_from = value,
      node_attr_to = data)

  # Create a data frame with node ID values
  # representing the graph edges (with `from`
  # and `to` columns), and, a set of numeric values
  df <-
    data.frame(
      from = c(1, 1, 2, 2, 3),
      to = c(2, 3, 4, 5, 5))

  # Join the data frame to the graph's internal
  # edge data frame (edf)
  graph_2 <-
    graph %>%
    join_edge_attrs(
      df = df)

  # Select node ID `3`, traverse to in/out edges,
  # copy `data` value from ndf (3.5) to edges traversed to
  graph_2 <-
    graph_2 %>%
    select_nodes_by_id(nodes = 3) %>%
    trav_in_edge(copy_attrs_from = data)

  # Expect that there are 5 columns in the edf
  expect_equal(
    ncol(graph_2$edges_df), 5)

  # Expect that the `data` column in the last column in
  # the edge data frame
  expect_equal(
    colnames(graph_2$edges_df)[length(colnames(graph_2$edges_df))],
    "data")

  # Expect certain values in the `data` column of the
  # edge data frame
  expect_equal(
    graph_2$edges_df$data,
    c(NA, 3.5, NA, NA, NA, NA, NA, NA,
      NA, NA, NA, NA, NA, NA, NA))
})

test_that("copying values with `trav_both_edge()` works", {

  # Set a seed
  set.seed(23)

  # Create a simple graph
  graph <-
    create_random_graph(
      n = 10, m = 10,
      directed = TRUE) %>%
    add_edges_w_string(
      edges = "1->2 1->3 2->4 2->5 3->5",
      rel = c(NA, "A", "B", "C", "D"))

  # Create a data frame with node ID values
  # representing the graph edges (with `from`
  # and `to` columns), and, a set of numeric values
  df <-
    data.frame(
      from = c(1, 1, 2, 2, 3),
      to = c(2, 3, 4, 5, 5),
      data = round(rnorm(5, 5), 2))

  # Join the data frame to the graph's internal
  # edge data frame (edf)
  graph_1 <-
    graph %>%
    join_edge_attrs(
      df = df)

  # Change the `value` column in ndf to `data` (both
  # ndf and edf have common column names now)
  graph_1 <-
    graph_1 %>%
    rename_node_attrs(
      node_attr_from = value,
      node_attr_to = data)

  # Select node ID `3`, traverse to in/out edges,
  # copy `data` value from ndf (3.5) to edges traversed to
  graph_1 <-
    graph_1 %>%
    select_nodes_by_id(nodes = 3) %>%
    trav_both_edge(copy_attrs_from = data)

  # Expect that there are 5 columns in the edf
  expect_equal(
    ncol(graph_1$edges_df), 5)

  # Expect that the `data` column in the last column in
  # the edge data frame
  expect_equal(
    colnames(graph_1$edges_df)[length(colnames(graph_1$edges_df))],
    "data")

  # Expect certain values in the `data` column of the
  # edge data frame
  expect_equal(
    graph_1$edges_df$data,
    c(NA, NA, NA, NA, NA, NA, 3.50, 3.50,
      NA, NA, 3.78, 8.81, 4.48, 4.56, 7.90))

  #
  # Test where there are no common column names
  #

  # Set a seed
  set.seed(23)

  # Create a simple graph
  graph <-
    create_random_graph(
      n = 10, m = 10,
      directed = TRUE) %>%
    add_edges_w_string(
      edges = "1->2 1->3 2->4 2->5 3->5",
      rel = c(NA, "A", "B", "C", "D")) %>%
    rename_node_attrs(
      node_attr_from = value,
      node_attr_to = data)

  # Create a data frame with node ID values
  # representing the graph edges (with `from`
  # and `to` columns), and, a set of numeric values
  df <-
    data.frame(
      from = c(1, 1, 2, 2, 3),
      to = c(2, 3, 4, 5, 5))

  # Join the data frame to the graph's internal
  # edge data frame (edf)
  graph_2 <-
    graph %>%
    join_edge_attrs(
      df = df)

  # Select node ID `3`, traverse to in/out edges,
  # copy `data` value from ndf (3.5) to edges traversed to
  graph_2 <-
    graph_2 %>%
    select_nodes_by_id(nodes = 3) %>%
    trav_both_edge(copy_attrs_from = data)

  # Expect that there are 5 columns in the edf
  expect_equal(
    ncol(graph_2$edges_df), 5)

  # Expect that the `data` column in the last column in
  # the edge data frame
  expect_equal(
    colnames(graph_2$edges_df)[length(colnames(graph_2$edges_df))],
    "data")

  # Expect certain values in the `data` column of the
  # edge data frame
  expect_equal(
    graph_2$edges_df$data,
    c(NA, 3.5, NA, NA, NA, NA, NA, 3.5, 3.5,
      3.5, NA, NA, NA, NA, NA))
})

test_that("copying values with `trav_both()` works", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_path(n = 5) %>%
    select_nodes_by_id(nodes = c(2, 4)) %>%
    set_node_attrs_ws(
      node_attr = value,
      value = 5)

  # Expect that specific values will be copied
  # from selected nodes and passed to adjacent nodes
  expect_equal(
    graph %>%
      trav_both(
        copy_attrs_from = value,
        agg = "sum") %>%
      get_node_attrs(node_attr = value) %>%
      as.numeric(),
    c(5, 5, 10, 5, 5))
})

test_that("copying values with `trav_in()` works", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node() %>%
    select_nodes() %>%
    add_n_nodes_ws(
      n = 2,
      direction = "from") %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 2:3) %>%
    set_node_attrs_ws(
      node_attr = value,
      value = 5)

  # Expect that specific values will be copied
  # from selected nodes and passed to adjacent nodes
  expect_equal(
    graph %>%
      trav_in(
        copy_attrs_from = value,
        agg = "sum") %>%
      get_node_attrs(node_attr = value) %>%
      as.numeric(),
    c(10, 5, 5))
})

test_that("copying values with `trav_out()` works", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node() %>%
    select_nodes() %>%
    add_n_nodes_ws(
      n = 2,
      direction = "to") %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 2:3) %>%
    set_node_attrs_ws(
      node_attr = value,
      value = 5)

  # Expect that specific values will be copied
  # from selected nodes and passed to adjacent nodes
  expect_equal(
    graph %>%
      trav_out(
        copy_attrs_from = value,
        agg = "sum") %>%
      get_node_attrs(node_attr = value) %>%
      as.numeric(),
    c(10, 5, 5))
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
