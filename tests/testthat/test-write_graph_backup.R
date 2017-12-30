context("Write graph backups")

test_that("Graph backups for `add_balanced_tree()` works", {

  #
  # Backup from `add_balanced_tree()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_balanced_tree(
      k = 2, h = 2)

  expect_equal(
    list.files(path = path) %>% length(), 1)
})

test_that("Graph backups for `add_cycle()` works", {

  #
  # Backup from `add_cycle()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_cycle(n = 6)

  expect_equal(
    list.files(path = path) %>% length(), 2)
})

test_that("Graph backups for `add_edge()` works", {

  #
  # Backup from `add_edge()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_n_nodes(
      n = 2) %>%
    add_edge(
      from = 1,
      to = 2)

  expect_equal(
    list.files(path = path) %>% length(), 3)
})

test_that("Graph backups for `add_edge_clone()` works", {

  #
  # Backup from `add_edge_clone()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_n_nodes(
      n = 3) %>%
    add_edge(
      from = 1,
      to = 2) %>%
    add_edge_clone(
      edge = 1,
      from = 2,
      to = 3)

  expect_equal(
    list.files(path = path) %>% length(), 4)
})

test_that("Graph backups for `add_edge_clone()` works", {

  #
  # Backup from `add_edge_clone()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_n_nodes(
      n = 3) %>%
    add_edge(
      from = 1,
      to = 2) %>%
    add_edge_clone(
      edge = 1,
      from = 2,
      to = 3)

  expect_equal(
    list.files(path = path) %>% length(), 5)
})

test_that("Graph backups for `add_edge_df()` works", {

  #
  # Backup from `add_edge_df()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  ndf <-
    create_node_df(
      n = 4,
      type = "letter",
      color = c(
        "red", "green",
        "grey", "blue"),
      value = c(
        3.5, 2.6, 9.4, 2.7))

  edf <-
    create_edge_df(
      from = c(1, 2, 3),
      to = c(4, 3, 1),
      rel = "leading_to")

  create_graph(
    nodes_df = ndf,
    write_backups = TRUE) %>%
    add_edge_df(
      edge_df = edf)

  expect_equal(
    list.files(path = path) %>% length(), 6)
})

test_that("Graph backups for `add_edge_df()` works", {

  #
  # Backup from `add_edges_from_table()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_nodes_from_table(
      table = currencies) %>%
    add_edges_from_table(
      table = usd_exchange_rates,
      from_col = from_currency,
      to_col = to_currency,
      from_to_map = iso_4217_code)

  expect_equal(
    list.files(path = path) %>% length(), 7)
})

test_that("Graph backups for `add_edges_w_string()` works", {

  #
  # Backup from `add_edges_w_string()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_n_nodes(n = 4) %>%
    add_edges_w_string(
      edges = "1->2 1->3 2->4 2->3")

  expect_equal(
    list.files(path = path) %>% length(), 8)
})

test_that("Graph backups for `add_forward_edges_ws()` works", {

  #
  # Backup from `add_forward_edges_ws()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_n_nodes(
      n = 2,
      type = "type_a",
      label = c("a_1", "a_2")) %>%
    add_edge(
      from = 1, to = 2, rel = "a") %>%
    select_edges() %>%
    add_forward_edges_ws(rel = "b")

  expect_equal(
    list.files(path = path) %>% length(), 9)
})

test_that("Graph backups for `add_full_graph()` works", {

  #
  # Backup from `add_full_graph()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_full_graph(n = 5)

  expect_equal(
    list.files(path = path) %>% length(), 10)
})

test_that("Graph backups for `add_gnm_graph()` works", {

  #
  # Backup from `add_gnm_graph()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_gnm_graph(n = 100, m = 120)

  expect_equal(
    list.files(path = path) %>% length(), 11)
})

test_that("Graph backups for `add_gnp_graph()` works", {

  #
  # Backup from `add_gnp_graph()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_gnp_graph(n = 100, p = 0.05)

  expect_equal(
    list.files(path = path) %>% length(), 12)
})

test_that("Graph backups for `add_node_clones_ws()` works", {

  #
  # Backup from `add_node_clones_ws()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_gnm_graph(
      n = 10, m = 22) %>%
    select_nodes() %>%
    add_node_clones_ws()

  expect_equal(
    list.files(path = path) %>% length(), 13)
})

test_that("Graph backups for `add_n_node_clones()` works", {

  #
  # Backup from `add_n_node_clones()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_gnm_graph(
      n = 10, m = 22) %>%
    add_n_node_clones(
      n = 2,
      node = 1)

  expect_equal(
    list.files(path = path) %>% length(), 14)
})

test_that("Graph backups for `add_global_graph_attrs()` works", {

  #
  # Backup from `add_global_graph_attrs()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_global_graph_attrs(
      attr = "penwidth",
      value = 12,
      attr_type = "node")

  expect_equal(
    list.files(path = path) %>% length(), 15)
})

test_that("Graph backups for `delete_global_graph_attrs()` works", {

  #
  # Backup from `delete_global_graph_attrs()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    delete_global_graph_attrs(
      attr = "outputorder",
      attr_type = "graph")

  expect_equal(
    list.files(path = path) %>% length(), 16)
})

test_that("Graph backups for `clear_global_graph_attrs()` works", {

  #
  # Backup from `clear_global_graph_attrs()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    clear_global_graph_attrs()

  expect_equal(
    list.files(path = path) %>% length(), 17)
})

test_that("Graph backups for `add_graph_action()` works", {

  #
  # Backup from `add_graph_action()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_gnm_graph(
      n = 10, m = 22) %>%
    add_graph_action(
      fcn = "set_node_attr_w_fcn",
      node_attr_fcn = "get_betweenness",
      column_name = "btwns",
      action_name = "get_btwns")

  expect_equal(
    list.files(path = path) %>% length(), 18)
})

test_that("Graph backups for `delete_graph_actions()` works", {

  #
  # Backup from `delete_graph_actions()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_gnm_graph(
      n = 10, m = 22) %>%
    add_graph_action(
      fcn = "set_node_attr_w_fcn",
      node_attr_fcn = "get_betweenness",
      column_name = "btwns",
      action_name = "get_btwns") %>%
    delete_graph_actions(
      actions = "get_btwns")

  expect_equal(
    list.files(path = path) %>% length(), 19)
})

test_that("Graph backups for `reorder_graph_actions()` works", {

  #
  # Backup from `reorder_graph_actions()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_gnm_graph(
      n = 10, m = 22) %>%
    add_graph_action(
      fcn = "rescale_node_attrs",
      node_attr_from = "pagerank",
      node_attr_to = "width",
      action_name = "pgrnk_to_width") %>%
    add_graph_action(
      fcn = "set_node_attr_w_fcn",
      node_attr_fcn = "get_pagerank",
      column_name = "pagerank",
      action_name = "get_pagerank") %>%
    reorder_graph_actions(
      indices = c(2, 1))

  expect_equal(
    list.files(path = path) %>% length(), 20)
})

test_that("Graph backups for `create_complement_graph()` works", {

  #
  # Backup from `create_complement_graph()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_cycle(n = 4) %>%
    create_complement_graph()

  expect_equal(
    list.files(path = path) %>% length(), 21)
})

test_that("Graph backups for `copy_node_attrs()` works", {

  #
  # Backup from `copy_node_attrs()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_path(
      n = 4,
      node_aes = node_aes(
        color = "blue")) %>%
    copy_node_attrs(
      node_attr_from = color,
      node_attr_to = color_2)

  expect_equal(
    list.files(path = path) %>% length(), 22)
})

test_that("Graph backups for `copy_edge_attrs()` works", {

  #
  # Backup from `copy_edge_attrs()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_path(
      n = 4,
      edge_aes = edge_aes(
        color = "blue")) %>%
    copy_edge_attrs(
      edge_attr_from = color,
      edge_attr_to = color_2)

  expect_equal(
    list.files(path = path) %>% length(), 23)
})

test_that("Graph backups for `colorize_node_attrs()` works", {

  #
  # Backup from `colorize_node_attrs()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  graph <-
    create_graph(write_backups = TRUE) %>%
    add_path(
      n = 8,
      node_data = node_data(
        weight = c(
          8.2, 3.7, 6.3, 9.2,
          1.6, 2.5, 7.2, 5.4))) %>%
    colorize_node_attrs(
      node_attr_from = weight,
      node_attr_to = fillcolor,
      palette = "Greens",
      cut_points = c(1, 3, 5, 7, 9),
      alpha = 90)

  expect_equal(
    list.files(path = path) %>% length(), 24)
})

test_that("Graph backups for `colorize_edge_attrs()` works", {

  #
  # Backup from `colorize_edge_attrs()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  graph <-
    create_graph(write_backups = TRUE) %>%
    add_path(
      n = 8,
      edge_data = edge_data(
        weight = c(
          8.2, 3.7, 6.3, 9.2,
          1.6, 2.5, 7.2))) %>%
    colorize_edge_attrs(
      edge_attr_from = weight,
      edge_attr_to = color,
      palette = "Greens",
      cut_points = c(1, 3, 5, 7, 9),
      alpha = 90)

  expect_equal(
    list.files(path = path) %>% length(), 25)
})

test_that("Graph backups for `drop_node_attrs()` works", {

  #
  # Backup from `drop_node_attrs()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_path(
      n = 4,
      node_aes = node_aes(
        color = "blue")) %>%
    drop_node_attrs(
      node_attr = color)

  expect_equal(
    list.files(path = path) %>% length(), 26)
})

test_that("Graph backups for `drop_edge_attrs()` works", {

  #
  # Backup from `drop_edge_attrs()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  create_graph(write_backups = TRUE) %>%
    add_path(
      n = 4,
      edge_aes = edge_aes(
        color = "blue")) %>%
    drop_edge_attrs(
      node_attr = color)

  expect_equal(
    list.files(path = path) %>% length(), 27)
})

test_that("Graph backups for `join_node_attrs()` works", {

  #
  # Backup from `join_node_attrs()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  df <-
    data.frame(
          id = c(1, 2, 3, 4, 5),
      values = c(5.5, 2.3, 6.3, 2.1, 8.7))

  create_graph(write_backups = TRUE) %>%
    add_path(n = 5) %>%
    join_node_attrs(df = df)

  expect_equal(
    list.files(path = path) %>% length(), 28)
})

test_that("Graph backups for `join_edge_attrs()` works", {

  #
  # Backup from `join_edge_attrs()`
  #

  path <- tempdir()
  on.exit(unlink(path))
  setwd(path)

  df <-
    data.frame(
      from = c(1, 2, 3, 4),
      to = c(2, 3, 4, 5),
      values = c(5.5, 2.3, 6.3, 2.1))

  create_graph(write_backups = TRUE) %>%
    add_path(n = 5) %>%
    join_edge_attrs(df = df)

  expect_equal(
    list.files(path = path) %>% length(), 29)
})
