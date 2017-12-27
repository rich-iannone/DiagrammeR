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
    list.files(path = path) %>% length(), 4)
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
    list.files(path = path) %>% length(), 5)
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
    list.files(path = path) %>% length(), 6)
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
    list.files(path = path) %>% length(), 7)
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
    list.files(path = path) %>% length(), 8)
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
    list.files(path = path) %>% length(), 9)
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
    list.files(path = path) %>% length(), 9)
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
    list.files(path = path) %>% length(), 9)
})

test_that("Graph backups for `add_graph_action()` works", {

  #
  # Backup from `add_gnp_graph()`
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
    list.files(path = path) %>% length(), 100)
})
