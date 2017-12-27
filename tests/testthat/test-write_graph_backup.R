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
