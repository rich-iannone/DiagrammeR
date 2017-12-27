context("Write graph backups")

test_that("Writing graph backups is possible", {

  #
  # Backup from `add_balanced_tree()`
  #

  path_add_balanced_tree <- tempdir()
  on.exit(unlink(path_add_balanced_tree))
  setwd(path_add_balanced_tree)

  create_graph(write_backups = TRUE) %>%
    add_balanced_tree(
      k = 2, h = 2)

  expect_equal(
    list.files(path = path_add_balanced_tree) %>% length(), 1)

  #
  # Backup from `add_cycle()`
  #

  path_add_cycle <- tempdir()
  on.exit(unlink(path_add_cycle))
  setwd(path_add_cycle)

  create_graph(write_backups = TRUE) %>%
    add_cycle(n = 6)

  expect_equal(
    list.files(path = path_add_cycle) %>% length(), 1)

  #
  # Backup from `add_edge()`
  #

  path_add_edge <- tempdir()
  on.exit(unlink(path_add_edge))
  setwd(path_add_edge)

  create_graph(write_backups = TRUE) %>%
    add_n_nodes(
      n = 2) %>%
    add_edge(
      from = 1,
      to = 2)

  expect_equal(
    list.files(path = path_add_edge) %>% length(), 1)

  #
  # Backup from `add_edge_clone()`
  #

  path_add_edge_clone <- tempdir()
  on.exit(unlink(path_add_edge_clone))
  setwd(path_add_edge_clone)

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
    list.files(path = path_add_edge_clone) %>% length(), 1)
})
