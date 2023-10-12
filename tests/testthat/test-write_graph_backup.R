test_that("Graph backups for `add_balanced_tree()` works", {

  #
  # Backup from `add_balanced_tree()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_balanced_tree(
      k = 2, h = 2)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `add_cycle()` works", {

  #
  # Backup from `add_cycle()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_cycle(n = 6)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `add_edge()` works", {

  #
  # Backup from `add_edge()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_n_nodes(
      n = 2) %>%
    add_edge(
      from = 1,
      to = 2)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `add_edge_clone()` works", {

  #
  # Backup from `add_edge_clone()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

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

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `add_edge_clone()` works", {

  #
  # Backup from `add_edge_clone()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

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

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `add_edge_df()` works", {

  #
  # Backup from `add_edge_df()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

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

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `add_edge_df()` works", {

  #
  # Backup from `add_edges_from_table()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_nodes_from_table(
      table = currencies) %>%
    add_edges_from_table(
      table = usd_exchange_rates,
      from_col = from_currency,
      to_col = to_currency,
      from_to_map = iso_4217_code)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `add_edges_w_string()` works", {

  #
  # Backup from `add_edges_w_string()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_n_nodes(n = 4) %>%
    add_edges_w_string(
      edges = "1->2 1->3 2->4 2->3")

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `add_forward_edges_ws()` works", {

  #
  # Backup from `add_forward_edges_ws()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_n_nodes(
      n = 2,
      type = "type_a",
      label = c("a_1", "a_2")) %>%
    add_edge(
      from = 1, to = 2, rel = "a") %>%
    select_edges() %>%
    add_forward_edges_ws(rel = "b")

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `add_full_graph()` works", {

  #
  # Backup from `add_full_graph()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_full_graph(n = 5)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `add_gnm_graph()` works", {

  #
  # Backup from `add_gnm_graph()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_gnm_graph(n = 100, m = 120)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `add_gnp_graph()` works", {

  #
  # Backup from `add_gnp_graph()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_gnp_graph(n = 100, p = 0.05)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `add_node_clones_ws()` works", {

  #
  # Backup from `add_node_clones_ws()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_gnm_graph(
      n = 10, m = 22) %>%
    select_nodes() %>%
    add_node_clones_ws()

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `add_n_node_clones()` works", {

  #
  # Backup from `add_n_node_clones()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_gnm_graph(
      n = 10, m = 22) %>%
    add_n_node_clones(
      n = 2,
      node = 1)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `add_global_graph_attrs()` works", {

  #
  # Backup from `add_global_graph_attrs()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_global_graph_attrs(
      attr = "penwidth",
      value = 12,
      attr_type = "node")

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `delete_global_graph_attrs()` works", {

  #
  # Backup from `delete_global_graph_attrs()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    delete_global_graph_attrs(
      attr = "outputorder",
      attr_type = "graph")

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `add_graph_action()` works", {

  #
  # Backup from `add_graph_action()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_gnm_graph(
      n = 10, m = 22) %>%
    add_graph_action(
      fcn = "set_node_attr_w_fcn",
      node_attr_fcn = "get_betweenness",
      column_name = "btwns",
      action_name = "get_btwns")

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `delete_graph_actions()` works", {

  #
  # Backup from `delete_graph_actions()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

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

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `reorder_graph_actions()` works", {

  #
  # Backup from `reorder_graph_actions()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

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

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `transform_to_complement_graph()` works", {

  #
  # Backup from `transform_to_complement_graph()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_cycle(n = 4) %>%
    transform_to_complement_graph()

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `copy_node_attrs()` works", {

  #
  # Backup from `copy_node_attrs()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_path(
      n = 4,
      node_aes = node_aes(
        color = "blue")) %>%
    copy_node_attrs(
      node_attr_from = color,
      node_attr_to = color_2)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `copy_edge_attrs()` works", {

  #
  # Backup from `copy_edge_attrs()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_path(
      n = 4,
      edge_aes = edge_aes(
        color = "blue")) %>%
    copy_edge_attrs(
      edge_attr_from = color,
      edge_attr_to = color_2)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `colorize_node_attrs()` works", {

  #
  # Backup from `colorize_node_attrs()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

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

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `colorize_edge_attrs()` works", {

  #
  # Backup from `colorize_edge_attrs()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

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

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `drop_node_attrs()` works", {

  #
  # Backup from `drop_node_attrs()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_path(
      n = 4,
      node_aes = node_aes(
        color = "blue")) %>%
    drop_node_attrs(
      node_attr = color)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `drop_edge_attrs()` works", {

  #
  # Backup from `drop_edge_attrs()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_path(
      n = 4,
      edge_aes = edge_aes(
        color = "blue")) %>%
    drop_edge_attrs(
      edge_attr = color)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `join_node_attrs()` works", {

  #
  # Backup from `join_node_attrs()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  df <-
    data.frame(
          id = c(1, 2, 3, 4, 5),
      values = c(5.5, 2.3, 6.3, 2.1, 8.7))

  create_graph(write_backups = TRUE) %>%
    add_path(n = 5) %>%
    join_node_attrs(df = df)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `join_edge_attrs()` works", {

  #
  # Backup from `join_edge_attrs()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  df <-
    data.frame(
      from = c(1, 2, 3, 4),
      to = c(2, 3, 4, 5),
      values = c(5.5, 2.3, 6.3, 2.1))

  create_graph(write_backups = TRUE) %>%
    add_path(n = 5) %>%
    join_edge_attrs(df = df)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `mutate_node_attrs()` works", {

  #
  # Backup from `mutate_node_attrs()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_path(
      n = 4,
      node_data = node_data(
        weight = c(
          8.2, 3.7, 6.3, 9.2))) %>%
    mutate_node_attrs(
      half_weight = weight / 2)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `mutate_edge_attrs()` works", {

  #
  # Backup from `mutate_edge_attrs()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_path(
      n = 4,
      edge_data = edge_data(
        weight = c(
          8.2, 3.7, 6.3))) %>%
    mutate_edge_attrs(
      half_weight = weight / 2)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `mutate_node_attrs_ws()` works", {

  #
  # Backup from `mutate_node_attrs_ws()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_path(
      n = 4,
      node_data = node_data(
        weight = c(
          8.2, 3.7, 6.3, 9.2))) %>%
    select_nodes_by_id(nodes = c(1, 2)) %>%
    mutate_node_attrs_ws(
      half_weight = weight / 2)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `mutate_edge_attrs_ws()` works", {

  #
  # Backup from `mutate_edge_attrs_ws()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_path(
      n = 4,
      edge_data = edge_data(
        weight = c(
          8.2, 3.7, 6.3))) %>%
    select_edges_by_edge_id(edges = c(1, 2)) %>%
    mutate_edge_attrs_ws(
      half_weight = weight / 2)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `rename_node_attrs()` works", {

  #
  # Backup from `rename_node_attrs()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_path(
      n = 4,
      node_data = node_data(
        weight = c(
          8.2, 3.7, 6.3, 9.2))) %>%
    rename_node_attrs(
      node_attr_from = weight,
      node_attr_to = weight_2)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `rename_edge_attrs()` works", {

  #
  # Backup from `rename_edge_attrs()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_path(
      n = 4,
      edge_data = edge_data(
        weight = c(
          8.2, 3.7, 6.3))) %>%
    rename_edge_attrs(
      edge_attr_from = weight,
      edge_attr_to = weight_2)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `recode_node_attrs()` works", {

  #
  # Backup from `recode_node_attrs()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_gnm_graph(
      n = 5,
      m = 10,
      set_seed = 23) %>%
    set_node_attrs(
      node_attr = shape,
      values =
        c("circle", "hexagon",
          "rectangle", "rectangle",
          "circle")) %>%
    recode_node_attrs(
      node_attr_from = shape,
      "circle -> square",
      "rectangle -> triangle")

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `recode_edge_attrs()` works", {

  #
  # Backup from `recode_edge_attrs()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_path(
      n = 6,
      edge_data = edge_data(
        values = c(
          "circle", "hexagon",
          "rectangle", "rectangle",
          "circle"))) %>%
    recode_edge_attrs(
      edge_attr_from = values,
      "circle -> square",
      "rectangle -> triangle")

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `rescale_node_attrs()` works", {

  #
  # Backup from `rescale_node_attrs()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_path(
      n = 4,
      node_data = node_data(
        weight = c(
          8.2, 3.7, 6.3, 9.2))) %>%
    rescale_node_attrs(
      node_attr_from = weight,
      to_lower_bound = 0,
      to_upper_bound = 1)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `rescale_edge_attrs()` works", {

  #
  # Backup from `rescale_edge_attrs()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_path(
      n = 5,
      edge_data = edge_data(
        weight = c(
          8.2, 3.7, 6.3, 9.2))) %>%
    rescale_edge_attrs(
      edge_attr_from = weight,
      to_lower_bound = 0,
      to_upper_bound = 1)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `rev_edge_dir()` works", {

  #
  # Backup from `rev_edge_dir()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_path(n = 2) %>%
    rev_edge_dir()

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `rev_edge_dir_ws()` works", {

  #
  # Backup from `rev_edge_dir_ws()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_path(n = 3) %>%
    select_edges_by_edge_id(edges = 1) %>%
    rev_edge_dir_ws()

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `set_node_position()` works", {

  #
  # Backup from `set_node_position()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_node() %>%
    set_node_position(
      node = 1, x = 1, y = 1)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `nudge_node_positions_ws()` works", {

  #
  # Backup from `nudge_node_positions_ws()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_node() %>%
    set_node_position(
      node = 1, x = 1, y = 1) %>%
    select_nodes() %>%
    nudge_node_positions_ws(
      dx = 2, dy = 0)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})

test_that("Graph backups for `nudge_node_positions_ws()` works", {

  #
  # Backup from `nudge_node_positions_ws()`
  #

  main_wd <- getwd()
  random_dir <- paste(sample(letters[1:10], 10), collapse = "")
  dir.create(random_dir)
  setwd(random_dir)

  create_graph(write_backups = TRUE) %>%
    add_node() %>%
    set_node_position(
      node = 1, x = 1, y = 1) %>%
    select_nodes() %>%
    nudge_node_positions_ws(
      dx = 2, dy = 0)

  expect_length(list.files(), 1)

  setwd(main_wd)
  unlink(random_dir, recursive = TRUE)
})
