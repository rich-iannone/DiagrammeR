context("Setting and getting data frames as node/edge attributes")

test_that("setting DFs as node attributes is possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_path(n = 8)

  # Create 2 data frames
  df_1 <-
    data.frame(
      a = c("one", "two", "three"),
      b = c(1, 2, 3),
      stringsAsFactors = FALSE)

  df_2 <-
    data.frame(
      c = c("four", "five", "six"),
      d = c(4, 5, 6),
      stringsAsFactors = FALSE)

  # Bind the data frame as a node attributes
  # of nodes `1` and `2`
  graph <-
    graph %>%
    set_df_as_node_attr(
      node = 1,
      df = df_1) %>%
    set_df_as_node_attr(
      node = 2,
      df = df_2)

  # Expect that the list of tibbles in
  # `graph$df_storage` to be of length 2
  expect_equal(
    graph$df_storage %>% length(), 2)

  # Expect certain columns in each of the
  # stored tibble objects
  expect_identical(
    graph$df_storage[[1]] %>%
      colnames(),
    c("df_id__", "node_edge__", "id__", "a", "b"))

  expect_identical(
    graph$df_storage[[2]] %>%
      colnames(),
    c("df_id__", "node_edge__", "id__", "c", "d"))

  # Expect that the `id__` column for each
  # stored tibble is associated with the
  # nodes to which each are assigned
  expect_equal(
    graph$df_storage[[1]]$id__ %>%
      unique(), 1)

  expect_equal(
    graph$df_storage[[2]]$id__ %>%
      unique(), 2)

  # Replace the tibble that is
  # associated with node `1`
  graph <-
    graph %>%
    set_df_as_node_attr(
      node = 1,
      df = df_2)

  # Expect that the column names for each
  # stored tibble is now the same
  expect_identical(
    graph$df_storage[[1]] %>% colnames(),
    graph$df_storage[[2]] %>% colnames())

  # Expect an error if the value provided
  # for node is not single-length
  expect_error(
    graph %>%
      set_df_as_node_attr(
        node = c(1, 2),
        df = df_2))

  # Expect an error if the value provided
  # for node is in the graph
  expect_error(
    graph %>%
      set_df_as_node_attr(
        node = 10,
        df = df_2))
})

test_that("setting DFs as edge attributes is possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_path(n = 8)

  # Create 2 data frames
  df_1 <-
    data.frame(
      a = c("one", "two", "three"),
      b = c(1, 2, 3),
      stringsAsFactors = FALSE)

  df_2 <-
    data.frame(
      c = c("four", "five", "six"),
      d = c(4, 5, 6),
      stringsAsFactors = FALSE)

  # Bind the data frame as a edge attributes
  # of edges `1` and `2`
  graph <-
    graph %>%
    set_df_as_edge_attr(
      edge = 1,
      df = df_1) %>%
    set_df_as_edge_attr(
      edge = 2,
      df = df_2)

  # Expect that the list of tibbles in
  # `graph$df_storage` to be of length 2
  expect_equal(
    graph$df_storage %>% length(), 2)

  # Expect certain columns in each of the
  # stored tibble objects
  expect_identical(
    graph$df_storage[[1]] %>%
      colnames(),
    c("df_id__", "node_edge__", "id__", "a", "b"))

  expect_identical(
    graph$df_storage[[2]] %>%
      colnames(),
    c("df_id__", "node_edge__", "id__", "c", "d"))

  # Expect that the `id__` column for each
  # stored tibble is associated with the
  # edges to which each are assigned
  expect_equal(
    graph$df_storage[[1]]$id__ %>%
      unique(), 1)

  expect_equal(
    graph$df_storage[[2]]$id__ %>%
      unique(), 2)

  # Replace the tibble that is
  # associated with node `1`
  graph <-
    graph %>%
    set_df_as_edge_attr(
      edge = 1,
      df = df_2)

  # Expect that the column names for each
  # stored tibble is now the same
  expect_identical(
    graph$df_storage[[1]] %>% colnames(),
    graph$df_storage[[2]] %>% colnames())

  # Expect an error if the value provided
  # for edge is not single-length
  expect_error(
    graph %>%
      set_df_as_edge_attr(
        edge = c(1, 2),
        df = df_2))

  # Expect an error if the value provided
  # for edge is in the graph
  expect_error(
    graph %>%
      set_df_as_edge_attr(
        edge = 10,
        df = df_2))
})

test_that("getting DFs as node/edge attributes is possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_path(n = 8)

  # Create 2 data frames
  df_1 <-
    data.frame(
      a = c("one", "two", "three"),
      b = c(1, 2, 3),
      stringsAsFactors = FALSE)

  df_2 <-
    data.frame(
      c = c("four", "five", "six"),
      d = c(4, 5, 6),
      stringsAsFactors = FALSE)

  # Bind the each data frame as node
  # and edge attributes (nodes and
  # edges `1` and `2`)
  graph <-
    graph %>%
    set_df_as_node_attr(
      node = 1,
      df = df_1) %>%
    set_df_as_edge_attr(
      edge = 1,
      df = df_1) %>%
    set_df_as_node_attr(
      node = 2,
      df = df_2) %>%
    set_df_as_edge_attr(
      edge = 2,
      df = df_2)

  # Expect that extracting as a
  # tibble returns a `tbl_df`
  expect_is(
    graph %>%
    get_attr_dfs(
      node_id = 1,
      return_format = "single_tbl"),
    "tbl_df")

  expect_is(
    graph %>%
      get_attr_dfs(
        edge_id = 1,
        return_format = "single_tbl"),
    "tbl_df")

  # Expect that extracting as a
  # df returns a `data.frame`
  expect_is(
    graph %>%
      get_attr_dfs(
        node_id = 1,
        return_format = "single_df"),
    "data.frame")

  expect_is(
    graph %>%
      get_attr_dfs(
        edge_id = 1,
        return_format = "single_df"),
    "data.frame")

  # Expect that extraction can occur
  # when targeting 2 nodes
  expect_is(
    graph %>%
      get_attr_dfs(
        node_id = c(1, 2),
        return_format = "single_tbl"),
    "tbl_df")

  # Expect that extraction can occur
  # when targeting 2 edges
  expect_is(
    graph %>%
      get_attr_dfs(
        edge_id = c(1, 2),
        return_format = "single_tbl"),
    "tbl_df")

  # Expect certain columns in each of the
  # extracted tibble objects
  expect_identical(
    graph %>%
      get_attr_dfs(
        node_id = c(1, 2),
        return_format = "single_tbl") %>%
      colnames(),
    c("node_edge__", "id", "type", "label",
      "a", "b", "c", "d"))

  expect_identical(
    graph %>%
      get_attr_dfs(
        edge_id = c(1, 2),
        return_format = "single_tbl") %>%
      colnames(),
    c("node_edge__", "id", "rel", "from", "to",
      "a", "b", "c", "d"))

  # Expect that the `id` column for each
  # stored tibble is associated with the
  # edges to which each are assigned
  expect_equal(
    (graph %>%
       get_attr_dfs(
         node_id = 1,
         return_format = "single_tbl"))$id %>%
      unique(), 1)

  expect_equal(
    (graph %>%
       get_attr_dfs(
         node_id = 2,
         return_format = "single_tbl"))$id %>%
      unique(), 2)

  expect_equal(
    (graph %>%
       get_attr_dfs(
         edge_id = 1,
         return_format = "single_tbl"))$id %>%
      unique(), 1)

  expect_equal(
    (graph %>%
       get_attr_dfs(
         edge_id = 2,
         return_format = "single_tbl"))$id %>%
      unique(), 2)

  # Expect an error if the value provided
  # for edge is not single-length
  expect_error(
    graph %>%
      set_df_as_edge_attr(
        edge = c(1, 2),
        df = df_2))

  # Expect an error if the value provided
  # for edge is in the graph
  expect_error(
    graph %>%
      set_df_as_edge_attr(
        edge = 10,
        df = df_2))
})
