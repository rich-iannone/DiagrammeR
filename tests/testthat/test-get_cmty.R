context("Community detection algorithms")

test_that("the edge betweeness algorithm is functional", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 10,
      set_seed = 23)

  # Get the edge betweenness for the graph
  edge_betweeness <- get_cmty_edge_btwns(graph)

  # Expect that a `data.frame` object is returned
  expect_s3_class(
    edge_betweeness, "data.frame")

  # Expect 2 columns in the data frame
  expect_equal(
    ncol(
      edge_betweeness), 2)

  # Expect specific column names in this data frame
  expect_equal(
    colnames(edge_betweeness),
    c("id", "edge_btwns_group"))

  # Expect as many rows in the data frame as there
  # are nodes in the graph
  expect_equal(
    nrow(edge_betweeness),
    count_nodes(graph = graph))

  # For this analysis expect 5 different groups
  expect_equal(
    sort(unique(edge_betweeness$edge_btwns_group)),
    c(1, 2, 3, 4, 5, 6))

  # Expect the first column to be integer
  expect_type(
    edge_betweeness$id, "integer")

  # Expect the second column to be numeric
  expect_type(
    edge_betweeness$edge_btwns_group, "double")
})

test_that("the fast-greedy algorithm is functional", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 15,
      m = 10,
      set_seed = 23)

  # Get the edge betweenness for the graph
  fast_greedy <- get_cmty_fast_greedy(graph)

  # Expect that a `data.frame` object is returned
  expect_s3_class(
    fast_greedy, "data.frame")

  # Expect 2 columns in the data frame
  expect_equal(
    ncol(fast_greedy), 2)

  # Expect specific column names in this data frame
  expect_equal(
    colnames(fast_greedy),
    c("id", "f_g_group"))

  # Expect as many rows in the data frame as there
  # are nodes in the graph
  expect_equal(
    nrow(fast_greedy), count_nodes(graph = graph))

  # For this analysis expect two different groups
  # identified with labels `1` and `2`
  expect_equal(
    sort(unique(fast_greedy$f_g_group)),
    c(1, 2, 3, 4, 5, 6))

  # Expect the first column to be integer
  expect_type(
    fast_greedy$id, "integer")

  # Expect the second column to be numeric
  expect_type(
    fast_greedy$f_g_group, "double")
})

test_that("the leading eigenvector algorithm is functional", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 10,
      set_seed = 23)

  # Get the edge betweenness for the graph
  l_eigenvec <- get_cmty_l_eigenvec(graph)

  # Expect that a `data.frame` object is returned
  expect_s3_class(
    l_eigenvec, "data.frame")

  # Expect 2 columns in the data frame
  expect_equal(
    ncol(l_eigenvec), 2)

  # Expect specific column names in this data frame
  expect_equal(
    colnames(l_eigenvec),
    c("id", "l_eigenvec_group"))

  # Expect as many rows in the data frame as there
  # are nodes in the graph
  expect_equal(
    nrow(l_eigenvec), count_nodes(graph = graph))

  # For this analysis expect two different groups
  # identified with labels `1` and `2`
  expect_equal(
    sort(unique(l_eigenvec$l_eigenvec_group)),
    c(1, 2, 3, 4))

  # Expect the first column to be integer
  expect_type(l_eigenvec$id, "integer")

  # Expect the second column to be numeric
  expect_type(l_eigenvec$l_eigenvec_group, "double")
})

test_that("the Louvain algorithm is functional", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 10,
      set_seed = 23)

  # Get the edge betweenness for the graph
  louvain <- get_cmty_louvain(graph)

  # Expect that a `data.frame` object is returned
  expect_s3_class(
    louvain, "data.frame")

  # Expect 2 columns in the data frame
  expect_equal(
    ncol(louvain), 2)

  # Expect specific column names in this data frame
  expect_equal(
    colnames(louvain),
    c("id", "louvain_group"))

  # Expect as many rows in the data frame as there
  # are nodes in the graph
  expect_equal(
    nrow(louvain), count_nodes(graph = graph))

  # For this analysis expect two different groups
  # identified with labels `1` and `2`
  expect_equal(
    sort(unique(louvain$louvain_group)),
    c(1, 2, 3, 4))

  # Expect the first column to be integer
  expect_type(
    louvain$id, "integer")

  # Expect the second column to be numeric
  expect_type(
    louvain$louvain_group, "double")
})

test_that("the walktrap algorithm is functional", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 10,
      set_seed = 23)

  # Get the edge betweenness for the graph
  walktrap <- get_cmty_walktrap(graph)

  # Expect that a `data.frame` object is returned
  expect_s3_class(
    walktrap, "data.frame")

  # Expect 2 columns in the data frame
  expect_equal(
    ncol(walktrap), 2)

  # Expect specific column names in this data frame
  expect_named(
    walktrap,
    c("id", "walktrap_group"))

  # Expect as many rows in the data frame as there
  # are nodes in the graph
  expect_equal(
    nrow(walktrap), count_nodes(graph = graph))

  # For this analysis expect two different groups
  # identified with labels `1` and `2`
  expect_equal(
    sort(unique(walktrap$walktrap_group)),
    c(1, 2, 3, 4))

  # Expect the first column to be integer
  expect_type(
    walktrap$id, "integer")

  # Expect the second column to be numeric
  expect_type(
    walktrap$walktrap_group, "double")
})
