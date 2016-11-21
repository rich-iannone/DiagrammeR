context("Caching attributes")

test_that("Caching node attributes is possible", {

  # Set a seed
  set.seed(25)

  # Create a graph with 10 nodes and 9 edges
  graph <-
    create_graph() %>%
    add_n_nodes(10) %>%
    set_node_attrs(
      "value", rnorm(node_count(.), 5, 2)) %>%
    add_edges_w_string(
      "1->2 1->3 2->4 2->5 3->6 3->7 4->8 4->9 5->10")

  # Cache all values from the node attribute `value`
  # as a numeric vector
  graph <-
    graph %>%
    cache_node_attrs("value", "numeric")

  # Expect a vector of length 10 in the graph's `$cache`
  expect_equal(length(graph$cache), 10)

  # Expect the vector to be `numeric`
  expect_is(graph$cache, "numeric")

  # Expect the vector to be equivalent to the node
  # attribute values in the graph's internal ndf
  # after coercing to numeric
  expect_equivalent(
    as.numeric(graph$nodes_df$value),
    graph$cache)

  # Cache all values from the node attribute `value`
  # as a character vector
  graph <-
    graph %>%
    cache_node_attrs("value", "character")

  # Expect a vector of length 10 in the graph's `$cache`
  expect_equal(length(graph$cache), 10)

  # Expect the vector to be `character`
  expect_is(graph$cache, "character")

  # Expect the charaacter vector to be equivalent
  # to the node attribute values in the graph's
  # internal node data frame
  expect_equivalent(
    as.character(graph$nodes_df$value),
    graph$cache)

  # Cache selected values from the node attribute
  # `value` as a character vector
  graph <-
    graph %>%
    cache_node_attrs("value", "character", nodes = 1:2)

  # Expect a vector of length 2 in the graph's `$cache`
  expect_equal(length(graph$cache), 2)

  # Expect the vector to be `character`
  expect_is(graph$cache, "character")

  # Expect the numeric vector to be equivalent to the
  # first two node attribute values in the
  # graph's internal ndf
  expect_equivalent(
    as.character(graph$nodes_df$value[1:2]),
    graph$cache)
})

test_that("Caching edge attributes is possible", {

  # Set a seed
  set.seed(25)

  # Create a graph with 10 nodes and 9 edges
  graph <-
    create_graph() %>%
    add_n_nodes(10) %>%
    add_edges_w_string(
      "1->2 1->3 2->4 2->5 3->6 3->7 4->8 4->9 5->10") %>%
    set_edge_attrs(
      "value", rnorm(edge_count(.), 5, 2))

  # Cache all values from the edge attribute `value`
  # as a numeric vector
  graph <-
    graph %>%
    cache_edge_attrs("value", "numeric")

  # Expect a vector of length 9 in the graph's `$cache`
  expect_equal(length(graph$cache), 9)

  # Expect the vector to be `numeric`
  expect_is(graph$cache, "numeric")

  # Expect the vector to be equivalent to the edge
  # attribute values in the graph's internal edf
  # after coercing to numeric
  expect_equivalent(
    as.numeric(graph$edges_df$value),
    graph$cache)

  # Cache all values from the edge attribute `value`
  # as a character vector
  graph <-
    graph %>%
    cache_edge_attrs("value", "character")

  # Expect a vector of length 9 in the graph's `$cache`
  expect_equal(length(graph$cache), 9)

  # Expect the vector to be `character`
  expect_is(graph$cache, "character")

  # Expect the vector to be equivalent to the node
  # attribute values in the graph's internal edf
  expect_equivalent(
    as.character(graph$edges_df$value),
    graph$cache)

  # Cache selected values from the edge attribute
  # `value` as a character vector
  graph <-
    graph %>%
    cache_edge_attrs(
      "value", "character",
      from = c(1, 1), to = c(2, 3))

  # Expect a vector of length 2 in the graph's `$cache`
  expect_equal(length(graph$cache), 2)

  # Expect the vector to be `character`
  expect_is(graph$cache, "character")

  # Expect the vector to be equivalent to the first two
  # node attribute values in the graph's internal ndf
  expect_equivalent(
    as.character(graph$edges_df$value[1:2]),
    graph$cache)
})

test_that("Caching node attrs (w/ selection) is possible", {

  # Set a seed
  set.seed(25)

  # Create a graph with 10 nodes and 9 edges
  graph <-
    create_graph() %>%
    add_n_nodes(10) %>%
    set_node_attrs(
      "value", rnorm(node_count(.), 5, 2)) %>%
    add_edges_w_string(
      "1->2 1->3 2->4 2->5 3->6 3->7 4->8 4->9 5->10")

  # Select all nodes where the node attribute `value`
  # is less than 5
  graph <-
    graph %>%
    select_nodes("value < 5.0")

  # Cache available values from the node attribute
  # `value` from the nodes that are selected; ensure
  # that the cached vector is numeric
  graph <-
    graph %>%
    cache_node_attrs_ws("value", "numeric")

  # Expect a vector of length 7 in the graph's `$cache`
  expect_equal(length(graph$cache), 7)

  # Expect the vector to be `numeric`
  expect_is(graph$cache, "numeric")

  # Expect the vector to be equivalent to the node
  # attribute values for certain nodes in the graph's
  # internal ndf (after coercing to numeric)
  expect_equivalent(
    as.numeric(graph$nodes_df$value)[c(1, 2, 6, 7, 8, 9, 10)],
    graph$cache)

  # Cache available values from the node attribute
  # `value` from the nodes that are selected; this time,
  # leave the cached vector as a character vector
  graph <-
    graph %>%
    cache_node_attrs_ws("value", "character")

  # Expect a vector of length 7 in the graph's `$cache`
  expect_equal(length(graph$cache), 7)

  # Expect the vector to be `character`
  expect_is(graph$cache, "character")

  # Expect the vector to be equivalent to the node
  # attribute values for certain nodes in the graph's
  # internal ndf
  expect_equivalent(
    as.character(graph$nodes_df$value[c(1, 2, 6, 7, 8, 9, 10)]),
    graph$cache)

  # Expect an error if the value provided in `node_attr`
  # is not a valid node attribute
  expect_error(
    create_graph() %>%
      add_n_nodes(10) %>%
      set_node_attrs(
        "value", rnorm(node_count(.), 5, 2)) %>%
      add_edges_w_string(
        "1->2 1->3 2->4 2->5 3->6 3->7 4->8 4->9 5->10") %>%
      select_nodes("value < 5.0") %>%
      cache_node_attrs_ws("value_2", "numeric"))
})

test_that("Caching edge attrs (w/ selection) is possible", {

  # Set a seed
  set.seed(23)

  # Create a graph with 10 nodes and 9 edges
  graph <-
    create_graph() %>%
    add_n_nodes(10) %>%
    add_edges_w_string(
      "1->2 1->3 2->4 2->5 3->6 3->7 4->8 4->9 5->10") %>%
    set_edge_attrs(
      "value", rnorm(edge_count(.), 5, 2))

  # Expect an error if no edge selection is available
  expect_error(
    graph %>%
      cache_edge_attrs_ws("value", "numeric"))

  # Select all nodes where the edge attribute `value`
  # is less than 5
  graph <-
    graph %>%
    select_edges("value < 5.0")

  # Cache available values from the edge attribute
  # `value` from the edges that are selected; ensure
  # that the cached vector is numeric
  graph <-
    graph %>%
    cache_edge_attrs_ws("value", "numeric")

  # Expect a vector of length 3 in the graph's `$cache`
  expect_equal(length(graph$cache), 3)

  # Expect the vector to be `numeric`
  expect_is(graph$cache, "numeric")

  # Cache available values from the edge attribute
  # `value` from the edges that are selected; this time,
  # leave the cached vector as a character vector
  graph <-
    graph %>%
    cache_edge_attrs_ws("value", "character")

  # Expect a vector of length 3 in the graph's `$cache`
  expect_equal(length(graph$cache), 3)

  # Expect the vector to be `character`
  expect_is(graph$cache, "character")
})

test_that("Caching a count of nodes is possible", {

  # Create a graph with 10 nodes and 9 edges
  graph <-
    create_graph() %>%
    add_n_nodes(10) %>%
    add_edges_w_string(
      "1->2 1->3 2->4 2->5 3->6 3->7 4->8 4->9 5->10")

  # Cache a count of nodes after creating a selection
  graph <-
    graph %>%
    select_nodes_by_id(2:5) %>%
    cache_node_count_ws()

  # Expect that the number of nodes stored in the cache
  # is 4
  expect_equal(graph$cache, 4)

  # Expect an error when caching a count of nodes where
  # there is no selection of nodes in the graph
  expect_error(
    graph %>%
      clear_selection() %>%
      cache_node_count_ws())
})

test_that("Caching a count of edges is possible", {

  # Create a graph with 10 nodes and 9 edges
  graph <-
    create_graph() %>%
    add_n_nodes(10) %>%
    add_edges_w_string(
      "1->2 1->3 2->4 2->5 3->6 3->7 4->8 4->9 5->10")

  # Cache a count of edges after creating a selection
  graph <-
    graph %>%
    select_edges_by_node_id(2) %>%
    cache_edge_count_ws()

  # Expect that the number of edges stored in the cache
  # is 3
  expect_equal(graph$cache, 3)

  # Expect an error when caching a count of edges where
  # there is no selection of edges in the graph
  expect_error(
    graph %>%
      clear_selection %>%
      cache_edge_count_ws())
})

test_that("Caching node attrs (w/ selection) is possible", {

  # Set a seed
  set.seed(25)

  # Create a graph with 10 nodes and 9 edges
  graph <-
    create_graph() %>%
    add_n_nodes(10) %>%
    set_node_attrs(
      "value", rnorm(node_count(.), 5, 2)) %>%
    add_edges_w_string(
      "1->2 1->3 2->4 2->5 3->6 3->7 4->8 4->9 5->10")

  # Select all nodes where the node attribute `value`
  # is less than 5
  graph <-
    graph %>%
    select_nodes("value < 5.0")

  # Cache available values from the node attribute
  # `value` from the nodes that are selected; ensure
  # that the cached vector is numeric
  graph <-
    graph %>%
    cache_node_attrs_ws("value", "numeric")

  # Expect a vector of length 7 in the graph's `$cache`
  expect_equal(length(graph$cache), 7)

  # Expect the vector to be `numeric`
  expect_is(graph$cache, "numeric")

  # Expect the vector to be equivalent to the node
  # attribute values for certain nodes in the graph's
  # internal ndf (after coercing to numeric)
  expect_equivalent(
    as.numeric(graph$nodes_df$value)[c(1, 2, 6, 7, 8, 9, 10)],
    graph$cache)

  # Cache available values from the node attribute
  # `value` from the nodes that are selected; this time,
  # leave the cached vector as a character vector
  graph <-
    graph %>%
    cache_node_attrs_ws("value", "character")

  # Expect a vector of length 7 in the graph's `$cache`
  expect_equal(length(graph$cache), 7)

  # Expect the vector to be `character`
  expect_is(graph$cache, "character")

  # Expect the vector to be equivalent to the node
  # attribute values for certain nodes in the graph's
  # internal ndf
  expect_equivalent(
    as.character(graph$nodes_df$value[c(1, 2, 6, 7, 8, 9, 10)]),
    graph$cache)
})

test_that("Caching edge attrs (w/ selection) is possible", {

  # Set a seed
  set.seed(23)

  # Create a graph with 10 nodes and 9 edges
  graph <-
    create_graph() %>%
    add_n_nodes(10) %>%
    add_edges_w_string(
      "1->2 1->3 2->4 2->5 3->6 3->7 4->8 4->9 5->10") %>%
    set_edge_attrs(
      "value", rnorm(edge_count(.), 5, 2))

  # Select all nodes where the edge attribute `value`
  # is less than 5
  graph <-
    graph %>%
    select_edges("value < 5.0")

  # Cache available values from the edge attribute
  # `value` from the edges that are selected; ensure
  # that the cached vector is numeric
  graph <-
    graph %>%
    cache_edge_attrs_ws("value", "numeric")

  # Expect a vector of length 3 in the graph's `$cache`
  expect_equal(length(graph$cache), 3)

  # Expect the vector to be `numeric`
  expect_is(graph$cache, "numeric")

  # Cache available values from the edge attribute
  # `value` from the edges that are selected; this time,
  # leave the cached vector as a character vector
  graph <-
    graph %>%
    cache_edge_attrs_ws("value", "character")

  # Expect a vector of length 3 in the graph's `$cache`
  expect_equal(length(graph$cache), 3)

  # Expect the vector to be `character`
  expect_is(graph$cache, "character")
})

test_that("Caching a count of nodes is possible", {

  # Create a graph with 10 nodes and 9 edges
  graph <-
    create_graph() %>%
    add_n_nodes(10) %>%
    add_edges_w_string(
      "1->2 1->3 2->4 2->5 3->6 3->7 4->8 4->9 5->10")

  # Cache a count of nodes after creating a selection
  graph <-
    graph %>%
    select_nodes_by_id(2:5) %>%
    cache_node_count_ws()

  # Expect that the number of nodes stored in the cache
  # is 4
  expect_equal(graph$cache, 4)

  # Expect an error when cachine a count of nodes where
  # there is no selection of nodes in the graph
  expect_error(
    graph %>%
      clear_selection() %>%
      cache_node_count_ws())
})

test_that("Getting a cache is possible", {

  # Set a seed
  set.seed(25)

  # Create a graph with 10 nodes and 9 edges
  graph <-
    create_graph() %>%
    add_n_nodes(10) %>%
    set_node_attrs(
      "value", rnorm(node_count(.), 5, 2)) %>%
    add_edges_w_string(
      "1->2 1->3 2->4 2->5 3->6 3->7 4->8 4->9 5->10")

  # Cache all values from the node attribute `value`
  # as a numeric vector
  graph <-
    graph %>%
    cache_node_attrs("value", "numeric")

  # Get the graph's cached values
  cached <- get_cache(graph)

  # Expect a vector of length 10
  expect_equal(length(cached), 10)

  # Expect the vector to be `numeric`
  expect_is(cached, "numeric")

  # Expect that the cached values in `cached` are
  # equivalent to those in `graph$cache`
  expect_equivalent(cached, graph$cache)

  # Create a new graph with 10 nodes and 9 edges
  graph <-
    create_graph() %>%
    add_n_nodes(10) %>%
    set_node_attrs(
      "value", rnorm(node_count(.), 5, 2)) %>%
    add_edges_w_string(
      "1->2 1->3 2->4 2->5 3->6 3->7 4->8 4->9 5->10")

  # Expect NA when trying to obtain a cache that is
  # not present
  expect_true(is.na(get_cache(graph)))
})

test_that("Setting a cache is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      10, 22, set_seed = 1)

  # Get the closeness values (as a data frame) for
  # all nodes from `1` to `10`
  closeness_df <- get_closeness(graph)

  # Set the values in the `closeness_df` column
  # `closeness` in the graph's cache
  graph_cache <-
    graph %>%
    set_cache(closeness_df, "closeness")

  # Expect that the column `closeness` in the df
  # is equivalent to the values in the cache
  expect_equivalent(
    closeness_df$closeness, graph_cache$cache)

  # Expect an error if providing a data frame
  # and not specifying a column to extract as a vector
  expect_error(
    set_cache(graph, closeness_df))

  # Expect an error if providing a data frame
  # and specifying a column that doesn't exist
  expect_error(
    set_cache(graph, closeness_df, "nonexistent"))

  # Get the closeness values (as a vector)
  closeness_vec <- closeness_df$closeness

  # Set the values from the `closeness_vec` vector
  # in the graph's cache
  graph_cache_from_vec <-
    graph %>%
    set_cache(closeness_vec)

  # Expect that the cache (originating from a vector)
  # is equivalent to the `closeness_vec` vector
  expect_equivalent(
    closeness_vec,
    graph_cache_from_vec$cache)
})
