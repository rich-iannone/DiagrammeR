context("Caching attributes")

test_that("Setting a cache is possible", {

  # Create a randomized graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      node_data = node_data(
        value = 1:10),
      set_seed = 23)

  # Get the closeness values (as a data frame) for
  # all nodes from `1` to `10`
  closeness_df <-
    get_closeness(graph)

  # Set the values in the `closeness_df` column
  # `closeness` in the graph's cache
  graph_cache <-
    graph %>%
    set_cache(
      name = "closeness_vector",
      to_cache = closeness_df,
      col = "closeness")

  # Expect that the column `closeness` in the df
  # is equivalent to the values in the cache
  expect_equivalent(
    closeness_df$closeness,
    graph_cache$cache$closeness_vector)

  # Expect an error if providing a data frame
  # and not specifying a column to extract as a vector
  expect_error(
    set_cache(
      graph = graph,
      name = "closeness_df_2",
      to_cache = closeness_df))

  # Expect an error if providing a data frame
  # and specifying a column that doesn't exist
  expect_error(
    set_cache(
      graph = graph,
      name = "closeness_df_3",
      to_cache = closeness_df,
      col = "nonexistent"))

  # Get the closeness values (as a vector)
  closeness_vec <- closeness_df$closeness

  # Set the values from the `closeness_vec` vector
  # in the graph's cache
  graph_cache_from_vec <-
    graph %>%
    set_cache(
      name = "closeness_vector",
      to_cache = closeness_vec)

  # Expect that the cache (originating from a vector)
  # is equivalent to the `closeness_vec` vector
  expect_equivalent(
    closeness_vec,
    graph_cache_from_vec$cache$closeness_vector)
})

test_that("Getting a cache is possible", {

  # Create a randomized graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      node_data = node_data(
        value = 1:10),
      set_seed = 23)

  # Get the closeness values (as a data frame) for
  # all nodes from `1` to `10`
  closeness_df <-
    get_closeness(graph)

  # Set the values in the `closeness_df` column
  # `closeness` in the graph's cache
  graph_cache <-
    graph %>%
    set_cache(
      name = "closeness_vector",
      to_cache = closeness_df,
      col = "closeness")

  # Get the graph's cached values
  cached <-
    get_cache(
      graph = graph_cache,
      name = "closeness_vector")

  # Expect a vector of length 10
  expect_equal(
    length(cached), 10)

  # Expect the vector to be `numeric`
  expect_is(
    cached, "numeric")

  # Expect that the cached values in `cached` are
  # equivalent to those in `graph$cache`
  expect_equivalent(
    cached, graph_cache$cache$closeness_vector)

  # Get the last cached vector
  cached_last <-
    get_cache(
      graph = graph_cache)

  # Expect a vector of length 10
  expect_equal(
    length(cached_last), 10)

  # Expect the vector to be `numeric`
  expect_is(
    cached_last, "numeric")

  # Create a new graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      node_data = node_data(
        value = 1:10),
      set_seed = 23)

  # Expect NA when trying to obtain a cache that is
  # not present
  expect_true(
    is.na(get_cache(graph)))
})
