context("Creating and manipulating graph series objects")

test_that("creating an empty series is possible", {

  # Create an empty graph series of the `sequential` type
  series <-
    create_graph_series(series_type = "sequential")

  # Expect an object of class `dgr_graph_1D`
  expect_is(
    series, "dgr_graph_1D")

  # Expect that the series type is `sequential`
  expect_equal(
    series$series_type, "sequential")

  # Expect that several series components are `NULL`
  expect_null(
    series$graphs)

  expect_null(
    series$series_name)

  expect_null(
    series$series_scripts)

  # Expect that the series is empty
  expect_equal(
    graph_count(series), 0)

  # Create an empty graph series of the `temporal` type
  series_temporal <-
    create_graph_series(series_type = "temporal")

  # Expect that this series is empty
  expect_equal(
    graph_count(series_temporal), 0)

  # Expect that several series components are `NULL`
  expect_null(
    series_temporal$graphs)

  expect_null(
    series_temporal$series_name)

  expect_null(
    series_temporal$series_scripts)

  # Expect an object of class `dgr_graph_1D`
  expect_is(
    series_temporal, "dgr_graph_1D")

  # Expect that the series type is `sequential`
  expect_equal(
    series_temporal$series_type, "temporal")
})

test_that("adding graphs to a series is also possible", {

  # Create an empty graph series
  series <-
    create_graph_series(series_type = "sequential")

  # Create a set of graphs for a graph series
  graph_1 <-
    create_graph() %>%
    add_path(n = 4)

  graph_2 <-
    create_graph() %>%
    add_cycle(n = 5)

  graph_3 <-
    create_graph() %>%
    add_star(n = 6)

  # Add graphs to the graph series
  series <-
    series %>%
    add_graph_to_graph_series(
      graph = graph_1) %>%
    add_graph_to_graph_series(
      graph = graph_2) %>%
    add_graph_to_graph_series(
      graph = graph_3)

  # Expect an object of class `dgr_graph_1D`
  expect_is(
    series, "dgr_graph_1D")

  # Expect that the series type is `sequential`
  expect_equal(
    series$series_type, "sequential")

  # Expect that the `graphs` component is not `NULL`
  expect_true(
    !is.null(series$graphs))

  # Expect that several series components are `NULL`
  expect_null(
    series$series_name)

  expect_null(
    series$series_scripts)

  # Expect that the series has a graph count of 3
  expect_equal(
    graph_count(series), 3L)

  expect_equal(
    length(series$graphs), 3L)

  # Expect that the graphs within the graph series object
  # are indeed graph objects
  expect_is(
    series$graphs[[1]], "dgr_graph")

  expect_is(
    series$graphs[[2]], "dgr_graph")

  expect_is(
    series$graphs[[3]], "dgr_graph")

  # Expect that the graphs within the graph series object
  # are the same as those outside the series
  expect_equivalent(
    graph_1, series$graphs[[1]])

  expect_equivalent(
    graph_2, series$graphs[[2]])

  expect_equivalent(
    graph_3, series$graphs[[3]])

  # Create a series with a graph
  series_w_graph <-
    create_graph_series(
      graph = graph_1,
      series_type = "sequential")

  # Expect an object of class `dgr_graph_1D`
  expect_is(
    series_w_graph, "dgr_graph_1D")

  # Expect that the series type is `sequential`
  expect_equal(
    series_w_graph$series_type, "sequential")

  # Expect that the 'graphs' component is not `NULL`
  expect_true(
    !is.null(series_w_graph$graphs))

  # Expect that the series has a graph count of 1
  expect_equal(
    graph_count(series_w_graph), 1)

  # Expect that several series components are `NULL`
  expect_null(
    series_w_graph$series_name)

  expect_null(
    series_w_graph$series_scripts)

  # Expect that the graphs within the graph series object
  # are indeed graph objects
  expect_is(
    series_w_graph$graphs[[1]], "dgr_graph")

  # Expect an error when adding something other
  # than a graph object to a graph series
  expect_error(
    add_graph_to_graph_series(
      graph_series = series_w_graph,
      graph = series_w_graph))

  # Expect an error if graph series type is not valid
  graph_series_invalid_type <-
    create_graph_series(series_type = "circular")

  expect_error(
    add_graph_to_graph_series(
      graph_series = graph_series_invalid_type,
      graph = graph_1))
})

test_that("removing graphs from a series is possible", {

  # Create an empty graph series
  series <-
    create_graph_series(
      series_type = "sequential")

  # Create a set of graphs for a graph series
  graph_1 <-
    create_graph() %>%
    add_path(n = 4)

  graph_2 <-
    create_graph() %>%
    add_cycle(n = 5)

  graph_3 <-
    create_graph() %>%
    add_star(n = 6)

  # Add graphs to the graph series
  series <-
    series %>%
    add_graph_to_graph_series(
      graph = graph_1) %>%
    add_graph_to_graph_series(
      graph = graph_2) %>%
    add_graph_to_graph_series(
      graph = graph_3)

  # Expect that the series has a graph count of 3
  expect_equal(
    graph_count(series), 3)

  expect_equal(
    length(series$graphs), 3)

  # Remove the last graph from the series
  series_2 <- remove_from_series(graph_series = series)

  # Expect that the graph count is now 2
  expect_equal(
    graph_count(series_2), 2)

  # Remove the first graph from the series
  series_removed_1 <-
    remove_from_series(
      graph_series = series,
      index = 1)

  # Expect that the first created graph is
  # not in the series
  expect_false(
    count_nodes(graph = series_removed_1$graphs[[1]]) == count_nodes(graph = graph_1))

  # Expect that `graph_1` in the series is
  # equivalent to the `graph_2` object
  expect_true(
    count_nodes(graph = series_removed_1$graphs[[1]]) == count_nodes(graph = graph_2))

  # Remove the first graph from the series using
  # the `first` character vector
  series_removed_2 <-
    remove_from_series(
      graph_series = series,
      index = "first")

  # Expect that the first created graph is
  # not in the series
  expect_false(
    count_nodes(graph = series_removed_2$graphs[[1]]) == count_nodes(graph = graph_1))

  # Expect that graph 1 in the series is
  # equivalent to the `graph_2` object
  expect_true(
    count_nodes(graph = series_removed_2$graphs[[1]]) == count_nodes(graph = graph_2))
})

test_that("subsetting graphs from a temporal series is possible", {

  # Create a set of graphs for a graph series
  graph_time_1 <-
    create_graph(
      graph_name = "graph_with_time_1") %>%
    set_graph_time(
      time = "2015-03-25 03:00",
      tz = "GMT") %>%
    add_path(n = 4)

  graph_time_2 <-
    create_graph(
      graph_name = "graph_with_time_2") %>%
    set_graph_time(
      time = "2015-03-26 03:00",
      tz = "GMT") %>%
    add_cycle(n = 5)

  graph_time_3 <-
    create_graph(
      graph_name = "graph_with_time_3") %>%
    set_graph_time(
      time = "2015-03-27 15:00",
      tz = "GMT") %>%
    add_star(n = 6)

  # Create an empty graph series
  series_temporal <-
    create_graph_series(
      series_type = "temporal")

  # Add graphs to the graph series
  series_temporal <-
    series_temporal %>%
    add_graph_to_graph_series(
      graph = graph_time_1) %>%
    add_graph_to_graph_series(
      graph = graph_time_2) %>%
    add_graph_to_graph_series(
      graph = graph_time_3)

  # Expect a graph count of 3
  expect_equal(
    graph_count(series_temporal), 3)

  # Subset graph series by sequence
  series_sequence_subset <-
    subset_series(
      graph_series = series_temporal,
      by = "number",
      values = 2)

  # Expect a single graph in the series
  expect_equal(
    graph_count(series_sequence_subset), 1)

  # Expect that this subset graph is the same
  # as `graph_time_2`
  expect_true(
    count_nodes(graph = series_sequence_subset$graphs[[1]]) ==
      count_nodes(graph = graph_time_2))

  # Subset graph series by date-time
  series_time_subset <-
    subset_series(
      graph_series = series_temporal,
      by = "time",
      values = c("2015-03-25 12:00",
                 "2015-03-26 12:00"),
      tz = "GMT")

  # Expect 2 graphs in the series
  expect_equal(
    graph_count(series_time_subset), 2)
})

test_that("Getting a graph from a series is possible", {

  # Create a set of graphs for a graph series
  graph_1 <-
    create_graph() %>%
    add_path(n = 4)

  graph_2 <-
    create_graph() %>%
    add_cycle(n = 5)

  graph_3 <-
    create_graph() %>%
    add_star(n = 6)

  # Create an empty graph series and add
  # the graphs
  series <-
    create_graph_series() %>%
    add_graph_to_graph_series(
      graph = graph_1) %>%
    add_graph_to_graph_series(
      graph = graph_2) %>%
    add_graph_to_graph_series(
      graph = graph_3)

  # Get the second graph in the series
  extracted_graph <-
    get_graph_from_series(
      graph_series = series,
      graph_no = 2)

  # Expect the equivalent object among
  # `extracted_graph` and `graph_2`
  expect_equivalent(
    extracted_graph, graph_2)

  # Expect an error if extracting a graph from
  # an empty graph series
  expect_error(
    create_graph_series() %>%
      get_graph_from_series(1))

  # Expect an error if the index for the graph to
  # be extracted is out of range
  expect_error(
    series %>% get_graph_from_series(graph_no = 4))
})
