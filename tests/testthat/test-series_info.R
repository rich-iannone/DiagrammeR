test_that("graph series information can be obtained", {

  # Create an empty graph series
  empty_series <- create_graph_series()

  empty_series_info <-
    empty_series %>%
    get_graph_series_info()

  # Expect that `empty_series_info` is a
  # data frame object
  expect_s3_class(
    empty_series_info, "data.frame")

  # Expect that the data frame has no rows
  expect_equal(
    nrow(empty_series_info), 0)

  # Expect that the data frame has 7 columns
  expect_equal(
    ncol(empty_series_info), 7)

  # Expect specific column classes in the data frame
  expect_type(
    empty_series_info[, 1], "integer")

  expect_type(
    empty_series_info[, 2], "character")

  expect_s3_class(
    empty_series_info[, 3], "POSIXct")

  expect_type(
    empty_series_info[, 4], "character")

  expect_type(
    empty_series_info[, 5], "integer")

  expect_type(
    empty_series_info[, 6], "integer")

  expect_type(
    empty_series_info[, 7], "logical")

  # Create 3 graph objects
  graph_1 <-
    create_graph() %>%
    add_node(type = 1) %>%
    add_node(type = 2) %>%
    add_node(type = 3) %>%
    add_edge(
      from = 1,
      to = 3) %>%
    add_edge(
      from = 1,
      to = 2) %>%
    add_edge(
      from = 2,
      to = 3)

  graph_2 <-
    graph_1 %>%
    add_node(type = 4) %>%
    add_edge(
      from = 4,
      to = 3)

  graph_3 <-
    graph_2 %>%
    add_node(type = 5) %>%
    add_edge(
      from = 5,
      to = 2)

  # Create an empty graph series
  series <- create_graph_series(series_type = "sequential")

  # Add graphs to the graph series
  series <-
    series %>%
    add_graph_to_graph_series(
      graph = graph_1) %>%
    add_graph_to_graph_series(
      graph = graph_2) %>%
    add_graph_to_graph_series(
      graph = graph_3)

  # Get information on the graphs in the series
  info_on_series <-
    series %>%
    get_graph_series_info()

  # Expect that `info_on_series` is a data frame object
  expect_s3_class(
    info_on_series, "data.frame")

  # Expect that the data frame has 3 rows
  expect_equal(
    nrow(info_on_series), 3)

  # Expect that the data frame has 7 columns
  expect_equal(
    ncol(info_on_series), 7)

  # Expect specific column classes in the data frame
  expect_type(
    info_on_series[, 1], "integer")

  expect_type(
    info_on_series[, 2], "character")

  expect_s3_class(
    info_on_series[, 3], "POSIXct")

  expect_type(
    info_on_series[, 4], "character")

  expect_type(
    info_on_series[, 5], "integer")

  expect_type(
    info_on_series[, 6], "integer")

  expect_type(
    info_on_series[, 7], "logical")

  # Expect that the values in the `graph`
  # column are sequential
  expect_equal(
    info_on_series[1, 1], 1)

  expect_equal(
    info_on_series[2, 1], 2)

  expect_equal(
    info_on_series[3, 1], 3)

  # Create a temporal graph series and add
  # a graph with name and time information
  graph_series_temporal_type <-
    create_graph_series(series_type = "temporal")

  graph <-
    create_graph(
      graph_name = "graph_no_tz_provided") %>%
    set_graph_time(
      time = "2015-03-25 03:00",
      tz = "GMT")

  graph_series_temporal_type <-
    graph_series_temporal_type %>%
    add_graph_to_graph_series(
      graph = graph)

  info_on_series_temporal <-
    graph_series_temporal_type %>%
    get_graph_series_info()

  # Expect that `info_on_series_temporal` is
  # a data frame object
  expect_s3_class(
    info_on_series_temporal, "data.frame")

  # Expect that the data frame has 1 row
  expect_equal(
    nrow(info_on_series_temporal), 1)

  # Expect that the data frame has 7 columns
  expect_equal(
    ncol(info_on_series_temporal), 7)

  # Expect specific column classes in the data frame
  expect_type(
    info_on_series_temporal[, 1], "integer")

  expect_type(
    info_on_series_temporal[, 2], "character")

  expect_s3_class(
    info_on_series_temporal[, 3], "POSIXct")

  expect_type(
    info_on_series_temporal[, 4], "character")

  expect_type(
    info_on_series_temporal[, 5], "integer")

  expect_type(
    info_on_series_temporal[, 6], "integer")

  expect_type(
    info_on_series_temporal[, 7], "logical")

  # Expect that the `name`, `date_time`,
  # and `tz` columns are populated with information
  expect_equal(
    info_on_series_temporal[, 2], "graph_no_tz_provided")

  expect_false(
    is.na(info_on_series_temporal[, 3]))

  expect_equal(
    info_on_series_temporal[, 4], "GMT")
})
