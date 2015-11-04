context("Getting information on a graph series")

test_that("graph series information can be obtained", {

  library(magrittr)

  # Create an empty graph series
  empty_series <- create_series()

  empty_series_info <-
    series_info(graph_series = empty_series)

  # Expect that "empty_series_info" is a data frame object
  expect_is(empty_series_info, "data.frame")

  # Expect that the data frame has no rows
  expect_equal(nrow(empty_series_info), 0)

  # Expect that the data frame has 7 columns
  expect_equal(ncol(empty_series_info), 7)

  # Expect specific column classes in the data frame
  expect_is(empty_series_info[,1], "numeric")
  expect_is(empty_series_info[,2], "character")
  expect_is(empty_series_info[,3], "character")
  expect_is(empty_series_info[,4], "character")
  expect_is(empty_series_info[,5], "numeric")
  expect_is(empty_series_info[,6], "numeric")
  expect_is(empty_series_info[,7], "logical")

  # Create 3 graph objects
  graph_1 <- create_graph() %>%
    add_node("a") %>% add_node("b") %>% add_node("c") %>%
    add_edge("a", "c") %>% add_edge("a", "b") %>% add_edge("b", "c")

  graph_2 <- graph_1 %>%
    add_node("d") %>% add_edge("d", "c")

  graph_3 <- graph_2 %>%
    add_node("e") %>% add_edge("e", "b")

  # Create an empty graph series
  series <- create_series(series_type = "sequential")

  # Add graphs to the graph series
  series <- graph_1 %>% add_to_series(series)
  series <- graph_2 %>% add_to_series(series)
  series <- graph_3 %>% add_to_series(series)

  # Get information on the graphs in the series
  info_on_series <- series_info(series)

  # Expect that "info_on_series" is a data frame object
  expect_is(info_on_series, "data.frame")

  # Expect that the data frame has 3 rows
  expect_equal(nrow(info_on_series), 3)

  # Expect that the data frame has 7 columns
  expect_equal(ncol(info_on_series), 7)

  # Expect specific column classes in the data frame
  expect_is(info_on_series[,1], "numeric")
  expect_is(info_on_series[,2], "character")
  expect_is(info_on_series[,3], "character")
  expect_is(info_on_series[,4], "character")
  expect_is(info_on_series[,5], "numeric")
  expect_is(info_on_series[,6], "numeric")
  expect_is(info_on_series[,7], "logical")

  # Expect that the "name", "date_time", and "tz" columns
  # are filled with NAs
  expect_true(all(is.na(info_on_series[,2])))
  expect_true(all(is.na(info_on_series[,3])))
  expect_true(all(is.na(info_on_series[,4])))

  # Expect that the values in the "graph" column are sequential
  expect_equal(info_on_series[1,1], 1)
  expect_equal(info_on_series[2,1], 2)
  expect_equal(info_on_series[3,1], 3)

  # Create a temporal graph series and add a graph with name and
  # time information
  graph_series_temporal_type <-
    create_series(series_type = "temporal")

  graph <-
    create_graph(graph_name = "graph_no_tz_provided",
                 graph_time = "2015-03-25 03:00",
                 graph_tz = "GMT")

  graph_series_temporal_type <-
    add_to_series(graph = graph,
                  graph_series = graph_series_temporal_type)

  info_on_series_temporal <-
    series_info(graph_series_temporal_type)

  # Expect that "info_on_series_temporal" is a data frame object
  expect_is(info_on_series_temporal, "data.frame")

  # Expect that the data frame has 1 row
  expect_equal(nrow(info_on_series_temporal), 1)

  # Expect that the data frame has 7 columns
  expect_equal(ncol(info_on_series_temporal), 7)

  # Expect specific column classes in the data frame
  expect_is(info_on_series_temporal[,1], "numeric")
  expect_is(info_on_series_temporal[,2], "character")
  expect_is(info_on_series_temporal[,3], "character")
  expect_is(info_on_series_temporal[,4], "character")
  expect_is(info_on_series_temporal[,5], "numeric")
  expect_is(info_on_series_temporal[,6], "numeric")
  expect_is(info_on_series_temporal[,7], "logical")

  # Expect that the "name", "date_time", and "tz" columns
  # are populated with information
  expect_equal(info_on_series_temporal[,2], "graph_no_tz_provided")
  expect_equal(info_on_series_temporal[,3], "2015-03-25 03:00")
  expect_equal(info_on_series_temporal[,4], "GMT")
})
