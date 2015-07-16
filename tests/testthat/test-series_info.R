context("Getting information on a graph series")

test_that("graph series information can be obtained", {

  library(magrittr)

  # Create 3 graph objects
  graph_1 <- create_graph() %>%
    add_node("a") %>% add_node("b") %>% add_node("c") %>%
    add_edges(from = c("a", "a", "b"),
              to =   c("c", "b", "c"))

  graph_2 <- graph_1 %>%
    add_node("d") %>% add_edges(from = "d", to = "c")

  graph_3 <- graph_2 %>%
    add_node("e") %>% add_edges(from = "e", to = "b")

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
  expect_equal(nrow(info_on_series), 3L)

  # Expect that the data frame has 7 columns
  expect_equal(ncol(info_on_series), 7L)

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
  expect_equal(info_on_series[1,1], 1L)
  expect_equal(info_on_series[2,1], 2L)
  expect_equal(info_on_series[3,1], 3L)
})
