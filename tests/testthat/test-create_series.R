context("Creating a graph series can be done")

test_that("creating an empty series is possible", {

  # Create an empty graph series
  series <- create_series(series_type = "sequential")

  # Expect an object of class "dgr_graph_1D"
  expect_is(series, "dgr_graph_1D")

  # Expect that the series type is sequential
  expect_equal(series$series_type, "sequential")

  # Expect that several series components are NULL
  expect_null(series$graphs)
  expect_null(series$series_name)
  expect_null(series$series_scripts)

  # Expect that the series is empty
  expect_equal(graph_count(series), 0)
})

test_that("adding graphs to a series is also possible", {

  library(magrittr)

  # Create an empty graph series
  series <- create_series(series_type = "sequential")

  # Create three different graphs
  graph_1 <- create_graph() %>%
    add_node("a") %>% add_node("b") %>% add_node("c") %>%
    add_edges(from = c("a", "a", "b"),
              to   = c("c", "b", "c"))

  graph_2 <- graph_1 %>%
    add_node("d") %>% add_edges(from = "d", to = "c")

  graph_3 <- graph_2 %>%
    add_node("e") %>% add_edges(from = "e", to = "b")

  # Add graphs to the graph series
  series <- graph_1 %>% add_to_series(series)
  series <- graph_2 %>% add_to_series(series)
  series <- graph_3 %>% add_to_series(series)

  # Expect an object of class "dgr_graph_1D"
  expect_is(series, "dgr_graph_1D")

  # Expect that the series type is sequential
  expect_equal(series$series_type, "sequential")

  # Expect that the 'graphs' component is not NULL
  expect_true(!is.null(series$graphs))

  # Expect that several series components are NULL
  expect_null(series$series_name)
  expect_null(series$series_scripts)

  # Expect that the series has a graph count of 3
  expect_equal(graph_count(series), 3L)
  expect_equal(length(series$graphs), 3L)

  # Expect that the graphs within the graph series object
  # are indeed graph objects
  expect_is(series$graphs[[1]], "dgr_graph")
  expect_is(series$graphs[[2]], "dgr_graph")
  expect_is(series$graphs[[3]], "dgr_graph")

  # Expect that the graphs within the graph series object
  # are the same as those outside the series
  expect_equivalent(graph_1, series$graphs[[1]])
  expect_equivalent(graph_2, series$graphs[[2]])
  expect_equivalent(graph_3, series$graphs[[3]])
})
