context("Print graph summary")

get_printed_output <- function(graph, line = NULL) {

  captured_output <-
    capture_output(
      graph %>% print()) %>%
    str_split(pattern = "\n") %>%
    unlist()

  if (!is.null(line)) {
    captured_output <- captured_output[line]
  }

  captured_output
}

test_that("Printing a summary of an empty graph works", {

  graph <- create_graph()

  expect_equal(
    graph %>% get_printed_output(1),
    "DiagrammeR Graph // no nodes")

  expect_equal(
    graph %>% get_printed_output(2),
    "  -- empty graph (mode: directed)")

  expect_equal(
    create_graph(directed = FALSE) %>% get_printed_output(2),
    "  -- empty graph (mode: undirected)")

  expect_equal(
    graph %>% get_printed_output(3), "")

  expect_equal(
    graph %>% get_printed_output(4) %>% substr(1, 45) %>% stringr::str_trim(),
    "NODES / type: <unused> / label: <unused>")

  expect_equal(
    graph %>% get_printed_output(5) %>% substr(1, 45) %>% stringr::str_trim(),
    "-- no additional node attributes")

  expect_equal(
    graph %>% get_printed_output(6) %>% substr(1, 45) %>% stringr::str_trim(),
    "EDGES / rel: <unused>")

  expect_equal(
    graph %>% get_printed_output(7) %>% substr(1, 45) %>% stringr::str_trim(),
    "-- no additional edge attributes")

  expect_equal(
    graph %>% get_printed_output(8) %>% substr(1, 45) %>% stringr::str_trim(),
    "SELECTION / <none>")

  expect_equal(
    graph %>% get_printed_output(9) %>% substr(1, 45) %>% stringr::str_trim(),
    "CACHE / <none>")

  expect_equal(
    graph %>% get_printed_output(10) %>% substr(1, 45) %>% stringr::str_trim(),
    "STORED DFs / <none>")

  expect_equal(
    graph %>% get_printed_output(11) %>% substr(1, 30) %>% stringr::str_trim(),
    "GLOBAL ATTRS / 17 are set")

  expect_equal(
    graph %>% get_printed_output(12) %>% substr(1, 45) %>% stringr::str_trim(),
    "GRAPH ACTIONS / <none>")

  expect_equal(
    graph %>% get_printed_output(13) %>% substr(1, 45) %>% stringr::str_trim(),
    "GRAPH LOG / create_graph()")
})
