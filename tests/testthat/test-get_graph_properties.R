context("Get graph properties")

test_that("Getting the graph object log is possible", {

  # Create a random graph and add
  # an edge as well
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      set_seed = 23) %>%
    add_edge(
      from = 5,
      to = 10)

  # Get the graph's log data
  graph_log <- get_graph_log(graph)

  # Expect that the graph log is provided
  # as a tibble object
  expect_is(
    graph_log, "tbl_df")

  # Expect certain column names for the
  # `graph_log` object
  expect_identical(
    colnames(graph_log),
    c("version_id", "function_used", "time_modified",
      "duration", "nodes", "edges", "d_n", "d_e"))

  # Expect 2 rows in this graph log
  expect_equal(
    nrow(graph_log), 3)
})
