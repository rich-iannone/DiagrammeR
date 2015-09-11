context("Triggering a script in a graph series")

test_that("script triggers result in modification of the graph series", {

  # Create a simple script
  test_series_script <-
  '
  # If there is no graph available in the series, create a an empty graph
  if (graph_count(graph_series = _SELF_) == 0){
    _SELF_ <- add_to_series(graph = create_graph(),
                                 graph_series = _SELF_)
  }

  # Add a random node to the graph
  _SELF_$graphs[[1]] <-
    add_node(graph = _SELF_$graphs[[1]],
           node = paste(sample(letters, 8), collapse = ""))

  return(_SELF_)
  '

  # Create an empty graph series with the script loaded
  test_series <- create_series(series_scripts = test_series_script)


  # Trigger the script and modify the graph series three times
  test_series_1 <-
      trigger_script(graph_series = test_series,
                     script = 1)

  test_series_2 <-
    trigger_script(graph_series = test_series_1,
                   script = 1)

  test_series_3 <-
    trigger_script(graph_series = test_series_2,
                   script = 1)

  # Expect that a graph series object is returned in all three cases
  # Expect an object of class "dgr_graph_1D"
  expect_is(test_series_1, "dgr_graph_1D")
  expect_is(test_series_2, "dgr_graph_1D")
  expect_is(test_series_3, "dgr_graph_1D")

  # Expect that the series type is sequential
  expect_equal(test_series_1$series_type, "sequential")
  expect_equal(test_series_2$series_type, "sequential")
  expect_equal(test_series_3$series_type, "sequential")

  # Expect that the series name is NULL
  expect_null(test_series_1$series_name)
  expect_null(test_series_2$series_name)
  expect_null(test_series_3$series_name)

  # Expect that the graph count is 1
  expect_equal(graph_count(test_series_1), 1)
  expect_equal(graph_count(test_series_2), 1)
  expect_equal(graph_count(test_series_3), 1)

  # Expect increasing counts of nodes
  expect_equal(series_info(test_series_1)$nodes, 1)
  expect_equal(series_info(test_series_2)$nodes, 2)
  expect_equal(series_info(test_series_3)$nodes, 3)
})
