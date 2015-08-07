context("Triggering scripts within a graph series")

test_that("a script can be added to a graph series object", {

  # Create script to generate a randomized graph object
  script <- '
    nodes <-
    create_nodes(nodes = LETTERS,
                 label = TRUE,
                 type = c(rep("a_to_g", 7),
                          rep("h_to_p", 9),
                          rep("q_to_x", 8),
                          rep("y_and_z",2)))

    edges <-
      create_edges(from = sample(LETTERS, replace = TRUE),
                   to = sample(LETTERS, replace = TRUE),
                   label = "edge",
                   relationship = "letter_to_letter")

    graph <- create_graph(nodes_df = nodes,
                          edges_df = edges,
                          graph_attrs = "layout = neato",
                          node_attrs = c("fontname = Helvetica",
                                         "shape = circle"))

    _SELF_ <- add_to_series(graph = graph,
                            graph_series = _SELF_)'

  # Create an empty graph series of the 'sequential' type and add
  # the "test_graph_generation_script" script as one of the graph series'
  # 'series scripts'
  series_sequential <- create_series(series_type = "sequential",
                                     series_scripts = script)

  # Trigger the script, calling the single script by number
  series_sequential <-
    trigger_script(graph_series = series_sequential,
                   script = 1)
#
#   # Expect that a graph series object is returned
#   expect_is(series_sequential_numbered_addition, "dgr_graph_1D")
#
#   # Expect that the graph series is of the "sequential" type
#   expect_equal(series_sequential_numbered_addition$series_type, "sequential")
#
#   # Expect the graph series name to be not set
#   expect_null(series_sequential_numbered_addition$series_name)
#
#   # Expect the graph series to contain the script object
#   expect_equal(series_sequential_numbered_addition$series_scripts[1],
#                test_script)
#
#   # Expect that a graph is produced
#   expect_is(series_sequential_numbered_addition$graphs[[1]], "dgr_graph")
#
#   # Expect that only a single graph was produced
#   expect_equal(graph_count(series_sequential_numbered_addition), 1L)
#
#   # Expect that the graph series won't be empty after triggering the script
#   expect_more_than(nrow(series_sequential_numbered_addition$graphs[[1]]$nodes_df),
#                    0)
#   expect_more_than(nrow(series_sequential_numbered_addition$graphs[[1]]$edges_df),
#                    0)
#
#   # Expect that the graph produced has specified values for its components
#   expect_null(series_sequential_numbered_addition$graphs[[1]]$graph_name)
#   expect_null(series_sequential_numbered_addition$graphs[[1]]$graph_time)
#   expect_null(series_sequential_numbered_addition$graphs[[1]]$graph_tz)
})
