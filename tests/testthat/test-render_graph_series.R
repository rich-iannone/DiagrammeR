context("Rendering a graph object")

test_that("rendering a graph is indeed possible", {

  # Create a node data frame
  ndf <-
    create_node_df(
      n = 26,
      type = "letter",
      shape = sample(c("circle", "rectangle"),
                     length(1:26),
                     replace = TRUE),
      fillcolor = sample(c("steelblue", "gray80",
                           "#FFC0CB", "green",
                           "azure", NA),
                         length(1:26),
                         replace = TRUE))

  # Create an edge data frame
  edf <-
    create_edge_df(
      from = sample(1:26, replace = TRUE),
      to = sample(1:26, replace = TRUE),
      relationship = "letter_to_letter")

  # Create the graph object using the node and
  # edge data frames
  graph <-
    create_graph(
      nodes_df = ndf,
      edges_df = edf)

  # Render the graph object and create a
  # `grViz`/`htmlwidget` object
  rendered_graph <- render_graph(graph)

  # Expect that the `rendered_graph` object inherits
  # from `grViz` & `htmlwidget`
  expect_s3_class(
    rendered_graph, c("grViz", "htmlwidget"))
})

test_that("rendering a graph from a series is also possible", {

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

  # Create an empty graph series
  series <-
    create_graph_series(series_type = "sequential")

  # Add graphs to the graph series
  series <-
    series %>%
    add_graph_to_graph_series(
      graph = graph_1) %>%
    add_graph_to_graph_series(
      graph = graph_2) %>%
    add_graph_to_graph_series(
      graph = graph_3)

  # View the second graph from the graph series in
  # the RStudio Viewer
  series_graph_2 <-
    render_graph_from_graph_series(
      graph_series = series,
      graph_no = 2)

  # Expect error when rendering graph 4 from the
  # series (which doesn't exist)
  expect_error(
    render_graph_from_graph_series(
      graph_series = series,
      graph_no = 4))

  # Expect that each of the graphs is different
  expect_true(
    render_graph_from_graph_series(
      graph_series = series,
      graph_no = 1)$x$diagram !=
      render_graph_from_graph_series(
        graph_series = series,
        graph_no = 2)$x$diagram)

  expect_true(
    render_graph_from_graph_series(
      graph_series = series,
      graph_no = 2)$x$diagram !=
      render_graph_from_graph_series(
        graph_series = series,
        graph_no = 3)$x$diagram)

  expect_true(
    render_graph_from_graph_series(
      graph_series = series,
      graph_no = 1)$x$diagram !=
      render_graph_from_graph_series(
        graph_series = series,
        graph_no = 3)$x$diagram)

  # Create an empty graph series
  empty_series <-
    create_graph_series(series_type = "sequential")

  # Expect an error if there are no graphs
  # in the series
  expect_error(
    render_graph_from_graph_series(
      graph_series = empty_series))
})
