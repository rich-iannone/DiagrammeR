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
      fillcolor = sample(c("aqua", "gray80",
                           "pink", "lightgreen",
                           "azure", "yellow"),
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
  expect_is(rendered_graph, c("grViz", "htmlwidget"))
})

test_that("exporting Graphviz DOT code is indeed possible", {

  # Create a node data frame
  ndf <-
    create_node_df(
      n = 26,
      type = "letter",
      shape = sample(c("circle", "rectangle"),
                     length(1:26),
                     replace = TRUE),
      fillcolor = sample(c("aqua", "gray80",
                           "pink", "lightgreen",
                           "azure", "yellow"),
                         length(1:26),
                         replace = TRUE))

  # Create an edge data frame
  edf <-
    create_edge_df(
      from = sample(1:26, replace = TRUE),
      to = sample(1:26, replace = TRUE),
      rel = "letter_to_letter")

  # Create the graph object using the node and
  # edge data frames
  graph <-
    create_graph(
      nodes_df = ndf,
      edges_df = edf)

  # Output the DOT code as a character object
  graph_dot_output <-
    render_graph(graph, output = "DOT")

  # Expect that the DOT code exported is the same
  # as that contained in the graph object
  expect_equal(graph_dot_output, graph$dot_code)
})

test_that("rendering a graph from a series is also possible", {

  # Create a set of graphs for a graph series
  graph_1 <-
    create_graph() %>%
    add_node(1) %>%
    add_node(2) %>%
    add_node(3) %>%
    add_edge(1, 3) %>%
    add_edge(1, 2) %>%
    add_edge(2, 3)

  graph_2 <-
    graph_1 %>%
    add_node(4) %>%
    add_edge(4, 3)

  graph_3 <-
    graph_2 %>%
    add_node(5) %>%
    add_edge(5, 2)

  # Create an empty graph series
  series <-
    create_series(series_type = "sequential")

  # Add graphs to the graph series
  series <- graph_1 %>% add_to_series(series)
  series <- graph_2 %>% add_to_series(series)
  series <- graph_3 %>% add_to_series(series)

  # View the second graph from the graph series in
  # the RStudio Viewer
  series_graph_2 <-
    render_graph_from_series(
      graph_series = series,
      graph_no = 2)

  # Expect error when rendering graph 4 from the
  # series (which doesn't exist)
  expect_error(
    render_graph_from_series(
      graph_series = series,
      graph_no = 4))

  # Expect that each of the graphs is different
  expect_true(
    render_graph_from_series(
      graph_series = series,
      graph_no = 1)$x$diagram !=
      render_graph_from_series(
        graph_series = series,
        graph_no = 2)$x$diagram)

  expect_true(
    render_graph_from_series(
      graph_series = series,
      graph_no = 2)$x$diagram !=
      render_graph_from_series(
        graph_series = series,
        graph_no = 3)$x$diagram)

  expect_true(
    render_graph_from_series(
      graph_series = series,
      graph_no = 1)$x$diagram !=
      render_graph_from_series(
        graph_series = series,
        graph_no = 3)$x$diagram)

  # Create an empty graph series
  empty_series <-
    create_series(series_type = "sequential")

  # Expect an error if there are no graphs
  # in the series
  expect_error(
    render_graph_from_series(
      graph_series = empty_series))
})
