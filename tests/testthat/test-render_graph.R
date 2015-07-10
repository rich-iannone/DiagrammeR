context("Rendering a graph object")

test_that("rendering a graph is indeed possible", {

  # Create a node data frame
  nodes <-
    create_nodes(nodes = LETTERS,
                 type = "letter",
                 shape = sample(c("circle", "rectangle"),
                                length(LETTERS),
                                replace = TRUE),
                 fillcolor = sample(c("aqua", "gray80",
                                      "pink", "lightgreen",
                                      "azure", "yellow"),
                                    length(LETTERS),
                                    replace = TRUE))

  # Create an edge data frame
  edges <-
    create_edges(from = sample(LETTERS, replace = TRUE),
                 to = sample(LETTERS, replace = TRUE),
                 relationship = "letter_to_letter")

  # Create the graph object using the node and edge data frames
  graph <-
    create_graph(nodes_df = nodes,
                 edges_df = edges,
                 graph_attrs = "layout = twopi",
                 node_attrs = c("fontname = Helvetica",
                                "style = filled"),
                 edge_attrs = c("color = gray20",
                                "arrowsize = 0.5"))

  # Render the graph object and create a 'grViz'/'htmlwidget' object
  rendered_graph <- render_graph(graph)

  # Expect that the 'rendered_graph' object inherits from 'grViz' & 'htmlwidget'
  expect_is(rendered_graph, c("grViz", "htmlwidget"))
})
