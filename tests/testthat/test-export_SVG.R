context("Exporting a graph to SVG")

test_that("exporting a graph to SVG code is possible", {

  library(V8)

  # Create a node data frame
  nodes <-
    create_nodes(nodes = c("a", "b", "c", "d"),
                 label = FALSE,
                 type = "lower",
                 style = "filled",
                 color = "aqua",
                 shape = c("circle", "circle",
                           "rectangle", "rectangle"),
                 data = c(3.5, 2.6, 9.4, 2.7))

  # Create an edge data frame
  edges <-
    create_edges(from = c("a", "b", "c"),
                 to = c("d", "c", "a"),
                 rel = "leading_to")

  # Create the graph object using the node and edge data frames
  graph <- create_graph(nodes_df = nodes,
                        edges_df = edges,
                        graph_attrs = "layout = dot",
                        node_attrs = "fontname = Helvetica",
                        edge_attrs = c("color = blue",
                                       "arrowsize = 2"))

  # Export as SVG by using 'exportSVG'
  svg_exportSVG <-
    exportSVG(grViz(graph$dot_code, engine = 'dot'))

  # Expect that a character object of length 1 is created
  expect_is(svg_exportSVG, "character")
  expect_equal(length(svg_exportSVG), 1L)

  # Export as SVG by using 'render_graph'
  svg_render_graph <-
    render_graph(graph, output = "SVG")

  # Expect that the different methods will create the same SVG text
  expect_equal(svg_exportSVG, svg_render_graph)
})
