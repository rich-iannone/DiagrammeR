context("Edge relationships")

test_that("edge relationships can be determined", {

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
                 relationship = "leading_to")

  # Create the graph object using the node and edge data frames
  graph <- create_graph(nodes_df = nodes,
                        edges_df = edges,
                        graph_attrs = "layout = dot",
                        node_attrs = "fontname = Helvetica",
                        edge_attrs = c("color = blue",
                                       "arrowsize = 2"))

  # Expect that using a 'read' action for an edge relationship for
  # an edge that doesn't exist will throw an error
  expect_error(edge_relationship(graph,
                                 from = "a",
                                 to = "b",
                                 action = "read"))

  # Expect that using a 'read' action for an edge relationship for
  # with a relationship set will return the relationship label
  expect_equal(edge_relationship(graph,
                                 from = "a",
                                 to = "d",
                                 action = "read"),
               "leading_to")
})
