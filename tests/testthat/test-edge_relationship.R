context("Edge relationships")

test_that("a specified edge relationship can be read from graph objects", {

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

  # Create an edge data frame with no edge relationships set
  edges_no_rel <-
    create_edges(from = c("a", "b", "c"),
                 to = c("d", "c", "a"))

  # Create the graph object using the node and edge data frames
  graph_no_rel <-
    create_graph(nodes_df = nodes,
                 edges_df = edges_no_rel,
                 graph_attrs = "layout = dot",
                 node_attrs = "fontname = Helvetica",
                 edge_attrs = c("color = blue",
                                "arrowsize = 2"))

  # Expect an NA value to be returned when reading an unset
  # relationship for an edge
  expect_true(is.na(edge_relationship(graph_no_rel,
                                      from = "a",
                                      to = "d",
                                      action = "read")))
})

test_that("removing an edge relationship is possible", {

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

  # Remove the edge relationship across the 'a->d' edge using
  # the "delete" action (synonymous with "remove" and "drop")
  graph_ad_remove_1 <-
    edge_relationship(graph,
                      from = "a",
                      to = "d",
                      action = "delete")

  # Remove the edge relationship across the 'a->d' edge using
  # the "remove" action (synonymous with "delete" and "drop")
  graph_ad_remove_2 <-
    edge_relationship(graph,
                      from = "a",
                      to = "d",
                      action = "remove")

  # Remove the edge relationship across the 'a->d' edge using
  # the "drop" action (synonymous with "delete" and "remove")
  graph_ad_remove_3 <-
    edge_relationship(graph,
                      from = "a",
                      to = "d",
                      action = "drop")

  # Expect that the resulting data frames will be the same
  expect_equal(graph_ad_remove_1, graph_ad_remove_2)
  expect_equal(graph_ad_remove_2, graph_ad_remove_3)
  expect_equal(graph_ad_remove_1, graph_ad_remove_3)

  # Expect that a relationship across the 'a->d' edge won't be present
  # in all 3 instances
  expect_true(is.na(edge_relationship(graph_ad_remove_1,
                                      from = "a",
                                      to = "d",
                                      action = "read")))

  expect_true(is.na(edge_relationship(graph_ad_remove_2,
                                      from = "a",
                                      to = "d",
                                      action = "read")))

  expect_true(is.na(edge_relationship(graph_ad_remove_3,
                                      from = "a",
                                      to = "d",
                                      action = "read")))
})
