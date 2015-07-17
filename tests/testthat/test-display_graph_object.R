context("Displaying the properties of a graph object")

test_that("the display of a graph object works correctly", {

  # Create a simple graph and display a visual summary
  # of simple graph properties
  nodes <-
    create_nodes(nodes = c("a", "b", "c", "d"),
                 label = FALSE,
                 type = "letter")

  edges <-
    create_edges(from = c("a", "b", "c"),
                 to = c("d", "c", "a"),
                 relationship = "connected_to")

  graph <- create_graph(nodes_df = nodes,
                        edges_df = edges)

  graph_object_for_display <-
    display_graph_object(graph, width = 640)

  # Expect that the "display_graph_object" function returns NULL
  expect_null(graph_object_for_display)
})
