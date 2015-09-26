context("Scaling values in node and edge data frames")

test_that("values are scaled correctly", {

  # Create a data frame for nodes and insert a data column
  nodes <- create_nodes(nodes = 1:7,
                        label = FALSE,
                        type = "example",
                        data = c(1, 2, 3, 4, 5, 10, 20))

  # Use the 'scale_nodes' function to create an attribute column
  # that's scaled to data in another column
  nodes <- scale_nodes(nodes_df = nodes,
                       to_scale = nodes$data,
                       node_attr = "penwidth",
                       range = c(2, 5))

  # Create a data frame for edges and insert a data column
  edges <- create_edges(from = c("a", "b", "c"),
                        to = c("d", "d", "a"),
                        rel = "given_to",
                        data = c(2.5, 3.2, 7.9))

  # Use the 'scale_edges' function to create an attribute column
  # that's scaled to data in another column
  edges <- scale_edges(edges_df = edges,
                       to_scale = edges$data,
                       edge_attr = "penwidth",
                       range = c(1, 5))

  # Expect that a data frame is generated when using 'scale_nodes'
  expect_true(class(nodes) == "data.frame")

  # Expect that a data frame is generated when using 'scale_edges'
  expect_true(class(edges) == "data.frame")

  # Expect that columns called 'penwidth' are generated in both cases
  expect_true("penwidth" %in% colnames(nodes))
  expect_true("penwidth" %in% colnames(edges))

  # Expect that the 'penwidth' columns are numeric
  expect_true(class(nodes$penwidth) == "numeric")
  expect_true(class(edges$penwidth) == "numeric")

  # Expect that the final node data frame has 7 rows
  expect_equal(nrow(nodes), 7L)

  # Expect that the final edge data frame has 3 rows
  expect_equal(nrow(edges), 3L)

  # Expect that the final node data frame has 5 columns
  expect_equal(ncol(nodes), 5L)

  # Expect that the final node data frame has 5 columns
  expect_equal(ncol(edges), 5L)
})
