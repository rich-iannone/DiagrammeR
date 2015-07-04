context("creating and combining node data frames")

test_that("a correct node data frame is generated", {

  # Create 'nodes_1' node data frame
  nodes_1 <-
    create_nodes(nodes = c("a", "b", "c", "d"),
                 label = FALSE,
                 type = "lower",
                 style = "filled",
                 color = "aqua",
                 shape = c("circle", "circle",
                           "rectangle", "rectangle"),
                 data = c(3.5, 2.6, 9.4, 2.7))

  # Create 'nodes_2' node data frame
  nodes_2 <-
    create_nodes(nodes = c("e", "f", "g", "h"),
                 label = FALSE,
                 type = "upper",
                 style = "filled",
                 color = "red",
                 shape = "triangle",
                 data = c(0.5, 3.9, 3.7, 8.2))

  # Expect that each of the node data frames has 4 rows
  expect_equal(nrow(nodes_1), 4L)
  expect_equal(nrow(nodes_2), 4L)

})
