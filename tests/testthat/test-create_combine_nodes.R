context("Creating and combining node data frames")

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

  # Expect that each of the node data frames has 7 columns
  expect_equal(ncol(nodes_1), 7L)
  expect_equal(ncol(nodes_2), 7L)

  # Expect that 'label = FALSE' produces blank label columns
  expect_true(all(nodes_1$label == rep("", 4)))
  expect_true(all(nodes_2$label == rep("", 4)))

  # Expect that a single value repeats across rows
  expect_true(all(nodes_1$type == rep("lower", 4)))
  expect_true(all(nodes_2$color == rep("red", 4)))

  # Expect that the numeric 'data' values are numeric
  expect_true(class(nodes_1$data) == "numeric")
  expect_true(class(nodes_2$data) == "numeric")

  # Create a node data frame using a vector with length > 1 and
  # length < length(from | to)
  nodes_var_1 <-
    create_nodes(nodes = c("a", "a", "b", "c"),
                 color = c("green", "green"))

  # Expect that a data frame is generated
  expect_is(nodes_var_1, "data.frame")

  # Expect that the data frame has 4 rows
  expect_equal(nrow(nodes_var_1), 4L)

  # Expect that the 'color' attribute is only written twice and not
  # repeated down
  expect_equal(nodes_var_1$color, c("green", "green", "", ""))

  # Create a node data frame using a vector with
  # length > length(from | to)
  nodes_var_2 <-
    create_nodes(nodes = c("a", "a", "b", "c"),
                 color = c("green", "green",
                           "green", "green",
                           "green", "green"))

  # Expect that a data frame is generated
  expect_is(nodes_var_2, "data.frame")

  # Expect that the data frame has 4 rows
  expect_equal(nrow(nodes_var_2), 4L)

  # Expect that the 'color' attribute is trimmed to correct number
  # of rows
  expect_equal(nodes_var_2$color, c("green", "green", "green", "green"))

})

test_that("node data frames can be successfully combined", {

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

  # Combine the 2 node data frames
  all_nodes <- combine_nodes(nodes_1, nodes_2)

  # Expect that the combined node data frame has 8 rows
  expect_equal(nrow(all_nodes), 8L)

  # Expect that the combined node data frame has 7 columns
  expect_equal(ncol(all_nodes), 7L)

  # Expect that the 'label' columns has spaces for labels
  expect_true(all(all_nodes$label == rep("", 8)))

  # Expect that the rows combined correctly
  expect_true(all(c(nodes_1[,1], nodes_2[,1]) ==
                    all_nodes[,1]))
  expect_true(all(c(nodes_1[,2], nodes_2[,2]) ==
                    all_nodes[,2]))
  expect_true(all(c(nodes_1[,3], nodes_2[,3]) ==
                    all_nodes[,3]))
  expect_true(all(c(nodes_1[,4], nodes_2[,4]) ==
                    all_nodes[,4]))
  expect_true(all(c(nodes_1[,5], nodes_2[,5]) ==
                    all_nodes[,5]))
  expect_true(all(c(nodes_1[,6], nodes_2[,6]) ==
                    all_nodes[,6]))
  expect_true(all(c(nodes_1[,7], nodes_2[,7]) ==
                    all_nodes[,7]))
})
