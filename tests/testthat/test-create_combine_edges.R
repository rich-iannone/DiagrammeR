context("Creating and combining edge data frames")

test_that("a correct edge data frame is generated", {

  # Create 'edges_1' edge data frame
  edges_1 <-
    create_edges(from = c("a", "a", "b", "c"),
                 to = c("b", "d", "d", "a"),
                 relationship = "requires",
                 color = "green",
                 data = c(2.7, 8.9, 2.6, 0.6))

  # Create 'edges_2' edge data frame
  edges_2 <-
    create_edges(from = c("e", "g", "h", "h"),
                 to = c("g", "h", "f", "e"),
                 relationship = "receives",
                 arrowhead = "dot",
                 color = "red")

  # Expect that data frames are generated
  expect_true(class(edges_1) == "data.frame")
  expect_true(class(edges_2) == "data.frame")

  # Expect that each of the edge data frames has 4 rows
  expect_equal(nrow(edges_1), 4L)
  expect_equal(nrow(edges_2), 4L)

  # Expect that each of the edge data frames has 5 columns
  expect_equal(ncol(edges_1), 5L)
  expect_equal(ncol(edges_2), 5L)
})

test_that("two edge data frames can be successfully combined", {

  # Create 'edges_1' edge data frame
  edges_1 <-
    create_edges(from = c("a", "a", "b", "c"),
                 to = c("b", "d", "d", "a"),
                 relationship = "requires",
                 color = "green",
                 data = c(2.7, 8.9, 2.6, 0.6))

  # Create 'edges_2' edge data frame
  edges_2 <-
    create_edges(from = c("e", "g", "h", "h"),
                 to = c("g", "h", "f", "e"),
                 relationship = "receives",
                 arrowhead = "dot",
                 color = "red")

  # Combine the 2 edge data frames
  all_edges <- combine_edges(edges_1, edges_2)

  # Expect that a data frame is generated
  expect_true(class(all_edges) == "data.frame")

  # Expect that the combined edge data frame has 8 rows
  expect_equal(nrow(all_edges), 8L)

  # Expect that the combined edge data frame has 6 columns
  expect_equal(ncol(all_edges), 6L)
})

test_that("three edge data frames can be successfully combined", {

  # Create 'edges_1' edge data frame
  edges_1 <-
    create_edges(from = c("a", "a"),
                 to = c("b", "d"),
                 relationship = "requires",
                 color = "green",
                 data = c(2.7, 8.9))

  # Create 'edges_2' edge data frame
  edges_2 <-
    create_edges(from = c("b", "c"),
                 to = c("d", "a"),
                 relationship = "requires",
                 color = "green",
                 data = c(2.6, 0.6))

  # Create 'edges_3' edge data frame
  edges_3 <-
    create_edges(from = c("e", "g", "h", "h"),
                 to = c("g", "h", "f", "e"),
                 relationship = "receives",
                 arrowhead = "dot",
                 color = "red")

  # Combine the 2 edge data frames
  all_edges <- combine_edges(edges_1, edges_2, edges_3)

  # Expect that a data frame is generated
  expect_true(class(all_edges) == "data.frame")

  # Expect that the combined edge data frame has 8 rows
  expect_equal(nrow(all_edges), 8L)

  # Expect that the combined edge data frame has 6 columns
  expect_equal(ncol(all_edges), 6L)
})
