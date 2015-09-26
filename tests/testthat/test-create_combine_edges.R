context("Creating and combining edge data frames")

test_that("a correct edge data frame is generated", {

  # Create 'edges_1' edge data frame
  edges_1 <-
    create_edges(from = c("a", "a", "b", "c"),
                 to = c("b", "d", "d", "a"),
                 rel = "requires",
                 color = "green",
                 data = c(2.7, 8.9, 2.6, 0.6))

  # Create 'edges_2' edge data frame
  edges_2 <-
    create_edges(from = c("e", "g", "h", "h"),
                 to = c("g", "h", "f", "e"),
                 rel = "receives",
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

  # Create an edge data frame using a vector with length > 1 and
  # length < length(from | to)
  edges_var_1 <-
    create_edges(from = c("a", "a", "b", "c"),
                 to = c("b", "d", "d", "a"),
                 rel = "requires",
                 color = c("green", "green"),
                 data = c(2.7, 8.9, 2.6, 0.6))

  # Expect that a data frame is generated
  expect_is(edges_var_1, "data.frame")

  # Expect that the data frame has 4 rows
  expect_equal(nrow(edges_var_1), 4L)

  # Expect that the 'color' attribute is only written twice and not
  # repeated down
  expect_equal(edges_var_1$color, c("green", "green", "", ""))

  # Create an edge data frame using a vector with
  # length > length(from | to)
  edges_var_2 <-
    create_edges(from = c("a", "a", "b", "c"),
                 to = c("b", "d", "d", "a"),
                 rel = "requires",
                 color = c("green", "green",
                           "green", "green",
                           "green", "green"),
                 data = c(2.7, 8.9, 2.6, 0.6))

  # Expect that a data frame is generated
  expect_is(edges_var_2, "data.frame")

  # Expect that the data frame has 4 rows
  expect_equal(nrow(edges_var_2), 4L)

  # Expect that the 'color' attribute is only written twice and not
  # repeated down
  expect_equal(edges_var_2$color, c("green", "green", "green", "green"))
})

test_that("two edge data frames can be successfully combined", {

  # Create 'edges_1' edge data frame
  edges_1 <-
    create_edges(from = c("a", "a", "b", "c"),
                 to = c("b", "d", "d", "a"),
                 rel = "requires",
                 color = "green",
                 data = c(2.7, 8.9, 2.6, 0.6))

  # Create 'edges_2' edge data frame
  edges_2 <-
    create_edges(from = c("e", "g", "h", "h"),
                 to = c("g", "h", "f", "e"),
                 rel = "receives",
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
                 rel = "requires",
                 color = "green",
                 data = c(2.7, 8.9))

  # Create 'edges_2' edge data frame
  edges_2 <-
    create_edges(from = c("b", "c"),
                 to = c("d", "a"),
                 rel = "requires",
                 color = "green",
                 data = c(2.6, 0.6))

  # Create 'edges_3' edge data frame
  edges_3 <-
    create_edges(from = c("e", "g", "h", "h"),
                 to = c("g", "h", "f", "e"),
                 rel = "receives",
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
