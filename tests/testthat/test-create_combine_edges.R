# Creating and combining edge data frames

test_that("a correct edge data frame is generated", {

  # Create `edges_1` edge data frame
  edges_1 <-
    create_edge_df(
      from = c(1, 1, 2, 3),
      to = c(2, 4, 4, 1),
      rel = "requires",
      color = "green",
      data = c(2.7, 8.9, 2.6, 0.6))

  # Create `edges_2` edge data frame
  edges_2 <-
    create_edge_df(
      from = c(5, 7, 8, 8),
      to = c(7, 8, 6, 5),
      rel = "receives",
      arrowhead = "dot",
      color = "red")

  # Expect that data frames are generated
  expect_s3_class(edges_1, "data.frame")

  expect_s3_class(edges_2, "data.frame")

  # Expect that each of the edfs has 4 rows
  expect_equal(
    nrow(edges_1), 4)

  expect_equal(
    nrow(edges_2), 4)

  # Expect that each of the edfs has 6 columns
  expect_equal(
    ncol(edges_1), 6)

  expect_equal(
    ncol(edges_2), 6)

  # Create an edge data frame using a vector with
  # length > 1 and length < length(from | to)
  edges_var_1 <-
    create_edge_df(
      from = c(1, 1, 2, 3),
      to = c(2, 4, 4, 1),
      rel = "requires",
      color = c("green", "green"),
      data = c(2.7, 8.9, 2.6, 0.6))

  # Expect that a data frame is generated
  expect_s3_class(
    edges_var_1, "data.frame")

  # Expect that the data frame has 4 rows
  expect_equal(
    nrow(edges_var_1), 4)

  # Expect that the `color` attribute is only
  # written twice and not repeated down
  expect_equal(
    edges_var_1$color, c("green", "green", NA, NA))

  # Create an edge data frame using a vector with
  # length > length(from | to)
  edges_var_2 <-
    create_edge_df(
      from = c(1, 1, 2, 3),
      to = c(2, 4, 4, 1),
      rel = "requires",
      color = c("green", "green",
                "green", "green",
                "green", "green"),
      data = c(2.7, 8.9, 2.6, 0.6))

  # Expect that a data frame is generated
  expect_s3_class(
    edges_var_2, "data.frame")

  # Expect that the data frame has 4 rows
  expect_equal(
    nrow(edges_var_2), 4)

  # Expect that the 'color' attribute is repeated down
  expect_equal(
    edges_var_2$color,
    c("green", "green", "green", "green"))
})

test_that("two edge data frames can be successfully combined", {

  # Create `edges_1` edge data frame
  edges_1 <-
    create_edge_df(
      from = c(1, 1, 2, 3),
      to = c(2, 4, 4, 1),
      rel = "requires",
      color = "green",
      data = c(2.7, 8.9, 2.6, 0.6))

  # Create `edges_2` edge data frame
  edges_2 <-
    create_edge_df(
      from = c(5, 7, 8, 8),
      to = c(7, 8, 6, 5),
      rel = "receives",
      arrowhead = "dot",
      color = "red")

  # Combine the 2 edge data frames
  all_edges <-
    combine_edfs(edges_1, edges_2)

  # Expect that a data frame is generated
  expect_s3_class(
    all_edges, "data.frame")

  # Expect that the combined edf has 8 rows
  expect_equal(
    nrow(all_edges), 8)

  # Expect that the combined edf has 7 columns
  expect_equal(
    ncol(all_edges), 7)
})

test_that("three edge data frames can be successfully combined", {

  # Create `edges_1` edge data frame
  edges_1 <-
    create_edge_df(
      from = c(1, 1),
      to = c(2, 4),
      rel = "requires",
      color = "green",
      data = c(2.7, 8.9))

  # Create `edges_2` edge data frame
  edges_2 <-
    create_edge_df(
      from = c(2, 3),
      to = c(4, 1),
      rel = "requires",
      color = "green",
      data = c(2.6, 0.6))

  # Create `edges_3` edge data frame
  edges_3 <-
    create_edge_df(
      from = c(5, 7, 8, 8),
      to = c(7, 8, 6, 5),
      rel = "receives",
      arrowhead = "dot",
      color = "red")

  # Combine the 3 edge data frames
  all_edges <-
    combine_edfs(
      edges_1, edges_2, edges_3)

  # Expect that a data frame is generated
  expect_s3_class(
    all_edges, "data.frame")

  # Expect that the combined edf has 8 rows
  expect_equal(
    nrow(all_edges), 8)

  # Expect that the combined edf has 7 columns
  expect_equal(
    ncol(all_edges), 7)
})
