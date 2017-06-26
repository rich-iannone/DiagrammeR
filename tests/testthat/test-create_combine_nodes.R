context("Creating and combining node data frames")

test_that("a correct node data frame is generated", {
  # Create 'nodes_1' node data frame
  nodes_1 <-
    create_node_df(
      n = 4,
      label = NULL,
      type = "lower",
      style = "filled",
      color = "aqua",
      shape = c("circle", "circle",
                "rectangle", "rectangle"),
      data = c(3.5, 2.6, 9.4, 2.7)
    )

  # Create 'nodes_2' node data frame
  nodes_2 <-
    create_node_df(
      n = 4,
      label = NULL,
      type = "upper",
      style = "filled",
      color = "red",
      shape = "triangle",
      data = c(0.5, 3.9, 3.7, 8.2)
    )

  # Expect that each of the node data frames has 4 rows
  expect_equal(nrow(nodes_1), 4)
  expect_equal(nrow(nodes_2), 4)

  # Expect that each of the node data frames has 7 columns
  expect_equal(ncol(nodes_1), 7)
  expect_equal(ncol(nodes_2), 7)

  # Expect that 'label = NULL' produces blank label columns
  expect_equal(nodes_1$label, rep(as.character(NA), 4))
  expect_equal(nodes_2$label, rep(as.character(NA), 4))

  # Expect that a single value repeats across rows
  expect_true(all(nodes_1$type == rep("lower", 4)))
  expect_true(all(nodes_2$color == rep("red", 4)))

  # Expect that the numeric `data` values are numeric
  expect_is(nodes_1$data, "numeric")
  expect_is(nodes_2$data, "numeric")

  # Create a node data frame using a vector with length > 1 and
  # length < length(from | to)
  nodes_var_1 <-
    create_node_df(n = 4,
                   color = c("green", "green"))

  # Expect that a data frame is generated
  expect_is(nodes_var_1, "data.frame")

  # Expect that the data frame has 4 rows
  expect_equal(nrow(nodes_var_1), 4)

  # Expect that the 'color' attribute is only written twice and not
  # repeated down
  expect_equal(nodes_var_1$color, c("green", "green", "", ""))

  # Create a node data frame using a vector with
  # length > length(from | to)
  nodes_var_2 <-
    create_node_df(
      n = 4,
      color = c("green", "green",
                "green", "green",
                "green", "green")
    )

  # Expect that a data frame is generated
  expect_is(nodes_var_2, "data.frame")

  # Expect that the data frame has 4 rows
  expect_equal(nrow(nodes_var_2), 4)

  # Expect that the `color` attribute is trimmed to
  # the correct number of rows
  expect_equal(nodes_var_2$color,
               c("green", "green", "green", "green"))

  # Expect an error if not supplying an integer
  expect_error(create_node_df(n = "a"))

  # Expect an error if supplying 2 values for `n`
  expect_error(create_node_df(n = c(1, 2)))

  # Create a node data frame using a vector with
  # `type` having length > n
  nodes_var_3 <-
    create_node_df(n = 4,
                   type = c("a", "a", "b", "a", "c"))

  # Expect the `type` values to be the first four
  expect_equal(nodes_var_3$type, c("a", "a", "b", "a"))
})

test_that("node data frames can be successfully combined", {
  # Create `ndf_1` node data frame
  ndf_1 <-
    create_node_df(
      n = 4,
      label = NULL,
      type = "lower",
      style = "filled",
      color = "aqua",
      shape = c("circle", "circle",
                "rectangle", "rectangle"),
      data = c(3.5, 2.6, 9.4, 2.7)
    )

  # Create 'nodes_2' node data frame
  ndf_2 <-
    create_node_df(
      n = 4,
      label = NULL,
      type = "upper",
      style = "filled",
      color = "red",
      shape = "triangle",
      data = c(0.5, 3.9, 3.7, 8.2)
    )

  # Combine the 2 node data frames
  all_nodes <- combine_ndfs(ndf_1, ndf_2)

  # Expect that the combined node data frame has 8 rows
  expect_equal(nrow(all_nodes), 8)

  # Expect that the combined node data frame has 7 columns
  expect_equal(ncol(all_nodes), 7)

  # Expect that the 'label' columns has spaces for labels
  expect_equal(all_nodes$label, rep(as.character(NA), 8))

  # Expect that the rows combined correctly
  expect_true(all(all_nodes[, 1] ==
                    c(1, 2, 3, 4, 5, 6, 7, 8)))
  expect_true(all(c(ndf_1[, 2], ndf_2[, 2]) ==
                    all_nodes[, 2]))
  expect_equal(c(ndf_1[, 3], ndf_2[, 3]),
               all_nodes[, 3])
  expect_true(all(c(ndf_1[, 4], ndf_2[, 4]) ==
                    all_nodes[, 4]))
  expect_true(all(c(ndf_1[, 5], ndf_2[, 5]) ==
                    all_nodes[, 5]))
  expect_true(all(c(ndf_1[, 6], ndf_2[, 6]) ==
                    all_nodes[, 6]))
  expect_true(all(c(ndf_1[, 7], ndf_2[, 7]) ==
                    all_nodes[, 7]))
})
