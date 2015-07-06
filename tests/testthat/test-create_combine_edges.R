context("Creating and combining edge data frames")

test_that("a correct edge data frame is generated", {

  # Create 'edges_1' edge data frame
  edges_1 <-
    create_edges(from = c("a", "a", "b", "c"),
                 to = c("b", "d", "d", "a"),
                 relationship = "requires",
                 color = "green",
                 data = c(2.7, 8.9, 2.6, 0.6))

  # Result
  #>   from to relationship color data
  #> 1    a  b     requires green  2.7
  #> 2    a  d     requires green  8.9
  #> 3    b  d     requires green  2.6
  #> 4    c  a     requires green  0.6

  # Create 'edges_2' edge data frame
  edges_2 <-
    create_edges(from = c("e", "g", "h", "h"),
                 to = c("g", "h", "f", "e"),
                 relationship = "receives",
                 arrowhead = "dot",
                 color = "red")

  # Result
  #>   from to relationship arrowhead color
  #> 1    e  g     receives       dot   red
  #> 2    g  h     receives       dot   red
  #> 3    h  f     receives       dot   red
  #> 4    h  e     receives       dot   red

  # Expect that data frames are generated
  expect_true(class(edges_1) == "data.frame")
  expect_true(class(edges_2) == "data.frame")

  # Expect that each of the edge data frames has 4 rows
  expect_equal(nrow(edges_1), 4L)
  expect_equal(nrow(edges_2), 4L)

  # Expect that each of the node data frames has 5 columns
  expect_equal(ncol(edges_1), 5L)
  expect_equal(ncol(edges_2), 5L)

})

test_that("edge data frames can be successfully combined", {

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

  # Result
  #>   from to relationship color data arrowhead
  #> 1    a  b     requires green  2.7
  #> 2    a  d     requires green  8.9
  #> 3    b  d     requires green  2.6
  #> 4    c  a     requires green  0.6
  #> 5    e  g     receives   red            dot
  #> 6    g  h     receives   red            dot
  #> 7    h  f     receives   red            dot
  #> 8    h  e     receives   red            dot

  # Expect that the combined edge data frame has 8 rows
  expect_equal(nrow(all_edges), 8L)

  # Expect that the combined edge data frame has 6 columns
  expect_equal(ncol(all_edges), 6L)

})

