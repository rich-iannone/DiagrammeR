context("Importing graphs from different file formats")

test_that("importing a .graphml file is possible", {

  graphml_graph <-
    import_graph(system.file("examples/power_grid.graphml",
                             package = "DiagrammeR"))

  # Expect a graph object of class 'dgr_graph'
  expect_true(class(graphml_graph) == "dgr_graph")

  # Expect that several of the graph components are NULL
  expect_null(graphml_graph$graph_name)
  expect_null(graphml_graph$graph_time)
  expect_null(graphml_graph$graph_tz)

  # Expect that the 'nodes_df' component is a data frame
  expect_true(class(graphml_graph$nodes_df) == "data.frame")

  # Expect that the 'edges_df' component is a data frame
  expect_true(class(graphml_graph$edges_df) == "data.frame")

  # Expect that the 'node_attrs' component is a character vector of length 1
  expect_true(class(graphml_graph$node_attrs) == "character")
  expect_equal(length(graphml_graph$node_attrs), 4L)

  # Expect that the graph is a directed graph
  expect_true(graphml_graph$directed == TRUE)

  # Expect that the 'nodes_df' data frame has 9 columns
  expect_true(ncol(graphml_graph$nodes_df) == 9L)

  # Expect that the 'nodes_df' data frame has 4941 rows
  expect_true(nrow(graphml_graph$nodes_df) == 4941L)

  # Expect that the 'edges_df' data frame has 2 columns
  expect_true(ncol(graphml_graph$edges_df) == 2L)

  # Expect that the 'edges_df' data frame has 6594 rows
  expect_true(nrow(graphml_graph$edges_df) == 6594L)
})
