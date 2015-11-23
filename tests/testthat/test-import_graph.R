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
  expect_equal(length(graphml_graph$node_attrs), 4)

  # Expect that the graph is a directed graph
  expect_true(graphml_graph$directed == TRUE)

  # Expect that the 'nodes_df' data frame has 10 columns
  expect_true(ncol(graphml_graph$nodes_df) == 10)

  # Expect that the 'nodes_df' data frame has 4941 rows
  expect_true(nrow(graphml_graph$nodes_df) == 4941)

  # Expect that the 'edges_df' data frame has 3 columns
  expect_true(ncol(graphml_graph$edges_df) == 3)

  # Expect that the 'edges_df' data frame has 6594 rows
  expect_true(nrow(graphml_graph$edges_df) == 6594)

})

test_that("importing a .sif file is possible", {

  sif_graph <-
    import_graph(graph_file = system.file("examples/Human_Interactome.sif",
                                          package = "DiagrammeR"))

  # Expect a graph object of class 'dgr_graph'
  expect_true(class(sif_graph) == "dgr_graph")

  # Expect that several of the graph components are NULL
  expect_null(sif_graph$graph_name)
  expect_null(sif_graph$graph_time)
  expect_null(sif_graph$graph_tz)

  # Expect that the 'nodes_df' component is a data frame
  expect_true(class(sif_graph$nodes_df) == "data.frame")

  # Expect that the 'edges_df' component is a data frame
  expect_true(class(sif_graph$edges_df) == "data.frame")

  # Expect that the 'node_attrs' is a vector of length 0
  expect_equal(length(sif_graph$node_attrs), 0)

  # Expect that the graph is a directed graph
  expect_true(sif_graph$directed == TRUE)

  # Expect that the 'nodes_df' data frame has 3 columns
  expect_true(ncol(sif_graph$nodes_df) == 3)

  # Expect that the 'nodes_df' data frame has 8347 rows
  expect_true(nrow(sif_graph$nodes_df) == 8347)

  # Expect that the 'edges_df' data frame has 3 columns
  expect_true(ncol(sif_graph$edges_df) == 3)

  # Expect that the 'edges_df' data frame has 61263 rows
  expect_true(nrow(sif_graph$edges_df) == 61263)
})

test_that("importing a .gml file is possible", {

  gml_graph <-
    import_graph(graph_file = system.file("examples/karate.gml",
                                          package = "DiagrammeR"))

  # Expect a graph object of class 'dgr_graph'
  expect_true(class(gml_graph) == "dgr_graph")

  # Expect that several of the graph components are NULL
  expect_null(gml_graph$graph_name)
  expect_null(gml_graph$graph_time)
  expect_null(gml_graph$graph_tz)

  # Expect that the 'nodes_df' component is a data frame
  expect_true(class(gml_graph$nodes_df) == "data.frame")

  # Expect that the 'edges_df' component is a data frame
  expect_true(class(gml_graph$edges_df) == "data.frame")

  # Expect that the 'node_attrs' is a vector of length 0
  expect_equal(length(gml_graph$node_attrs), 0)

  # Expect that the graph is an undirected graph
  expect_true(gml_graph$directed == FALSE)

  # Expect that the 'nodes_df' data frame has 3 columns
  expect_true(ncol(gml_graph$nodes_df) == 3)

  # Expect that the 'nodes_df' data frame has 34 rows
  expect_true(nrow(gml_graph$nodes_df) == 34)

  # Expect that the 'edges_df' data frame has 3 columns
  expect_true(ncol(gml_graph$edges_df) == 3)

  # Expect that the 'edges_df' data frame has 78 rows
  expect_true(nrow(gml_graph$edges_df) == 78)
})
