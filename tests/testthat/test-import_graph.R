context("Importing graphs from different file formats")

test_that("importing a .sif file is possible", {

  sif_graph <-
    import_graph(
      graph_file = system.file(
        "extdata",
        "Human_Interactome.sif",
        package = "DiagrammeR"))

  # Expect a graph object of class `dgr_graph`
  expect_is(
    sif_graph, "dgr_graph")

  # Expect that several of the graph components are NULL
  expect_null(
    sif_graph$graph_name)

  expect_null(
    sif_graph$graph_time)

  expect_null(
    sif_graph$graph_tz)

  # Expect that the `nodes_df` component is a data frame
  expect_is(
    sif_graph$nodes_df, "data.frame")

  # Expect that the `edges_df` component is a data frame
  expect_is(
    sif_graph$edges_df, "data.frame")

  # Expect that the graph is a directed graph
  expect_true(
    sif_graph$directed == TRUE)

  # Expect that the `nodes_df` data frame has 3 columns
  expect_true(
    ncol(sif_graph$nodes_df) == 3)

  # Expect that the `nodes_df` data frame has 8347 rows
  expect_true(
    nrow(sif_graph$nodes_df) == 8347)

  # Expect that the `edges_df` data frame has 4 columns
  expect_true(
    ncol(sif_graph$edges_df) == 4)

  # Expect that the `edges_df` data frame has 59207 rows
  expect_true(
    nrow(sif_graph$edges_df) == 59207)
})
