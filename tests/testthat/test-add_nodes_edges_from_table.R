context("Adding nodes and/or edges to an existing graph object")

test_that("adding nodes from a table to a graph is possible", {

  # Specify a path to a CSV file
  path_to_csv <-
    system.file("examples/currencies.csv",
                package = "DiagrammeR")

  # Add nodes directly from the CSV file, calling the
  # `add_nodes_from_table()` function with default
  # options
  graph_1_csv <-
    create_graph() %>%
    add_nodes_from_table(path_to_csv)

  # Expect that the graph has a non-NULL ndf but a
  # NULL edf
  expect_true(!is.null(graph_1_csv$nodes_df))
  expect_true(is.null(graph_1_csv$edges_df))

  # Expect that the graph has the same number of nodes
  # as there are rows in the CSV
  expect_equal(
    nrow(read.csv(path_to_csv)), node_count(graph_1_csv))

  # Expect certain columns to exist in the graph's
  # node data frame
  expect_equal(
    colnames(graph_1_csv$nodes_df),
    c("id", "type", "label", "iso_4217_code",
      "curr_number", "exponent", "currency_name"))
})
