# Importing graphs from various formats

test_that("importing a GML graph applies edge labels", {

  # Create a temporary GML file with edge labels
  gml_text <- c(
    "graph",
    "[",
    "  directed 1",
    "  node",
    "  [",
    "    id 1",
    "    label \"A\"",
    "  ]",
    "  node",
    "  [",
    "    id 2",
    "    label \"B\"",
    "  ]",
    "  node",
    "  [",
    "    id 3",
    "    label \"C\"",
    "  ]",
    "  edge",
    "  [",
    "    source 1",
    "    target 2",
    "    label \"e1\"",
    "  ]",
    "  edge",
    "  [",
    "    source 2",
    "    target 3",
    "    label \"e2\"",
    "  ]",
    "]"
  )

  gml_file <- tempfile(fileext = ".gml")
  writeLines(gml_text, gml_file)
  on.exit(unlink(gml_file), add = TRUE)

  # Import the GML graph
  graph <- import_graph(gml_file, file_type = "gml")

  # Expect 3 nodes and 2 edges

  expect_equal(count_nodes(graph), 3)
  expect_equal(count_edges(graph), 2)

  # Expect node labels are applied
  expect_equal(graph$nodes_df$label, c("A", "B", "C"))

  # Expect edge labels are applied
  expect_equal(graph$edges_df$label, c("e1", "e2"))
})

test_that("importing a GML graph applies edge values", {

  gml_text <- c(
    "graph",
    "[",
    "  directed 0",
    "  node",
    "  [",
    "    id 1",
    "  ]",
    "  node",
    "  [",
    "    id 2",
    "  ]",
    "  edge",
    "  [",
    "    source 1",
    "    target 2",
    "    label \"x\"",
    "    value 3.5",
    "  ]",
    "]"
  )

  gml_file <- tempfile(fileext = ".gml")
  writeLines(gml_text, gml_file)
  on.exit(unlink(gml_file), add = TRUE)

  graph <- import_graph(gml_file, file_type = "gml")

  # Expect edge label and value are both applied
  expect_equal(graph$edges_df$label, "x")
  expect_equal(graph$edges_df$data_value, "3.5")

  # Expect the graph is undirected
  expect_false(graph$directed)
})
