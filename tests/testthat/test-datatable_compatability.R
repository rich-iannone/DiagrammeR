context("data.table and dplyr integration")

test_that("combine_edges",{

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

  class(edges_1) <-
    c("data.table", "tbl_df", class(edges_1))

  class(edges_2) <-
    c("data.table", "tbl_df", class(edges_2))

  # Combine the 2 edge data frames
  expect_warning(combine_edfs(edges_1, edges_2),
                 regexp = NA)

  all_edges <- combine_edfs(edges_1, edges_2)

  # Expect that a data frame is generated
  expect_is(all_edges, "data.frame")

  # Expect that the combined edge data frame
  # has 8 rows
  expect_equal(nrow(all_edges), 8)

  # Expect that the combined edge data frame
  # has 7 columns
  expect_equal(ncol(all_edges), 7)
})

test_that("create_graph",{

  # Create a node data frame
  nodes <-
    create_node_df(
      n = 4,
      type = "lower",
      style = "filled",
      color = "aqua",
      shape = c("circle", "circle",
                "rectangle", "rectangle"),
      data = c(3.5, 2.6, 9.4, 2.7))

  class(nodes) <-
    c("data.table", "tbl_df", class(nodes))

  # Create the graph object using the node data frame
  expect_warning(create_graph(nodes_df = nodes),
                 regexp = NA)

  graph <- create_graph(nodes_df = nodes)

  # Expect that names in this graph object match a
  # prescribed set of names
  expect_true(
    all(names(graph) ==
          c("graph_info", "nodes_df", "edges_df",
            "global_attrs", "directed",
            "last_node", "last_edge",
            "node_selection", "edge_selection",
            "graph_log")))
})
