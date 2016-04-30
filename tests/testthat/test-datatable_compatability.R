context("data.table and dplyr integration.")

test_that("combine_edges",{
  edges_1 <-
    create_edges(
      from = c("a", "a", "b", "c"),
      to = c("b", "d", "d", "a"),
      rel = "requires",
      color = "green",
      data = c(2.7, 8.9, 2.6, 0.6))

  # Create 'edges_2' edge data frame
  edges_2 <-
    create_edges(
      from = c("e", "g", "h", "h"),
      to = c("g", "h", "f", "e"),
      rel = "receives",
      arrowhead = "dot",
      color = "red")

  class(edges_1) <-
    c("data.table", "tbl_df", class(edges_1))

  class(edges_2) <-
    c("data.table", "tbl_df", class(edges_2))

  # Combine the 2 edge data frames
  expect_warning(combine_edges(edges_1, edges_2),
                 regexp = NA)

  all_edges <- combine_edges(edges_1, edges_2)

  # Expect that a data frame is generated
  expect_true(inherits(all_edges, "data.frame"))

  # Expect that the combined edge data frame
  # has 8 rows
  expect_equal(nrow(all_edges), 8L)

  # Expect that the combined edge data frame
  # has 6 columns
  expect_equal(ncol(all_edges), 6L)
})

test_that("create_graph",{

  # Create a node data frame
  nodes <-
    create_nodes(
      nodes = c("a", "b", "c", "d"),
      label = FALSE,
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
          c("graph_name", "graph_time", "graph_tz",
            "nodes_df", "edges_df", "graph_attrs",
            "node_attrs", "edge_attrs", "directed",
            "dot_code")))
})
