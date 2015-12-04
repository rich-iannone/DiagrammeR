context("Creating a subgraph")

test_that("a subgraph can be created and such an object is correct", {

  # Create a simple graph
  nodes <-
    create_nodes(nodes = c("a", "b", "c", "d",
                           "e", "f", "g", "h"),
                 value = c(3.5, 2.6, 9.4, 2.7,
                           5.2, 2.1, 4.8, 8.5))

  edges <-
    create_edges(from = c("a", "b", "c", "g", "e",
                          "e", "h", "f", "a", "c"),
                 to = c("d", "c", "a", "c", "h",
                        "b", "d", "e", "f", "d"))

  graph <-
    create_graph(nodes_df = nodes,
                 edges_df = edges)

  # Create a selection of nodes, stored within the
  # graph object
  graph <-
    select_nodes(graph,
                 node_attr = "value",
                 search = "> 3")

  # Create a subgraph based on the selection
  subgraph <-
    create_subgraph_from_selection(graph)

  # Expect that only those nodes with a value >3 are in the subgraph
  expect_true(all(c("a", "c", "e", "g", "h") %in% get_nodes(subgraph)))

  # Expect only certain edges to be present in the subgraph
  expect_true(all(c("c -> a", "g -> c", "e -> h") %in%
                    get_edges(subgraph, return_type = "vector")))

  # Expect an error when attempting to create a subgraph with
  # a graph without an active selection
  graph <- clear_selection(graph)
  expect_error(create_subgraph_from_selection(graph))
})
