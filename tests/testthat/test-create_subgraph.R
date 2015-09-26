context("Creating a subgraph from a graph object")

test_that("a subgraph can be created", {

  # Create a graph
  set.seed(64)

  nodes <-
    create_nodes(nodes = LETTERS,
                 type = "letter",
                 shape = sample(c("circle", "rectangle"),
                                length(LETTERS),
                                replace = TRUE),
                 fillcolor = sample(c("aqua", "gray80",
                                      "pink", "lightgreen",
                                      "azure", "yellow"),
                                    length(LETTERS),
                                    replace = TRUE))

  edges <-
    create_edges(from = sample(LETTERS, replace = TRUE),
                 to = sample(LETTERS, replace = TRUE),
                 rel = "letter_to_letter")

  graph <- create_graph(nodes_df = nodes,
                        edges_df = edges,
                        graph_attrs = "layout = neato",
                        node_attrs = c("fontname = Helvetica",
                                       "style = filled"),
                        edge_attrs = c("color = gray20",
                                       "arrowsize = 0.5"))

  # Define a subgraph starting from node "U", with a distance of 2
  subgraph <- create_subgraph(graph = graph,
                              starting_node = "U",
                              distance = 2)

  # Expect a graph object of class 'dgr_graph'
  expect_true(class(subgraph) == "dgr_graph")

  # Expect that the use of 'is_graph_empty' function will result in FALSE
  expect_false(is_graph_empty(subgraph))

  # Expect that the number of nodes and edges are less in the subgraph than
  # those of the main graph
  expect_less_than(nrow(subgraph$nodes_df), nrow(graph$nodes_df))
  expect_less_than(nrow(subgraph$edges_df), nrow(graph$edges_df))

  # Expect that the attributes/properties are retained in the subgraph
  expect_equal(graph$graph_name, subgraph$graph_name)
  expect_equal(graph$graph_time, subgraph$graph_time)
  expect_equal(graph$graph_tz, subgraph$graph_tz)
  expect_true(all(graph$graph_attrs == subgraph$graph_attrs))
  expect_true(all(graph$node_attrs == subgraph$node_attrs))
  expect_true(all(graph$edge_attrs == subgraph$edge_attrs))
  expect_true(all(graph$directed == subgraph$directed))
})
