context("Generate dot syntax from graph objects")

test_that("Simple graph translates into specific form", {

  # Create simple graph
  nodes <- create_node_df(n = 10)

  edges <-
    create_edge_df(
      from = 1:9,
      to = 2:10)

  graph <-
    create_graph(
      nodes_df = nodes,
      edges_df = edges)

  expect_match(
    object = generate_dot(graph),
    regexp = paste0(
      # digraph {
      "^digraph[[:space:]]*\\{",
      # graph
      "[[:space:]]*graph[[:space:]]*",
      # [attrib block]
      "\\[[[:alnum:]'=.,[:space:]]*\\]",
      # node
      "[[:space:]]*node[[:space:]]*",
      # [attrib block]
      "\\[[[:alnum:]'=.,[:space:]]*\\]",
      # edge
      "[[:space:]]*edge[[:space:]]*",
      # [attrib block]
      "\\[[[:alnum:]'=.,[:space:]]*\\]",
      # node block  
      paste0("[[:space:]]*'", 1:10, "'", collapse=""),
      # edge block
      paste0("[[:space:]]*'", 1:9, "'->'", 2:10, "'", collapse=""),
      # }
      "[[:space:]]*\\}$")
    )
})
