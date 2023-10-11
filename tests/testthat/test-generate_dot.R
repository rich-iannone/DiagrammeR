# Generate dot syntax from graph objects

# helper functions
node <- function(id, label = NA) {
  if(all(is.na(label))) {
    paste0("'", id, "'")
  } else {
    paste0("'", id, "' \\[label = '", label, "'\\]")
  }
}
edge <- function(from, to) paste0("'", from, "'->'", to, "'")
attrib_block <- "\\[[[:alnum:]'=.,[:space:]]*\\]"

expect_dot <- function(graph, pattern) {
  expect_match(
    object = generate_dot(graph),
    regexp = paste0(pattern, collapse="[[:space:]]*"))
}

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

  expect_dot(
    graph,
    c("^digraph", "\\{",
      "graph", attrib_block,
      "node", attrib_block,
      "edge", attrib_block,
      node(1:10),
      edge(1:9, 2:10),
      "\\}$")
    )
})

# test_that("Graph with clustered nodes create subgraph", {
#
#   # Create node clusters
#
#   nodes <-
#     create_node_df(
#       n = 6,
#       cluster = c(NA, 'foo', 'foo', 'bar', NA, 'bar'))
#
#   edges <-
#     create_edge_df(
#       from = 1:5,
#       to = 2:6)
#
#   graph <-
#     create_graph(
#       nodes_df = nodes,
#       edges_df = edges)
#
#   expect_dot(
#     graph,
#     c("^digraph", "\\{",
#       "graph", attrib_block,
#       "node", attrib_block,
#       "edge", attrib_block,
#       node(c(1,5)),
#       "subgraph cluster2\\{\nlabel='bar'",
#       node(c(4,6)), "\\}",
#       "subgraph cluster3\\{\nlabel='foo'",
#       node(c(2,3)), "\\}",
#       edge(1:5, 2:6),
#       "\\}$")
#     )
#
#   # Example from @Enchufa2
#
#   nodes = data.frame(id = c(1,2,3),
#                      label = c("a","b","c"),
#                      cluster = c(NA,"data","data"))
#   edges = data.frame(from = c(1,2),
#                      to = c(3,3))
#
#   graph <- create_graph() %>%
#     add_nodes_from_table(nodes, label_col = label) %>%
#     add_edges_from_table(edges, from, to, id_external) %>%
#     add_global_graph_attrs("layout", "dot", "graph")
#
#   expect_dot(
#     graph,
#     c("^digraph", "\\{",
#       "graph", attrib_block,
#       "node", attrib_block,
#       "edge", attrib_block,
#       node(1, 'a'),
#       "subgraph cluster2\\{\nlabel='data'",
#       node(c(2,3), c('b', 'c')), "\\}",
#       edge(c(1, 2), c(3, 3)),
#       "\\}$")
#     )
# })
