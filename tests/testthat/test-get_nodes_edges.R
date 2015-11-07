context("Getting node IDs from the entire graph or within edges")

test_that("getting node IDs from various objects is possible", {

  set.seed(26)

  # Create a node data frame
  nodes <-
    create_nodes(nodes = LETTERS,
                 label = TRUE,
                 type = c(rep("a_to_g", 7),
                          rep("h_to_p", 9),
                          rep("q_to_x", 8),
                          rep("y_and_z",2)))

  # Create an edge data frame
  edges <-
    create_edges(from = sample(LETTERS, replace = TRUE),
                 to = sample(LETTERS, replace = TRUE),
                 label = "edge",
                 relationship = "letter_to_letter")

  # Create the graph object using the node and edge data frames
  graph <- create_graph(nodes_df = nodes,
                        edges_df = edges,
                        graph_attrs = "layout = neato",
                        node_attrs = c("fontname = Helvetica",
                                       "shape = circle"))

  # Get information on the graph's nodes
  gotten_nodes <- get_nodes(graph)

  # Expect a character vector object
  expect_is(gotten_nodes, "character")

  # Expect that the character vector object has no names
  expect_null(names(gotten_nodes))

  # Expect a vector that has all LETTERS
  expect_true(all(LETTERS == gotten_nodes))

  # Expect that the same node IDs will be returned from the
  # graph object, the node data frame, and the edge data frame
  expect_equal(get_nodes(graph), get_nodes(nodes))

  # Expect that the number of nodes obtain from the entire graph
  # will be greater than the nodes associated with edges (since there
  # will be free nodes with no edges)
  expect_more_than(length(get_nodes(graph)),
                   length(unique(c(graph$edges_df$from,
                                   graph$edges_df$to))))

  # Get the node df from the graph using `get_node_df()`
  node_df_from_graph <- get_node_df(graph)

  # Expect that the nodes from the graph and from the extracted
  # node df are the same
  expect_true(all(get_nodes(node_df_from_graph) == get_nodes(graph)))

  # Expect that using `get_node_df()` on a graph with no nodes
  # will return an NA
  expect_true(is.na(get_node_df(create_graph())))
})

test_that("getting node IDs associated within a graph's edges is possible", {

  set.seed(26)

  # Create a node data frame
  nodes <-
    create_nodes(nodes = LETTERS,
                 label = TRUE,
                 type = c(rep("a_to_g", 7),
                          rep("h_to_p", 9),
                          rep("q_to_x", 8),
                          rep("y_and_z",2)))

  # Create an edge data frame
  edges <-
    create_edges(from = sample(LETTERS, replace = TRUE),
                 to = sample(LETTERS, replace = TRUE),
                 label = "edge",
                 relationship = "letter_to_letter")

  # Create the graph object using the node and edge data frames
  graph <-
    create_graph(nodes_df = nodes,
                 edges_df = edges,
                 graph_attrs = "layout = neato",
                 node_attrs = c("fontname = Helvetica",
                                "shape = circle"))

  # Get the 'outgoing' and 'incoming' node ID values
  # in a list object
  gotten_edges_list <- get_edges(graph, return_type = "list")

  # Expect a list object
  expect_is(gotten_edges_list, "list")

  # Expect that the list is of length 2
  expect_true(length(gotten_edges_list) == 2)

  # Expect character vectors of length 26 in 'gotten_edges_list'
  expect_true(length(gotten_edges_list[[1]]) == 26)
  expect_is(gotten_edges_list[[1]], "character")

  expect_true(length(gotten_edges_list[[2]]) == 26)
  expect_is(gotten_edges_list[[2]], "character")

  # Get the 'outgoing' and 'incoming' node ID values
  # in a data frame object
  gotten_edges_df <- get_edges(graph, return_type = "df")

  # Expect a data frame object
  expect_is(gotten_edges_df, "data.frame")

  # Expect that the data frame has 2 columns
  expect_true(ncol(gotten_edges_df) == 2)

  # Expect columns of class 'character' and 26 rows in 'gotten_edges_df'
  expect_is(gotten_edges_df[,1], "character")
  expect_is(gotten_edges_df[,2], "character")
  expect_true(nrow(gotten_edges_df) == 26)

  # Get the 'outgoing' and 'incoming' node ID values
  # in a vector object
  gotten_edges_vector <- get_edges(graph, return_type = "vector")

  # Expect a vector object of class 'character'
  expect_is(gotten_edges_vector, "character")

  # Expect that the vector object is of length 26
  expect_true(length(gotten_edges_vector) == 26)

  # Expect that the ' -> ' substring is in each vector component
  expect_true(all(grepl(" -> ", gotten_edges_vector)))

  # Get the edge df from the graph using `get_edge_df()`
  edge_df_from_graph <- get_edge_df(graph)

  # Expect that the edges from the graph and from the extracted
  # edge df are the same
  expect_true(all(get_edges(edge_df_from_graph, return_type = "vector") ==
        get_edges(edge_df_from_graph, return_type = "vector")))

  # Expect that using `get_edge_df()` on a graph with no edges
  # will return an NA
  expect_true(is.na(get_edge_df(create_graph(nodes_df = create_nodes("a")))))
})

test_that("getting edge information from an edge data frame is possible", {

  # Create a simple edge data frame
  edges <- create_edges(from = c("a", "a"),
                        to = c("b", "c"))

  # Get edges from the edge data frame as a returned vector object
  edges_vector_from_edf <-
    get_edges(edges, return_type = "vector")

  # Expect a vector object of class 'character'
  expect_is(edges_vector_from_edf, "character")

  # Expect that the vector object is of length 26
  expect_true(length(edges_vector_from_edf) == 2)

  # Expect that the ' -> ' substring is in each vector component
  expect_true(all(grepl(" -> ", edges_vector_from_edf)))

  # Get edges from the edge data frame as a returned list object
  edges_list_from_edf <-
    get_edges(edges, return_type = "list")

  # Expect that the list is of length 2
  expect_true(length(edges_list_from_edf) == 2)

  # Expect character vectors of length 26 in 'gotten_edges_list'
  expect_true(length(edges_list_from_edf[[1]]) == 2)
  expect_is(edges_list_from_edf[[1]], "character")

  expect_true(length(edges_list_from_edf[[2]]) == 2)
  expect_is(edges_list_from_edf[[2]], "character")

  # Get edges from the edge data frame as a returned data frame object
  edges_df_from_edf <-
    get_edges(edges, return_type = "df")

  # Expect a data frame object
  expect_is(edges_df_from_edf, "data.frame")

  # Expect that the data frame has 2 columns
  expect_true(ncol(edges_df_from_edf) == 2)

  # Expect columns of class 'character' and 26 rows in 'gotten_edges_df'
  expect_is(edges_df_from_edf[,1], "character")
  expect_is(edges_df_from_edf[,2], "character")
  expect_true(nrow(edges_df_from_edf) == 2)
})

test_that("getting edge information from a graph with no edges is possible ", {

  nodes <- create_nodes(nodes = c("a", "b"))

  graph_no_edges <- create_graph(nodes_df = nodes)

  # Get edges from an edgeless graph returned as a vector
  edges_vector_from_graph_no_edges <-
    get_edges(graph_no_edges, return_type = "vector")

  # Expect a vector object of class 'logical'
  expect_is(edges_vector_from_graph_no_edges, "logical")

  # Expect that an NA is returned
  expect_true(is.na(edges_vector_from_graph_no_edges))

  # Get edges from an edgeless graph returned as a list
  edges_list_from_graph_no_edges <-
    get_edges(graph_no_edges, return_type = "list")

  # Expect that an NA is returned
  expect_true(is.na(edges_list_from_graph_no_edges))

  # Get edges from an edgeless graph returned as a data frame
  edges_df_from_graph_no_edges <-
    get_edges(graph_no_edges, return_type = "df")

  # Expect that an NA is returned
  expect_true(is.na(edges_df_from_graph_no_edges))
})
