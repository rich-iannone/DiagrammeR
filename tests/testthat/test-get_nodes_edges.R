context("Getting node IDs from the entire graph or within edges")

test_that("getting node IDs from various objects is possible", {

  set.seed(26)

  # Create a node data frame
  nodes <-
    create_node_df(
      n = 26,
      label = TRUE,
      type = c(rep("a_to_g", 7),
               rep("h_to_p", 9),
               rep("q_to_x", 8),
               rep("y_and_z",2)))

  # Create an edge data frame
  edges <-
    create_edge_df(
      from = sample(1:26, replace = TRUE),
      to = sample(1:26, replace = TRUE),
      label = "edge",
      relationship = "letter_to_letter")

  # Create the graph object using
  # the node and edge data frames
  graph <-
    create_graph(
      nodes_df = nodes,
      edges_df = edges)

  # Get the graph's node ID values
  gotten_nodes <- get_node_ids(graph)

  # Expect an `integer` vector object
  expect_is(
    gotten_nodes, "integer")

  # Expect that the integer vector object
  # has no names
  expect_null(
    names(gotten_nodes))

  # Expect a vector that is sequence
  # from `1` to `26`
  expect_true(
    all(1:26 == gotten_nodes))

  # Expect that the same node IDs will
  # be returned from the graph object,
  # the node data frame, and the edf
  expect_equal(
    get_node_ids(graph),
    get_node_ids(nodes))

  # Expect that the number of nodes
  # obtained from the entire graph will
  # be greater than the nodes associated
  # with edges (since there will be free
  # nodes with no edges)
  expect_gt(
    length(get_node_ids(graph)),
    length(unique(c(graph$edges_df$from,
                    graph$edges_df$to))))

  # Get the node df from the graph using `get_node_df()`
  node_df_from_graph <- get_node_df(graph)

  # Expect that the nodes from the graph and from the
  # extracted node df are the same
  expect_true(
    all(
      get_node_ids(node_df_from_graph) ==
        get_node_ids(graph)))

  # Expect that using `get_node_df()` on a graph with
  # no nodes will return an empty data frame
  expect_equal(
    nrow(get_node_df(create_graph())), 0)

  # Get the graph's node ID values with
  # a condition attached
  gotten_nodes_w_condition <-
    graph %>%
    get_node_ids(
      conditions =
        type == "h_to_p")

  # Expect an `integer` vector object
  expect_is(
    gotten_nodes_w_condition, "integer")

  # Expect that the integer vector object
  # has no names
  expect_null(
    names(gotten_nodes_w_condition))

  # Expect a vector that is sequence from `1` to `26`
  expect_true(
    all(8:16 == gotten_nodes_w_condition))
})

test_that("getting node IDs associated within a graph's edges is possible", {

  # Set a seed to make results reproducible
  set.seed(23)

  # Create a node data frame
  nodes <-
    create_node_df(
      n = 26,
      label = TRUE,
      type = c(rep("a_to_g", 7),
               rep("h_to_p", 9),
               rep("q_to_x", 8),
               rep("y_and_z",2)))

  # Create an edge data frame
  edges <-
    create_edge_df(
      from = sample(1:26, replace = TRUE),
      to = sample(1:26, replace = TRUE),
      rel = "letter_to_letter",
      label = "edge")

  # Create the graph object using
  # the node and edge data frames
  graph <-
    create_graph(
      nodes_df = nodes,
      edges_df = edges)

  # Get the `outgoing` and `incoming`
  # node ID values in a list object
  gotten_edges_list <-
    get_edges(graph, return_type = "list")

  # Expect a list object
  expect_is(
    gotten_edges_list, "list")

  # Expect that the list is of length 2
  expect_true(
    length(gotten_edges_list) == 2)

  # Expect integer vectors of length 26
  # in `gotten_edges_list`
  expect_true(
    length(gotten_edges_list[[1]]) == 26)

  expect_is(
    gotten_edges_list[[1]], "integer")

  expect_true(
    length(gotten_edges_list[[2]]) == 26)

  expect_is(
    gotten_edges_list[[2]], "integer")

  # Get the `outgoing` and `incoming` node ID values
  # in a data frame object
  gotten_edges_df <-
    get_edges(
      x = graph,
      return_type = "df")

  # Expect a data frame object
  expect_is(
    gotten_edges_df, "data.frame")

  # Expect that the data frame has 2 columns
  expect_true(
    ncol(gotten_edges_df) == 2)

  # Expect columns of class `integer` and that there
  # are 26 rows in `gotten_edges_df`
  expect_is(
    gotten_edges_df[, 1], "integer")

  expect_is(
    gotten_edges_df[, 2], "integer")

  expect_true(
    nrow(gotten_edges_df) == 26)

  # Get the `outgoing` and `incoming` node ID values
  # in a vector object
  gotten_edges_vector <-
    get_edges(
      x = graph,
      return_type = "vector")

  # Expect a vector object of class `character`
  expect_is(
    gotten_edges_vector, "character")

  # Expect that the vector object is of length 26
  expect_true(
    length(gotten_edges_vector) == 26)

  # Expect that the '->' substring is in
  # each vector component
  expect_true(
    all(grepl("->", gotten_edges_vector)))

  # Get the edge df from the graph using `get_edge_df()`
  edge_df_from_graph <- get_edge_df(graph)

  # Expect that using `get_edge_df()` on a graph
  # with no edges will return an empty data frame
  expect_equal(
    nrow(
      get_edge_df(
        create_graph(nodes_df = create_node_df(1)))), 0)
})

test_that("getting edge information from an edge data frame is possible", {

  # Create a simple edge data frame
  edges <-
    create_edge_df(
      from = c(1, 1),
      to = c(2, 3))

  # Get edges from the edge data frame as a
  # returned vector object
  edges_vector_from_edf <-
    get_edges(
      x = edges,
      return_type = "vector")

  # Expect a vector object of class `character`
  expect_is(
    edges_vector_from_edf, "character")

  # Expect that the vector object is of length 2
  expect_true(
    length(edges_vector_from_edf) == 2)

  # Expect that the ' -> ' substring is in
  # each vector component
  expect_true(
    all(grepl("->", edges_vector_from_edf)))

  # Get edges from the edge data frame as a
  # returned list object
  edges_list_from_edf <-
    get_edges(
      x = edges,
      return_type = "list")

  # Expect that the list is of length 2
  expect_true(
    length(edges_list_from_edf) == 2)

  # Expect integer vectors of length 26 in
  # `gotten_edges_list`
  expect_true(
    length(edges_list_from_edf[[1]]) == 2)

  expect_is(
    edges_list_from_edf[[1]], "integer")

  expect_true(
    length(edges_list_from_edf[[2]]) == 2)

  expect_is(
    edges_list_from_edf[[2]], "integer")

  # Get edges from the edge data frame as a
  # returned data frame object
  edges_df_from_edf <-
    get_edges(
      x = edges,
      return_type = "df")

  # Expect a data frame object
  expect_is(
    edges_df_from_edf, "data.frame")

  # Expect that the data frame has 2 columns
  expect_true(
    ncol(edges_df_from_edf) == 2)

  # Expect columns of class `integer` and that there
  # are 26 rows in `gotten_edges_df`
  expect_is(
    edges_df_from_edf[, 1], "integer")

  expect_is(
    edges_df_from_edf[, 2], "integer")

  expect_true(
    nrow(edges_df_from_edf) == 2)
})

test_that("getting edge information from a graph with no edges is possible ", {

  # Create a node data frame with 2 nodes
  ndf <- create_node_df(n = 2)

  # Create a graph with 2 nodes but no edges
  graph_no_edges <-
    create_graph(nodes_df = ndf)

  # Get edges from an edgeless graph returned as a vector
  edges_vector_from_graph_no_edges <-
    get_edges(
      x = graph_no_edges,
      return_type = "vector")

  # Expect a vector object of class `logical`
  expect_is(
    edges_vector_from_graph_no_edges, "logical")

  # Expect that an NA is returned
  expect_true(
    is.na(edges_vector_from_graph_no_edges))

  # Get edges from an edgeless graph returned as a list
  edges_list_from_graph_no_edges <-
    get_edges(
      x = graph_no_edges,
      return_type = "list")

  # Expect that an NA is returned
  expect_true(
    is.na(edges_list_from_graph_no_edges))

  # Get edges from an edgeless graph returned as a data frame
  edges_df_from_graph_no_edges <-
    get_edges(
      x = graph_no_edges,
      return_type = "df")

  # Expect that an NA is returned
  expect_true(
    is.na(edges_df_from_graph_no_edges))
})

test_that("getting an ndf based on a selection of nodes is possible", {

  # Create a simple graph
  graph <-
    create_graph() %>%
    add_path(n = 5)

  # Select 2 nodes from the graph
  # and view a subset of the
  # node data frame that only contains
  # that selection of nodes
  ndf_subset <-
    graph %>%
    select_nodes_by_id(
      nodes = c(1, 2)) %>%
    get_node_df_ws()

  # Expect the same column names
  # and order of column names compared
  # to output generated by `get_node_df()`
  expect_identical(
    ndf_subset %>% colnames(),
    graph %>% get_node_df %>% colnames())

  # Expect that there are 2 nodes
  # in `ndf_subset`
  expect_equal(
    nrow(ndf_subset), 2)

  # Expect that just nodes `1` and
  # `2` are in `ndf_subset`
  expect_equal(
    ndf_subset$id, c(1, 2))

  # Expect an error if there is no
  # valid node selection when calling
  # `get_node_df_ws()`
  expect_error(
    graph %>% get_node_df_ws())
})

test_that("getting an edf based on a selection of edges is possible", {

  # Create a simple graph
  graph <-
    create_graph() %>%
    add_path(n = 5)

  # Select 2 edges from the graph
  # and view a subset of the
  # edge data frame that only contains
  # that selection of edges
  edf_subset <-
    graph %>%
    select_edges_by_edge_id(
      edges = c(1, 2)) %>%
    get_edge_df_ws()

  # Expect the same column names
  # and order of column names compared
  # to output generated by `get_edge_df()`
  expect_identical(
    edf_subset %>% colnames(),
    graph %>% get_edge_df %>% colnames())

  # Expect that there are 2 edges
  # in `edf_subset`
  expect_equal(
    nrow(edf_subset), 2)

  # Expect that just edges `1` and
  # `2` are in `edf_subset`
  expect_equal(
    edf_subset$id, c(1, 2))

  # Expect an error if there is no
  # valid edge selection when calling
  # `get_edge_df_ws()`
  expect_error(
    graph %>% get_edge_df_ws())
})

test_that("getting connected nodes is possible", {

  # Create a graph with many weakly
  # connected components
  graph <-
    create_random_graph(
      n = 10, m = 10,
      set_seed = 23)

  connect_node_1 <-
    get_all_connected_nodes(
      graph = graph,
      node = 1)

  # Expect certain node IDs to be returned
  expect_equal(
    connect_node_1,
    c(2, 3, 5, 6, 7, 9, 10))

  # Expect that the node ID provided won't be
  # returned in the set of node ID values
  expect_false(
    1 %in% connect_node_1)

  # Expect an NA value if there are no connected
  # nodes to the provided node
  expect_true(
    is.na(get_all_connected_nodes(graph, 4)))

  # Expect an error if providing a node ID that
  # doesn't exist in the graph
  expect_error(
    get_all_connected_nodes(graph, 35))
})

test_that("getting edge IDs from graph objects is possible", {

# Create a node data frame (ndf)
ndf <-
  create_node_df(
    n = 4,
    type = "letter",
    color = c("red", "green", "grey", "blue"),
    value = c(3.5, 2.6, 9.4, 2.7))

# Create an edge data frame (edf)
edf <-
  create_edge_df(
    from = c(1, 2, 3),
    to = c(4, 3, 1),
    rel = "leading_to",
    color = c("pink", "blue", "blue"),
    value = c(3.9, 2.5, 7.3))

# Create a graph
graph <-
  create_graph(
    nodes_df = ndf,
    edges_df = edf)

# Expect a vector of all edge IDs in the graph
expect_equal(
  get_edge_ids(graph), c(1, 2, 3))

# Using a numeric comparison (i.e., all edges
# with `value` attribute greater than 3),
# expect edges `1` and `3`
expect_equal(
  get_edge_ids(
    graph,
    conditions = value > 3), c(1, 3))

# Using an equality for a character object,
# (i.e., all nodes with `color` attribute of
# `pink`), expect edge `1`
expect_equal(
  get_edge_ids(
    graph = graph,
    conditions = color == "pink"),
  1)

# Expect that multiple conditions will work
# to return edges with the desired attribute
# values (in this case, edge `3`)
expect_equal(
  get_edge_ids(
    graph,
    conditions =
      color == "blue" &
      value > 5),
  3)

# Expect NA if no edges are matched after
# providing unmatched conditions
expect_true(
  is.na(
    get_edge_ids(
      graph,
      conditions = color == "red")))

# Expect NA if there are no edges in the graph
expect_true(
  is.na(
    get_edge_ids(
      create_graph())))
})
