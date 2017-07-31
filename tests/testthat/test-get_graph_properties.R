context("Get graph properties")

test_that("Getting the graph object log is possible", {

  # Create a random graph and add
  # an edge as well
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 23) %>%
    add_edge(
      from = 5,
      to = 10)

  # Get the graph's log data
  graph_log <- get_graph_log(graph)

  # Expect that the graph log is provided
  # as a tibble object
  expect_is(
    graph_log, "tbl_df")

  # Expect certain column names for the
  # `graph_log` object
  expect_identical(
    colnames(graph_log),
    c("version_id", "function_used", "time_modified",
      "duration", "nodes", "edges"))

  # Expect 2 rows in this graph log
  expect_equal(
    nrow(graph_log), 2)
})

test_that("Identifying the graph as a property graph is possible", {

  # Create a graph with 2 nodes (with `type`
  # values) and a single edge (with a `rel`)
  simple_property_graph <-
    create_graph() %>%
    add_node(
      type = "a",
      label = "first") %>%
    add_node(
      type = "b",
      label = "second") %>%
    add_edge(
      from = "first",
      to = "second",
      rel = "rel_1")

  # Expect that this graph be classified
  # as a property graph
  expect_true(
    is_property_graph(simple_property_graph))

  # Create an analogous graph that is not
  # a property graph because it lacks a value
  # for `rel`
  not_a_property_graph <-
    create_graph() %>%
    add_node(
      type = "a",
      label = "first") %>%
    add_node(
      type = "b",
      label = "second") %>%
    add_edge(
      from = "first",
      to = "second")

  # Expect that the `is_property_graph()`
  # function returns FALSE for this graph
  expect_false(
    is_property_graph(not_a_property_graph))
})

test_that("Identifying the graph as a DAG is possible", {

  # Create a graph with 2 nodes (with `type`
  # values) and a single edge (with a `rel`)
  non_dag <-
    create_graph() %>%
    add_cycle(n = 5) %>%
    add_n_nodes(n = 2)

  # Expect that this graph not be classified
  # as a directed acyclic graph (DAG)
  expect_false(
    is_graph_dag(non_dag))

  # Create a graph with a balanced tree and
  # several isolated nodes; this is a DAG
  a_dag <-
    create_graph() %>%
    add_balanced_tree(k = 3, h = 2) %>%
    add_n_nodes(n = 2)

  # Expect that this graph is classified
  # as a directed acyclic graph (DAG)
  expect_true(
    is_graph_dag(a_dag))

  # If a graph contains no nodes, then
  # that graph is not a DAG
  expect_false(
    is_graph_dag(create_graph()))
})
