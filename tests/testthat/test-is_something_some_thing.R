context("Is this thing what we think it is?")

test_that("Detecting edge loops is possible", {

  # Create a graph that has multiple
  # loop edges
  graph <-
    create_graph() %>%
    add_path(n = 4) %>%
    add_edge(
      from = 1,
      to = 1) %>%
    add_edge(
      from = 3,
      to = 3)

  # Expect that edge `4` is a loop edge
  expect_true(
    graph %>%
      is_edge_loop(edge = 4))

  # Expect that edge `2` is not a loop edge
  expect_false(
    graph %>%
      is_edge_loop(edge = 2))
})

test_that("Detecting an edge being part of a multiple edge is possible", {

  # Create a graph that has multiple
  # edges across some node pairs
  graph <-
    create_graph() %>%
    add_path(n = 4) %>%
    add_edge(
      from = 1,
      to = 2) %>%
    add_edge(
      from = 3,
      to = 4)

  # Expect that edge `1` is a multiple edge
  expect_true(
    graph %>%
      is_edge_multiple(edge = 1))

  # Expect that edge `2` is not a multiple edge
  expect_false(
    graph %>%
      is_edge_multiple(edge = 2))
})

test_that("Detecting an edge being part of a mutual edge is possible", {

  # Create a graph that has mutual
  # edges across some node pairs
  graph <-
    create_graph() %>%
    add_path(n = 4) %>%
    add_edge(
      from = 4,
      to = 3) %>%
    add_edge(
      from = 2,
      to = 1)

  # Expect that edge `1` has a mutual edge
  expect_true(
    graph %>%
      is_edge_mutual(edge = 1))

  # Expect that edge `2` does not have
  # a mutual edge
  expect_false(
    graph %>%
      is_edge_mutual(edge = 2))
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

test_that("Detecting whether a graph is simple is possible", {

  # Create a graph with 2 cycles
  graph <-
    create_graph() %>%
    add_cycle(n = 4) %>%
    add_cycle(n = 3)

  # Expect that the graph is simple
  expect_true(
    is_graph_simple(graph))

  # Create a graph that has multiple
  # loop edges
  graph <-
    create_graph() %>%
    add_path(n = 4) %>%
    add_edge(
      from = 1,
      to = 1) %>%
    add_edge(
      from = 3,
      to = 3)

  # Expect that the graph is not simple
  expect_false(
    is_graph_simple(graph))

  # Create a graph that has multiple
  # edges across some node pairs
  graph <-
    create_graph() %>%
    add_path(n = 4) %>%
    add_edge(
      from = 1,
      to = 2) %>%
    add_edge(
      from = 3,
      to = 4)

  # Expect that the graph is not simple
  expect_false(
    is_graph_simple(graph))
})

test_that("Identifying the graph as weighted is possible", {

  # Create a graph where the edges
  # have a `weight` attribute
  graph_weighted <-
    create_graph() %>%
    add_cycle(n = 5) %>%
    select_edges() %>%
    set_edge_attrs_ws(
      edge_attr = "weight",
      value = c(3, 5, 2, 9, 6)) %>%
    clear_selection()

  # Expect that this graph is classified
  # as a weighted graph
  expect_true(
    is_graph_weighted(graph_weighted))

  # Create a graph where the edges
  # have a `weight` attribute, but
  # there are some NA values in the
  # `weight` column
  graph_weights_incomplete <-
    create_graph() %>%
    add_cycle(n = 5) %>%
    select_edges() %>%
    set_edge_attrs_ws(
      edge_attr = "weight",
      value = c(3, 5, as.numeric(NA), 9, as.numeric(NA))) %>%
    clear_selection()

  # Expect that this graph won't be
  # classified as a weighted graph since
  # we need a complete set of values
  # assigned to the edges
  expect_false(
    is_graph_weighted(graph_weights_incomplete))

  # Create a graph where the edges
  # have a `weight` attribute, but
  # there the values contained therein
  # are of the `character` class
  graph_weights_chr <-
    create_graph() %>%
    add_cycle(n = 5) %>%
    select_edges() %>%
    set_edge_attrs_ws(
      edge_attr = "weight",
      value = as.character(c(3, 5, 2, 9, 6))) %>%
    clear_selection()

  # Expect that this graph won't be
  # classified as a weighted graph since
  # we the `weight` values to be either
  # as `numeric` or as `integer`
  expect_false(
    is_graph_weighted(graph_weights_chr))

  # Expect that an empty graph will
  # return FALSE
  expect_false(
    is_graph_weighted(create_graph()))

  # Expect that a graph with no edges
  # will return FALSE
  expect_false(
    is_graph_weighted(
      create_graph() %>%
        add_n_nodes(n = 5)))

  # Create a graph that does not have
  # a weight attribute
  graph <-
    create_graph() %>%
    add_path(n = 4) %>%
    add_edge(
      from = 1,
      to = 2) %>%
    add_edge(
      from = 3,
      to = 4)

  # Expect that a graph without a
  # weight attribute will return FALSE
  expect_false(
    is_graph_weighted(graph))
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

  # Expect that an empty graph will
  # return FALSE
  expect_false(
    is_graph_weighted(create_graph()))
})



