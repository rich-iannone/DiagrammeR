context("Adding forward and reverse edges through a selection")

test_that("forward edges can be added given a selection of edges", {
  # Create an empty graph, add 2 nodes to it,
  # and create the edge `1->2`
  graph <-
    create_graph() %>%
    add_n_nodes(
      n = 2,
      type = "type_a",
      label = c("a_1", "a_2")) %>%
    add_edge(
      from = 1,
      to = 2,
      rel = "a")

  # Select the edge and create 2 additional edges
  # with the same definition (`1->2`) but with
  # different `rel` values (`b` and `c`)
  graph <-
    graph %>%
    select_edges() %>%
    add_forward_edges_ws("b") %>%
    add_forward_edges_ws("c") %>%
    clear_selection()

  # Expect that there are 3 edges in the graph
  expect_equivalent(
    edge_count(graph), 3)

  # Expect edge `id` values of `1`, `2`, and `3`
  expect_equivalent(
    graph$edges_df$id,
    c(1, 2, 3))

  # Expect the edges have the same definition
  # of `1->2`
  expect_true(
    all(get_edges(graph) == "1->2"))

  # Expect `rel` values of `a`, `b`, and `c`
  expect_equivalent(
    graph$edges_df$rel,
    c("a", "b", "c"))
})

test_that("reverse edges can be added given a selection of edges", {

  # Create an empty graph, add 2 nodes to it,
  # and create the edge `1->2`
  graph <-
    create_graph() %>%
    add_n_nodes(
      n = 2,
      type = "type_a",
      label = c("a_1", "a_2")) %>%
    add_edge(
      from = 1,
      to = 2,
      rel = "a")

  # Select the edge and create 2 additional edges
  # with the oppossite definition (`2->1`) and with
  # different `rel` values (`b` and `c`)
  graph <-
    graph %>%
    select_edges() %>%
    add_reverse_edges_ws("b") %>%
    add_reverse_edges_ws("c") %>%
    clear_selection()

  # Expect that there are 3 edges in the graph
  expect_equivalent(
    edge_count(graph), 3)

  # Expect edge `id` values of `1`, `2`, and `3`
  expect_equivalent(
    graph$edges_df$id, c(1, 2, 3))

  # Expect the last 2 edges have the definition
  # of `2->1` whereas the original edge is `1->2`
  expect_equivalent(
    get_edges(graph),
    c("1->2", "2->1", "2->1"))

  # Expect `rel` values of `a`, `b`, and `c`
  expect_equivalent(
    graph$edges_df$rel,
    c("a", "b", "c"))

  # Expect an error if the graph does not
  # have an active selection of edges
  expect_error(
    create_graph() %>%
      add_n_nodes(
        n = 2,
        type = "type_a",
        label = c("a_1", "a_2")) %>%
      add_edge(
        from = 1,
        to = 2,
        rel = "a") %>%
      add_reverse_edges_ws("b"))
})
