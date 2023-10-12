test_that("selective traversals with `trav_in_until()` are possible", {

  # Create a path graph and add
  # values of 1 to 10 across the
  # nodes from beginning to end;
  # select the last path node
  graph <-
    create_graph() %>%
    add_path(
      n = 10,
      node_data = node_data(
        value = 1:10)) %>%
    select_nodes_by_id(
      nodes = 10)

  # Traverse outward, node-by-node
  # until stopping at a node where
  # the `value` attribute is 1
  graph <-
    graph %>%
    trav_in_until(
      conditions =
        value == 1)

  # Expect that the graph's node
  # selection is node `1` (which
  # has a node attr `value` of 1)
  expect_equal(
    graph %>%
      get_selection(), 1)

  # Create two cycles in a graph and
  # add values of 1 to 6 to the
  # first cycle, and values 7 to
  # 12 in the second; select nodes
  # `6` and `12`
  graph <-
    create_graph() %>%
    add_cycle(
      n = 6,
      node_data = node_data(
        value = 1:6)) %>%
    add_cycle(
      n = 6,
      node_data = node_data(
        value = 7:12)) %>%
    select_nodes_by_id(
      nodes = c(6, 12))

  # Traverse inward, node-by-node
  # from `6` and `12` until stopping
  # at the first nodes where the
  # `value` attribute is 1, 2, or 10;
  # specify that we should only
  # keep the finally traversed to
  # nodes that satisfy the conditions
  graph <-
    graph %>%
    trav_in_until(
      conditions =
        value %in% c(1, 2, 10),
      exclude_unmatched = TRUE)

  # Expect that the graph's node
  # selection contains nodes
  # `2` and `10`
  expect_equal(
    graph %>%
      get_selection(), c(2, 10))

  # Expect an error if there isn't
  # an active node selection
  expect_snapshot(error = TRUE,
    create_graph() %>%
      add_path(
        n = 10,
        node_data = node_data(
          value = 1:10)) %>%
      trav_in_until(
        conditions =
          value == 1))
})
