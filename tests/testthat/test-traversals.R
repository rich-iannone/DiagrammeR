context("Traversals through nodes and edges in a graph object")

test_that("simple traversals across nodes is possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node %>% add_node %>% add_node %>% add_node %>%
    add_edge(1, 2) %>% add_edge(2, 3) %>% add_edge(3, 4)

  # Starting at node `1`, traverse to node `4`, storing
  # the traversed location as a selection in the graph
  # object
  graph <-
    graph %>% select_nodes(nodes = 1) %>%
    trav_out %>% trav_out %>% trav_out

  # Expect that node `4` is the current selection
  expect_equal(get_selection(graph)[[1]], "4")

  # Traverse back to node `1` from node `4`
  graph <-
    graph %>% trav_in %>% trav_in %>% trav_in

  # Expect that node `1` is the current selection
  expect_equal(get_selection(graph)[[1]], "1")

  # Traverse from node `1` to `2`, then, traverse nodes
  # in both directions
  graph <-
    graph %>% trav_out %>% trav_both

  # Expect that nodes `1` and `3` are in the current selection
  expect_true(all(c("1", "3") %in% get_selection(graph)[[1]]))

  # Traverse by moving from nodes onto edges, then,
  # onto nodes; from `1` to `4`
  graph <-
    graph %>% clear_selection %>% select_nodes(nodes = 1) %>%
    trav_out_edge %>% trav_in_node %>%
    trav_out_edge %>% trav_in_node %>%
    trav_out_edge %>% trav_in_node

  # Expect that node `4` is the current selection
  expect_equal(get_selection(graph)[[1]], "4")

  # Traverse back to node `1` from node `4`, using the
  # same types of traversals
  graph <-
    graph %>% trav_in_edge %>% trav_out_node %>%
    trav_in_edge %>% trav_out_node %>%
    trav_in_edge %>% trav_out_node

  # Expect that node `1` is the current selection
  expect_equal(get_selection(graph)[[1]], "1")

  # Modify the graph so that it contains a branch
  graph <-
    graph %>% clear_selection %>% select_nodes(nodes = 3) %>%
    add_n_nodes_from_selection(1) %>% clear_selection %>%
    select_nodes(nodes = 2) %>% add_n_nodes_from_selection(2) %>%
    clear_selection

  # Traverse nodes from `1` until traverse can no longer occur
  graph <- graph %>% select_nodes(nodes = 1) %>% trav_out

  # Expect that node `2` is the current selection
  expect_equal(get_selection(graph)[[1]], "2")

  # Continue traversal outward by node
  graph <- graph %>% trav_out

  # Expect that nodes `3`, `6`, and `7` are in the current selection
  expect_equal(get_selection(graph)[[1]], c("3", "6", "7"))

  # Continue traversal outward by node
  graph <- graph %>% trav_out

  # Expect that nodes `4` and `5` are in the current selection
  expect_equal(get_selection(graph)[[1]], c("4", "5"))

  # Continue traversal outward, even though at end
  graph <- graph %>% trav_out

  # Expect that nodes `4` and `5` are still in the current selection
  expect_equal(get_selection(graph)[[1]], c("4", "5"))

  # Expect an error if attempting to perform a node traversal without
  # any selection of nodes
  graph <- graph %>% clear_selection()
  expect_error(graph %>% trav_in)
  expect_error(graph %>% trav_out)
  expect_error(graph %>% trav_both)
  expect_error(graph %>% trav_in_node)
  expect_error(graph %>% trav_out_node)
  expect_error(graph %>% trav_in_edge)
  expect_error(graph %>% trav_out_edge)
})

