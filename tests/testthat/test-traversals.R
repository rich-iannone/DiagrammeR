context("Traversals through nodes and edges in a graph object")

test_that("simple traversals are possible", {

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

test_that("selective traversals with `trav_out()` are possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node %>% add_node %>% add_node %>% add_node %>%
    add_edge(1, 2) %>% add_edge(2, 3) %>% add_edge(3, 4) %>%
    select_nodes %>%
    set_node_attr_with_selection(node_attr = "type", value = "circle") %>%
    clear_selection %>%
    select_nodes_by_id(c(2, 3)) %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 10) %>%
    clear_selection %>%
    select_nodes_by_id(4) %>%
    set_node_attr_with_selection(node_attr = "shape", value = "square") %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 5) %>%
    clear_selection %>%
    select_nodes_by_id(1) %>%
    set_node_attr_with_selection(node_attr = "shape", value = "triangle") %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 5) %>%
    clear_selection %>%
    select_edges() %>%
    set_edge_attr_with_selection(edge_attr = "data_value", value = 5) %>%
    set_edge_attr_with_selection(edge_attr = "rel", value = "related_to") %>%
    clear_selection

  # Starting at node `1`, traverse to node `2` with a
  # match expression (==)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(1) %>%
    trav_out(node_attr = "data_value", match = "==10")

  # Expect that node `2` is the current selection
  expect_equal(get_selection(graph)[[1]], "2")

  # Starting at node `1`, traverse to node `2`, using a
  # different match expression (<)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(1) %>%
    trav_out(node_attr = "data_value", match = "<15")

  # Expect that node `2` is the current selection
  expect_equal(get_selection(graph)[[1]], "2")

  # Starting at node `1`, traverse to node `2`, using a
  # different match expression (>)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(1) %>%
    trav_out(node_attr = "data_value", match = ">5")

  # Expect that node `2` is the current selection
  expect_equal(get_selection(graph)[[1]], "2")

  # Starting at node `1`, traverse to node `2`, using a
  # different match expression (!=)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(1) %>%
    trav_out(node_attr = "data_value", match = "!=5")

  # Expect that node `2` is the current selection
  expect_equal(get_selection(graph)[[1]], "2")

  # Starting at node `1`, attempt to traverse to node `2`
  # using a match expression that won't yield a match
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(1) %>%
    trav_out(node_attr = "data_value", match = "!=10")

  # Expect that node `1` is the current selection (since
  # no traversal had occurred)
  expect_equal(get_selection(graph)[[1]], "1")

  # Starting at node `3`, traverse to node `4` using a match
  # on a character field
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(3) %>%
    trav_out(node_attr = "shape", match = "square")

  # Expect that node `4` is the current selection
  expect_equal(get_selection(graph)[[1]], "4")

  # Starting at node `3`, attempt to traverse to node `4`
  # using a match expression that won't yield a match
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(3) %>%
    trav_out(node_attr = "shape", match = "triangle")

  # Expect that node `4` is the current selection
  expect_equal(get_selection(graph)[[1]], "3")
})

test_that("selective traversals with `trav_in()` are possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node %>% add_node %>% add_node %>% add_node %>%
    add_edge(1, 2) %>% add_edge(2, 3) %>% add_edge(3, 4) %>%
    select_nodes %>%
    set_node_attr_with_selection(node_attr = "type", value = "circle") %>%
    clear_selection %>%
    select_nodes_by_id(c(2, 3)) %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 10) %>%
    clear_selection %>%
    select_nodes_by_id(4) %>%
    set_node_attr_with_selection(node_attr = "shape", value = "square") %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 5) %>%
    clear_selection %>%
    select_nodes_by_id(1) %>%
    set_node_attr_with_selection(node_attr = "shape", value = "triangle") %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 5) %>%
    clear_selection %>%
    select_edges() %>%
    set_edge_attr_with_selection(edge_attr = "data_value", value = 5) %>%
    set_edge_attr_with_selection(edge_attr = "rel", value = "related_to") %>%
    clear_selection

  # Starting at node `4`, traverse to node `3` with a
  # match expression (==)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(4) %>%
    trav_in(node_attr = "data_value", match = "==10")

  # Expect that node `2` is the current selection
  expect_equal(get_selection(graph)[[1]], "3")

  # Starting at node `4`, traverse to node `3`, using a
  # different match expression (<)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(4) %>%
    trav_in(node_attr = "data_value", match = "<15")

  # Expect that node `3` is the current selection
  expect_equal(get_selection(graph)[[1]], "3")

  # Starting at node `4`, traverse to node `3`, using a
  # different match expression (>)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(4) %>%
    trav_in(node_attr = "data_value", match = ">5")

  # Expect that node `3` is the current selection
  expect_equal(get_selection(graph)[[1]], "3")

  # Starting at node `4`, traverse to node `3`, using a
  # different match expression (!=)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(4) %>%
    trav_in(node_attr = "data_value", match = "!=5")

  # Expect that node `3` is the current selection
  expect_equal(get_selection(graph)[[1]], "3")

  # Starting at node `4`, attempt to traverse to node `3`
  # using a match expression that won't yield a match
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(4) %>%
    trav_in(node_attr = "data_value", match = "!=10")

  # Expect that node `4` is the current selection (since
  # no traversal had occurred)
  expect_equal(get_selection(graph)[[1]], "4")

  # Starting at node `2`, traverse to node `1` using a match
  # on a character field
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(2) %>%
    trav_in(node_attr = "shape", match = "triangle")

  # Expect that node `1` is the current selection
  expect_equal(get_selection(graph)[[1]], "1")

  # Starting at node `2`, attempt to traverse to node `1`
  # using a match expression that won't yield a match
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(2) %>%
    trav_in(node_attr = "shape", match = "square")

  # Expect that node `2` is the current selection (since
  # no traversal had occurred)
  expect_equal(get_selection(graph)[[1]], "2")
})

test_that("selective traversals with `trav_out_edge()` are possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node %>% add_node %>% add_node %>% add_node %>%
    add_edge(1, 2) %>% add_edge(2, 3) %>% add_edge(3, 4) %>%
    select_nodes %>%
    set_node_attr_with_selection(node_attr = "type", value = "circle") %>%
    clear_selection %>%
    select_nodes_by_id(c(2, 3)) %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 10) %>%
    clear_selection %>%
    select_nodes_by_id(4) %>%
    set_node_attr_with_selection(node_attr = "shape", value = "square") %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 5) %>%
    clear_selection %>%
    select_nodes_by_id(1) %>%
    set_node_attr_with_selection(node_attr = "shape", value = "triangle") %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 5) %>%
    clear_selection %>%
    select_edges() %>%
    set_edge_attr_with_selection(edge_attr = "data_value", value = 5) %>%
    set_edge_attr_with_selection(edge_attr = "rel", value = "related_to") %>%
    clear_selection

  # Starting at node `1`, traverse to edge between nodes
  # `1` and `2` with a match expression (==)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(1) %>%
    trav_out_edge(edge_attr = "data_value", match = "==5")

  # Expect that the edge `1` -> `2` is the current selection
  expect_equal(get_selection(graph)$edges$from, "1")
  expect_equal(get_selection(graph)$edges$to, "2")

  # Starting at node `1`, traverse to edge between nodes
  # `1` and `2` with a match expression (<)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(1) %>%
    trav_out_edge(edge_attr = "data_value", match = "<10")

  # Expect that the edge `1` -> `2` is the current selection
  expect_equal(get_selection(graph)$edges$from, "1")
  expect_equal(get_selection(graph)$edges$to, "2")

  # Starting at node `1`, traverse to edge between nodes
  # `1` and `2` with a match expression (>)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(1) %>%
    trav_out_edge(edge_attr = "data_value", match = ">2")

  # Expect that the edge `1` -> `2` is the current selection
  expect_equal(get_selection(graph)$edges$from, "1")
  expect_equal(get_selection(graph)$edges$to, "2")

  # Starting at node `1`, traverse to edge between nodes
  # `1` and `2` with a match expression (!=)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(1) %>%
    trav_out_edge(edge_attr = "data_value", match = "!=1")

  # Expect that the edge `1` -> `2` is the current selection
  expect_equal(get_selection(graph)$edges$from, "1")
  expect_equal(get_selection(graph)$edges$to, "2")

  # Starting at node `1`, attempt to traverse to edge
  # between nodes `1` and `2` using a match expression that
  # won't yield a match
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(1) %>%
    trav_out_edge(edge_attr = "data_value", match = "!=5")

  # Expect that node `1` is the current selection (since
  # no traversal had occurred)
  expect_equal(get_selection(graph)[[1]], "1")

  # Starting at node `1`, traverse to edge between nodes
  # `1` and `2` using a match on a character field
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(1) %>%
    trav_out_edge(edge_attr = "rel", match = "related_to")

  # Expect that the edge `1` -> `2` is the current selection
  expect_equal(get_selection(graph)$edges$from, "1")
  expect_equal(get_selection(graph)$edges$to, "2")

  # Starting at node `1`, attempt to traverse to edge between
  # nodes `1` and `2` using a match expression that won't yield
  # a match
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(1) %>%
    trav_out_edge(edge_attr = "rel", match = "belongs_with")

  # Expect that node `1` is the current selection (since
  # no traversal had occurred)
  expect_equal(get_selection(graph)[[1]], "1")
})

test_that("selective traversals with `trav_in_edge()` are possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node %>% add_node %>% add_node %>% add_node %>%
    add_edge(1, 2) %>% add_edge(2, 3) %>% add_edge(3, 4) %>%
    select_nodes %>%
    set_node_attr_with_selection(node_attr = "type", value = "circle") %>%
    clear_selection %>%
    select_nodes_by_id(c(2, 3)) %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 10) %>%
    clear_selection %>%
    select_nodes_by_id(4) %>%
    set_node_attr_with_selection(node_attr = "shape", value = "square") %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 5) %>%
    clear_selection %>%
    select_nodes_by_id(1) %>%
    set_node_attr_with_selection(node_attr = "shape", value = "triangle") %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 5) %>%
    clear_selection %>%
    select_edges() %>%
    set_edge_attr_with_selection(edge_attr = "data_value", value = 5) %>%
    set_edge_attr_with_selection(edge_attr = "rel", value = "related_to") %>%
    clear_selection

  # Starting at node `4`, traverse to edge between nodes
  # `3` and `4` with a match expression (==)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(4) %>%
    trav_in_edge(edge_attr = "data_value", match = "==5")

  # Expect that the edge `3` -> `4` is the current selection
  expect_equal(get_selection(graph)$edges$from, "3")
  expect_equal(get_selection(graph)$edges$to, "4")

  # Starting at node `4`, traverse to edge between nodes
  # `3` and `4` with a match expression (<)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(4) %>%
    trav_in_edge(edge_attr = "data_value", match = "<10")

  # Expect that the edge `3` -> `4` is the current selection
  expect_equal(get_selection(graph)$edges$from, "3")
  expect_equal(get_selection(graph)$edges$to, "4")

  # Starting at node `4`, traverse to edge between nodes
  # `3` and `4` with a match expression (>)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(4) %>%
    trav_in_edge(edge_attr = "data_value", match = ">2")

  # Expect that the edge `3` -> `4` is the current selection
  expect_equal(get_selection(graph)$edges$from, "3")
  expect_equal(get_selection(graph)$edges$to, "4")

  # Starting at node `4`, traverse to edge between nodes
  # `3` and `4` with a match expression (!=)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(4) %>%
    trav_in_edge(edge_attr = "data_value", match = "!=1")

  # Expect that the edge `3` -> `4` is the current selection
  expect_equal(get_selection(graph)$edges$from, "3")
  expect_equal(get_selection(graph)$edges$to, "4")

  # Starting at node `4`, attempt to traverse to edge
  # between nodes `3` and `4` using a match expression that
  # won't yield a match
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(4) %>%
    trav_in_edge(edge_attr = "data_value", match = "!=5")

  # Expect that node `4` is the current selection (since
  # no traversal had occurred)
  expect_equal(get_selection(graph)[[1]], "4")

  # Starting at node `4`, traverse to edge between nodes
  # `3` and `4` using a match on a character field
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(4) %>%
    trav_in_edge(edge_attr = "rel", match = "related_to")

  # Expect that the edge `3` -> `4` is the current selection
  expect_equal(get_selection(graph)$edges$from, "3")
  expect_equal(get_selection(graph)$edges$to, "4")

  # Starting at node `4`, attempt to traverse to edge between
  # nodes `3` and `4` using a match expression that won't yield
  # a match
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(4) %>%
    trav_in_edge(edge_attr = "rel", match = "belongs_with")

  # Expect that node `4` is the current selection (since
  # no traversal had occurred)
  expect_equal(get_selection(graph)[[1]], "4")
})

test_that("selective traversals with `trav_in_node()` are possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node %>% add_node %>% add_node %>% add_node %>%
    add_edge(1, 2) %>% add_edge(2, 3) %>% add_edge(3, 4) %>%
    select_nodes %>%
    set_node_attr_with_selection(node_attr = "type", value = "circle") %>%
    clear_selection %>%
    select_nodes_by_id(c(2, 3)) %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 10) %>%
    clear_selection %>%
    select_nodes_by_id(4) %>%
    set_node_attr_with_selection(node_attr = "shape", value = "square") %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 5) %>%
    clear_selection %>%
    select_nodes_by_id(1) %>%
    set_node_attr_with_selection(node_attr = "shape", value = "triangle") %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 5) %>%
    clear_selection %>%
    select_edges() %>%
    set_edge_attr_with_selection(edge_attr = "data_value", value = 5) %>%
    set_edge_attr_with_selection(edge_attr = "rel", value = "related_to") %>%
    clear_selection

  # Starting at edge `3` -> `4`, traverse to node `4` with
  # a match expression (==)
  graph <-
    graph %>% clear_selection %>% select_edges(from = 3, to = 4) %>%
    trav_in_node(node_attr = "data_value", match = "==5")

  # Expect that the node `4` is the current selection
  expect_equal(get_selection(graph)[[1]], "4")

  # Starting at edge `3` -> `4`, traverse to node `4` with
  # a match expression (<)
  graph <-
    graph %>% clear_selection %>% select_edges(from = 3, to = 4) %>%
    trav_in_node(node_attr = "data_value", match = "<10")

  # Expect that the node `4` is the current selection
  expect_equal(get_selection(graph)[[1]], "4")

  # Starting at edge `3` -> `4`, traverse to node `4` with
  # a match expression (>)
  graph <-
    graph %>% clear_selection %>% select_edges(from = 3, to = 4) %>%
    trav_in_node(node_attr = "data_value", match = ">1")

  # Expect that the node `4` is the current selection
  expect_equal(get_selection(graph)[[1]], "4")

  # Starting at edge `3` -> `4`, traverse to node `4` with
  # a match expression (!=)
  graph <-
    graph %>% clear_selection %>% select_edges(from = 3, to = 4) %>%
    trav_in_node(node_attr = "data_value", match = "!=1")

  # Expect that the node `4` is the current selection
  expect_equal(get_selection(graph)[[1]], "4")

  # Starting at edge `3` -> `4`, attempt to traverse to node `4`
  # using a match expression that won't yield a match
  graph <-
    graph %>% clear_selection %>% select_edges(from = 3, to = 4) %>%
    trav_in_node(node_attr = "data_value", match = "!=5")

  # Expect that the edge `3` -> `4` is the current selection (since
  # no traversal had occurred)
  expect_equal(get_selection(graph)$edges$from, "3")
  expect_equal(get_selection(graph)$edges$to, "4")

  # Starting at node `4`, traverse to edge between nodes
  # `3` and `4` using a match on a character field
  graph <-
    graph %>% clear_selection %>% select_edges(from = 3, to = 4) %>%
    trav_in_node(node_attr = "shape", match = "square")

  # Expect that the node `4` is the current selection
  expect_equal(get_selection(graph)[[1]], "4")

  # Starting at edge `3` -> `4`, attempt to traverse to node `4`
  # using a match on a character field that won't yield a match
  graph <-
    graph %>% clear_selection %>% select_edges(from = 3, to = 4) %>%
    trav_in_node(node_attr = "shape", match = "triangle")

  # Expect that the edge `3` -> `4` is the current selection (since
  # no traversal had occurred)
  expect_equal(get_selection(graph)$edges$from, "3")
  expect_equal(get_selection(graph)$edges$to, "4")
})

test_that("selective traversals with `trav_out_node()` are possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node %>% add_node %>% add_node %>% add_node %>%
    add_edge(1, 2) %>% add_edge(2, 3) %>% add_edge(3, 4) %>%
    select_nodes %>%
    set_node_attr_with_selection(node_attr = "type", value = "circle") %>%
    clear_selection %>%
    select_nodes_by_id(c(2, 3)) %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 10) %>%
    clear_selection %>%
    select_nodes_by_id(4) %>%
    set_node_attr_with_selection(node_attr = "shape", value = "square") %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 5) %>%
    clear_selection %>%
    select_nodes_by_id(1) %>%
    set_node_attr_with_selection(node_attr = "shape", value = "triangle") %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 5) %>%
    clear_selection %>%
    select_edges() %>%
    set_edge_attr_with_selection(edge_attr = "data_value", value = 5) %>%
    set_edge_attr_with_selection(edge_attr = "rel", value = "related_to") %>%
    clear_selection

  # Starting at edge `1` -> `2`, traverse to node `1` with
  # a match expression (==)
  graph <-
    graph %>% clear_selection %>% select_edges(from = 1, to = 2) %>%
    trav_out_node(node_attr = "data_value", match = "==5")

  # Expect that the node `1` is the current selection
  expect_equal(get_selection(graph)[[1]], "1")

  # Starting at edge `1` -> `2`, traverse to node `1` with
  # a match expression (<)
  graph <-
    graph %>% clear_selection %>% select_edges(from = 1, to = 2) %>%
    trav_out_node(node_attr = "data_value", match = "<10")

  # Expect that the node `1` is the current selection
  expect_equal(get_selection(graph)[[1]], "1")

  # Starting at edge `1` -> `2`, traverse to node `1` with
  # a match expression (>)
  graph <-
    graph %>% clear_selection %>% select_edges(from = 1, to = 2) %>%
    trav_out_node(node_attr = "data_value", match = ">1")

  # Expect that the node `1` is the current selection
  expect_equal(get_selection(graph)[[1]], "1")

  # Starting at edge `1` -> `2`, traverse to node `1` with
  # a match expression (!=)
  graph <-
    graph %>% clear_selection %>% select_edges(from = 1, to = 2) %>%
    trav_out_node(node_attr = "data_value", match = "!=1")

  # Expect that the node `1` is the current selection
  expect_equal(get_selection(graph)[[1]], "1")

  # Starting at edge `1` -> `2`, attempt to traverse to node `1`
  # using a match expression that won't yield a match
  graph <-
    graph %>% clear_selection %>% select_edges(from = 1, to = 2) %>%
    trav_out_node(node_attr = "data_value", match = "!=5")

  # Expect that the edge `1` -> `2` is the current selection (since
  # no traversal had occurred)
  expect_equal(get_selection(graph)$edges$from, "1")
  expect_equal(get_selection(graph)$edges$to, "2")

  # Starting at edge `1` -> `2`, traverse to node `1` using a match
  # on a character field
  graph <-
    graph %>% clear_selection %>% select_edges(from = 1, to = 2) %>%
    trav_out_node(node_attr = "shape", match = "triangle")

  # Expect that the node `1` is the current selection
  expect_equal(get_selection(graph)[[1]], "1")

  # Starting at edge `1` -> `2`, attempt to traverse to node `1`
  # using a match on a character field that won't yield
  # a match
  graph <-
    graph %>% clear_selection %>% select_edges(from = 1, to = 2) %>%
    trav_out_node(node_attr = "shape", match = "circle")

  # Expect that the edge `1` -> `2` is the current selection (since
  # no traversal had occurred)
  expect_equal(get_selection(graph)$edges$from, "1")
  expect_equal(get_selection(graph)$edges$to, "2")
})

test_that("selective traversals with `trav_both()` are possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node %>% add_node %>% add_node %>% add_node %>% add_node %>%
    add_edge(1, 2) %>% add_edge(2, 3) %>% add_edge(3, 4) %>% add_edge(4, 5) %>%
    select_nodes %>%
    set_node_attr_with_selection(node_attr = "type", value = "circle") %>%
    clear_selection %>%
    select_nodes_by_id(c(2, 3, 4)) %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 10) %>%
    set_node_attr_with_selection(node_attr = "shape", value = "circle") %>%
    clear_selection %>%
    select_nodes_by_id(5) %>%
    set_node_attr_with_selection(node_attr = "shape", value = "square") %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 5) %>%
    clear_selection %>%
    select_nodes_by_id(1) %>%
    set_node_attr_with_selection(node_attr = "shape", value = "triangle") %>%
    set_node_attr_with_selection(node_attr = "data_value", value = 5) %>%
    clear_selection

  # Starting at node `3`, traverse to nodes `2` and `4` with a
  # match expression (==)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(3) %>%
    trav_both(node_attr = "data_value", match = "==10")

  # Expect that nodes `2` and `4` are in the current selection
  expect_true(all(get_selection(graph)[[1]] %in% c("2", "4")))

  # Starting at node `3`, traverse to nodes `2` and `4` with a
  # match expression (<)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(3) %>%
    trav_both(node_attr = "data_value", match = "<15")

  # Expect that nodes `2` and `4` are in the current selection
  expect_true(all(get_selection(graph)[[1]] %in% c("2", "4")))

  # Starting at node `3`, traverse to nodes `2` and `4` with a
  # match expression (>)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(3) %>%
    trav_both(node_attr = "data_value", match = ">5")

  # Expect that nodes `2` and `4` are in the current selection
  expect_true(all(get_selection(graph)[[1]] %in% c("2", "4")))

  # Starting at node `3`, traverse to nodes `2` and `4` with a
  # match expression (!=)
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(3) %>%
    trav_both(node_attr = "data_value", match = "!=5")

  # Expect that nodes `2` and `4` are in the current selection
  expect_true(all(get_selection(graph)[[1]] %in% c("2", "4")))

  # Starting at node `3`, attempt traverse to nodes `2` and `4`
  # with a match expression that won't yield a match
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(3) %>%
    trav_both(node_attr = "data_value", match = "!=10")

  # Expect that the node `3` is the current selection
  expect_equal(get_selection(graph)[[1]], "3")

  # Starting at node `3`, traverse to nodes `2` and `4` with a
  # character match expression
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(3) %>%
    trav_both(node_attr = "shape", match = "circle")

  # Expect that nodes `2` and `4` are in the current selection
  expect_true(all(get_selection(graph)[[1]] %in% c("2", "4")))

  # Starting at node `3`, attempt to traverse to nodes `2` and
  # `4` with a character match expression that won't yield a
  # match
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(3) %>%
    trav_both(node_attr = "shape", match = "square")

  # Expect that the node `3` is the current selection
  expect_equal(get_selection(graph)[[1]], "3")

  # Starting at node `2`, traverse to nodes `1` and `3` with a
  # character match expression
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(2) %>%
    trav_both(node_attr = "shape", match = "triangle")

  # Expect that the node `1` is the current selection
  expect_equal(get_selection(graph)[[1]], "1")

  # Starting at node `2`, traverse to nodes `1` and `3` with a
  # character match expression
  graph <-
    graph %>% clear_selection %>% select_nodes_by_id(2) %>%
    trav_both(node_attr = "shape", match = "circle")

  # Expect that the node `3` is the current selection
  expect_equal(get_selection(graph)[[1]], "3")
})
