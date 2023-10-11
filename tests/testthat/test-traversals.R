# Traversals through nodes and edges in a graph object

test_that("simple traversals are possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_edge(
      from = 1,
      to = 2) %>%
    add_edge(
      from = 2,
      to = 3) %>%
    add_edge(
      from = 3,
      to = 4)

  # Starting at node `1`, traverse to node `4`, storing
  # the traversed location as a selection in the graph
  # object
  graph <-
    graph %>%
    select_nodes(nodes = 1) %>%
    trav_out() %>%
    trav_out() %>%
    trav_out()

  # Expect that node `4` is the current selection
  expect_equal(
    get_selection(graph), 4)

  # Traverse back to node `1` from node `4`
  graph <-
    graph %>%
    trav_in() %>%
    trav_in() %>%
    trav_in()

  # Expect that node `1` is the current selection
  expect_equal(
    get_selection(graph), 1)

  # Traverse from node `1` to `2`, then,
  # traverse to nodes in both directions
  graph <-
    graph %>%
    trav_out() %>%
    trav_both()

  # Expect that nodes `1` and `3` are in the
  # current selection
  expect_in(get_selection(graph), c(1, 3))

  # Traverse by moving from nodes onto edges, then,
  # onto nodes; from `1` to `4`
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes(nodes = 1) %>%
    trav_out_edge() %>%
    trav_in_node() %>%
    trav_out_edge() %>%
    trav_in_node() %>%
    trav_out_edge() %>%
    trav_in_node()

  # Expect that node `4` is the current selection
  expect_equal(
    get_selection(graph), 4)

  # Traverse back to node `1` from node `4`,
  # using the same types of traversals
  graph <-
    graph %>%
    trav_in_edge() %>%
    trav_out_node() %>%
    trav_in_edge() %>%
    trav_out_node() %>%
    trav_in_edge() %>%
    trav_out_node()

  # Expect that node `1` is the current selection
  expect_equal(
    get_selection(graph), 1)

  # Modify the graph so that it contains a branch
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes(nodes = 3) %>%
    add_n_nodes_ws(
      n = 1,
      direction = "from") %>%
    clear_selection() %>%
    select_nodes(nodes = 2) %>%
    add_n_nodes_ws(
      n = 2,
      direction = "from") %>%
    clear_selection()

  # Traverse nodes from `1` until
  # traversal can no longer occur
  graph <-
    graph %>%
    select_nodes(nodes = 1) %>%
    trav_out()

  # Expect that node `2` is the current selection
  expect_equal(
    get_selection(graph), 2)

  # Continue traversal outward by node
  graph <-
    graph %>%
    trav_out()

  # Expect that nodes `3`, `6`, and `7` are
  # in the current selection
  expect_equal(
    get_selection(graph), c(3, 6, 7))

  # Continue traversal outward by node
  graph <-
    graph %>%
    trav_out()

  # Expect that nodes `4` and `5` are in the
  # current selection
  expect_equal(
    get_selection(graph), c(4, 5))

  # Continue traversal outward, even though at end
  graph <-
    graph %>%
    trav_out()

  # Expect that nodes `4` and `5` are still in the
  # current selection
  expect_equal(
    get_selection(graph), c(4, 5))

  # Expect an error if attempting to perform a node
  # traversal without any selection of nodes
  graph <-
    graph %>%
    clear_selection()

  expect_error(
    graph %>% trav_in())

  expect_error(
    graph %>% trav_out())

  expect_error(
    graph %>% trav_both())

  expect_error(
    graph %>% trav_in_node())

  expect_error(
    graph %>% trav_out_node())

  expect_error(
    graph %>% trav_in_edge())

  expect_error(
    graph %>% trav_out_edge())
})

test_that("selective traversals with `trav_out()` are possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_edge(
      from = 1,
      to = 2) %>%
    add_edge(
      from = 2,
      to = 3) %>%
    add_edge(
      from = 3,
      to = 4) %>%
    select_nodes() %>%
    set_node_attrs_ws(
      node_attr = type,
      value = "circle") %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = c(2, 3)) %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 10) %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    set_node_attrs_ws(
      node_attr = shape,
      value = "square") %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 5) %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 1) %>%
    set_node_attrs_ws(
      node_attr = shape,
      value = "triangle") %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 5) %>%
    clear_selection() %>%
    select_edges() %>%
    set_edge_attrs_ws(
      edge_attr = data_value,
      value = 5) %>%
    set_edge_attrs_ws(
      edge_attr = rel,
      value = "related_to") %>%
    clear_selection()

  # Starting at node `1`, traverse to
  # node `2` with a match expression (==)
  graph <-
    graph %>%
    clear_selection %>%
    select_nodes_by_id(nodes = 1) %>%
    trav_out(
      conditions = data_value == 10)

  # Expect that node `2` is the
  # current selection
  expect_equal(
    get_selection(graph), 2)

  # Starting at node `1`, traverse to
  # node `2`, using a different match
  # expression (<)
  graph <-
    graph %>%
    clear_selection %>%
    select_nodes_by_id(nodes = 1) %>%
    trav_out(
      conditions = data_value < 15)

  # Expect that node `2` is the
  # current selection
  expect_equal(
    get_selection(graph), 2)

  # Starting at node `1`, traverse to
  # node `2`, using a different match expression (>)
  graph <-
    graph %>%
    clear_selection %>%
    select_nodes_by_id(nodes = 1) %>%
    trav_out(
      conditions = data_value > 5)

  # Expect that node `2` is the
  # current selection
  expect_equal(
    get_selection(graph), 2)

  # Starting at node `1`, traverse to
  # node `2`, using a different match expression (!=)
  graph <-
    graph %>%
    clear_selection %>%
    select_nodes_by_id(nodes = 1) %>%
    trav_out(
      conditions = data_value != 5)

  # Expect that node `2` is the
  # current selection
  expect_equal(
    get_selection(graph), 2)

  # Starting at node `1`, attempt to
  # traverse to node `2` using a match
  # expression that won't yield a match
  graph <-
    graph %>%
    clear_selection %>%
    select_nodes_by_id(nodes = 1) %>%
    trav_out(
      conditions = data_value != 10)

  # Expect that node `1` is the
  # current selection (since no traversal
  # had occurred)
  expect_equal(
    get_selection(graph), 1)

  # Starting at node `3`, traverse to
  # node `4` using a match on a character field
  graph <-
    graph %>%
    clear_selection %>%
    select_nodes_by_id(nodes = 3) %>%
    trav_out(
      conditions = shape == "square")

  # Expect that node `4` is the
  # current selection
  expect_equal(
    get_selection(graph), 4)

  # Starting at node `3`, attempt to
  # traverse to node `4` using a match
  # expression that won't yield a match
  graph <-
    graph %>%
    clear_selection %>%
    select_nodes_by_id(nodes = 3) %>%
    trav_out(
      conditions = shape == "triangle")

  # Expect that node `3` is the current selection
  expect_equal(
    get_selection(graph), 3)
})

test_that("selective traversals with `trav_in()` are possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_edge(
      from = 1,
      to = 2) %>%
    add_edge(
      from = 2,
      to = 3) %>%
    add_edge(
      from = 3,
      to = 4) %>%
    select_nodes() %>%
    set_node_attrs_ws(
      node_attr = type,
      value = "circle") %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = c(2, 3)) %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 10) %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    set_node_attrs_ws(
      node_attr = shape,
      value = "square") %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 5) %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 1) %>%
    set_node_attrs_ws(
      node_attr = shape,
      value = "triangle") %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 5) %>%
    clear_selection() %>%
    select_edges() %>%
    set_edge_attrs_ws(
      edge_attr = data_value,
      value = 5) %>%
    set_edge_attrs_ws(
      edge_attr = rel,
      value = "related_to") %>%
    clear_selection()

  # Starting at node `4`, traverse to node `3` with a
  # match expression (==)
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    trav_in(
      conditions = data_value == 10)

  # Expect that node `3` is the current selection
  expect_equal(
    get_selection(graph), 3)

  # Starting at node `4`, traverse to node `3`, using a
  # different match expression (<)
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    trav_in(
      conditions = data_value < 15)

  # Expect that node `3` is the
  # current selection
  expect_equal(
    get_selection(graph), 3)

  # Starting at node `4`, traverse
  # to node `3`, using a different
  # match expression (>)
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    trav_in(
      conditions = data_value > 5)

  # Expect that node `3` is the
  # current selection
  expect_equal(
    get_selection(graph), 3)

  # Starting at node `4`, traverse
  # to node `3`, using a
  # different match expression (!=)
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    trav_in(
      conditions = data_value != 5)

  # Expect that node `3` is the
  # current selection
  expect_equal(
    get_selection(graph), 3)

  # Starting at node `4`, attempt
  # to traverse to node `3` using a
  # match expression that won't
  # yield a match
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    trav_in(
      conditions = data_value != 10)

  # Expect that node `4` is the
  # current selection (since no
  # traversal had occurred)
  expect_equal(
    get_selection(graph), 4)

  # Starting at node `2`, traverse
  # to node `1` using a match on a
  # character field
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 2) %>%
    trav_in(
      conditions = shape == "triangle")

  # Expect that node `1` is the
  # current selection
  expect_equal(
    get_selection(graph), 1)

  # Starting at node `2`, attempt
  # to traverse to node `1` using a
  # match expression that won't
  # yield a match
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 2) %>%
    trav_in(
      conditions = shape == "square")

  # Expect that node `2` is the
  # current selection (since no
  # traversal had occurred)
  expect_equal(
    get_selection(graph), 2)
})

test_that("selective traversals with `trav_out_edge()` are possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_edge(
      from = 1,
      to = 2) %>%
    add_edge(
      from = 2,
      to = 3) %>%
    add_edge(
      from = 3,
      to = 4) %>%
    select_nodes() %>%
    set_node_attrs_ws(
      node_attr = type,
      value = "circle") %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = c(2, 3)) %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 10) %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    set_node_attrs_ws(
      node_attr = shape,
      value = "square") %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 5) %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 1) %>%
    set_node_attrs_ws(
      node_attr = shape,
      value = "triangle") %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 5) %>%
    clear_selection() %>%
    select_edges() %>%
    set_edge_attrs_ws(
      edge_attr = data_value,
      value = 5) %>%
    set_edge_attrs_ws(
      edge_attr = rel,
      value = "related_to") %>%
    clear_selection()

  # Starting at node `1`, traverse to edge between
  # nodes `1` and `2` with a match expression (==)
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 1) %>%
    trav_out_edge(
      conditions = data_value == 5)

  # Expect that the edge `1` -> `2`
  # is the current selection
  expect_equal(
    get_selection(graph), 1)

  # Starting at node `1`, traverse to
  # the edge between nodes `1` and `2`
  # with a match expression (<)
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 1) %>%
    trav_out_edge(
      conditions = data_value < 10)

  # Expect that the edge `1` -> `2` is
  # the current selection
  expect_equal(
    get_selection(graph), 1)

  # Starting at node `1`, traverse to
  # the edge between nodes `1` and `2`
  # with a match expression (>)
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 1) %>%
    trav_out_edge(
      conditions = data_value > 2)

  # Expect that the edge `1` -> `2`
  # is the current selection
  expect_equal(
    get_selection(graph), 1)

  # Starting at node `1`, traverse to
  # the edge between nodes `1` and `2`
  # with a match expression (!=)
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 1) %>%
    trav_out_edge(
      conditions = data_value != 1)

  # Expect that the edge `1` -> `2`
  # is the current selection
  expect_equal(
    get_selection(graph), 1)

  # Starting at node `1`, attempt to
  # traverse to edge between nodes `1`
  # and `2` using a match expression
  # that won't yield a match
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 1) %>%
    trav_out_edge(
      conditions = data_value != 5)

  # Expect that node `1` is the
  # current selection (since no traversal
  # had occurred)
  expect_equal(
    get_selection(graph), 1)

  # Starting at node `1`, traverse to
  # the edge between nodes `1` and `2`
  # using a match on a character field
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 1) %>%
    trav_out_edge(
      conditions = rel == "related_to")

  # Expect that the edge `1` -> `2`
  # is the current selection
  expect_equal(
    get_selection(graph), 1)

  # Starting at node `1`, attempt to
  # traverse to the edge between nodes
  # `1` and `2` using a match expression
  # that won't yield a match
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 1) %>%
    trav_out_edge(
      conditions = rel == "belongs_with")

  # Expect that node `1` is the
  # current selection (since no
  # traversal had occurred)
  expect_equal(
    get_selection(graph), 1)
})

test_that("selective traversals with `trav_in_edge()` are possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_edge(
      from = 1,
      to = 2) %>%
    add_edge(
      from = 2,
      to = 3) %>%
    add_edge(
      from = 3,
      to = 4) %>%
    select_nodes() %>%
    set_node_attrs_ws(
      node_attr = type,
      value = "circle") %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = c(2, 3)) %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 10) %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    set_node_attrs_ws(
      node_attr = shape,
      value = "square") %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 5) %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 1) %>%
    set_node_attrs_ws(
      node_attr = shape,
      value = "triangle") %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 5) %>%
    clear_selection() %>%
    select_edges() %>%
    set_edge_attrs_ws(
      edge_attr = data_value,
      value = 5) %>%
    set_edge_attrs_ws(
      edge_attr = rel,
      value = "related_to") %>%
    clear_selection()

  # Starting at node `4`, traverse to
  # the edge between nodes `3` and `4`
  # with a match expression (==)
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    trav_in_edge(
      conditions = data_value == 5)

  # Expect that the edge `3` -> `4`
  # is the current selection
  expect_equal(
    get_selection(graph), 3)

  # Starting at node `4`, traverse to
  # the edge between nodes `3` and `4`
  # with a match expression (<)
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    trav_in_edge(
      conditions = data_value < 10)

  # Expect that the edge `3` -> `4`
  # is the current selection
  expect_equal(
    get_selection(graph), 3)

  # Starting at node `4`, traverse to
  # the edge between nodes `3` and `4`
  # with a match expression (>)
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    trav_in_edge(
      conditions = data_value > 2)

  # Expect that the edge `3` -> `4`
  # is the current selection
  expect_equal(
    get_selection(graph), 3)

  # Starting at node `4`, traverse to
  # the edge between nodes `3` and `4`
  # with a match expression (!=)
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    trav_in_edge(
      conditions = data_value != 1)

  # Expect that the edge `3` -> `4`
  # is the current selection
  expect_equal(
    get_selection(graph), 3)

  # Starting at node `4`, attempt to
  # traverse to the edge between nodes
  # `3` and `4` using a match expression
  # that won't yield a match
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    trav_in_edge(
      conditions = data_value != 5)

  # Expect that node `4` is the
  # current selection (since no traversal
  # had occurred)
  expect_equal(
    get_selection(graph), 4)

  # Starting at node `4`, traverse to
  # the edge between nodes `3` and `4`
  # using a match on a character field
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    trav_in_edge(
      conditions = rel == "related_to")

  # Expect that the edge `3` -> `4`
  # is the current selection
  expect_equal(
    get_selection(graph), 3)

  # Starting at node `4`, attempt to
  # traverse to edge between nodes `3`
  # and `4` using a match expression
  # that won't yield a match
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    trav_in_edge(
      conditions = rel == "belongs_with")

  # Expect that node `4` is the
  # current selection (since no traversal
  # had occurred)
  expect_equal(
    get_selection(graph), 4)
})

test_that("selective traversals with `trav_in_node()` are possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_edge(
      from = 1,
      to = 2) %>%
    add_edge(
      from = 2,
      to = 3) %>%
    add_edge(
      from = 3,
      to = 4) %>%
    select_nodes() %>%
    set_node_attrs_ws(
      node_attr = type,
      value = "circle") %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = c(2, 3)) %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 10) %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    set_node_attrs_ws(
      node_attr = shape,
      value = "square") %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 5) %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 1) %>%
    set_node_attrs_ws(
      node_attr = shape,
      value = "triangle") %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 5) %>%
    clear_selection() %>%
    select_edges() %>%
    set_edge_attrs_ws(
      edge_attr = data_value,
      value = 5) %>%
    set_edge_attrs_ws(
      edge_attr = rel,
      value = "related_to") %>%
    clear_selection()

  # Starting at edge `3` -> `4`,
  # traverse to node `4` with a
  # match expression (==)
  graph <-
    graph %>%
    clear_selection() %>%
    select_edges(
      from = 3,
      to = 4) %>%
    trav_in_node(
      conditions = data_value == 5)

  # Expect that the node `4` is
  # the current selection
  expect_equal(
    get_selection(graph), 4)

  # Starting at edge `3` -> `4`,
  # traverse to node `4` with a match
  # expression (<)
  graph <-
    graph %>%
    clear_selection() %>%
    select_edges(
      from = 3,
      to = 4) %>%
    trav_in_node(
      conditions = data_value < 10)

  # Expect that the node `4` is
  # the current selection
  expect_equal(
    get_selection(graph), 4)

  # Starting at edge `3` -> `4`,
  # traverse to node `4` with a
  # match expression (>)
  graph <-
    graph %>%
    clear_selection() %>%
    select_edges(
      from = 3,
      to = 4) %>%
    trav_in_node(
      conditions = data_value > 1)

  # Expect that the node `4` is
  # the current selection
  expect_equal(
    get_selection(graph), 4)

  # Starting at edge `3` -> `4`,
  # traverse to node `4` with a
  # match expression (!=)
  graph <-
    graph %>%
    clear_selection() %>%
    select_edges(
      from = 3,
      to = 4) %>%
    trav_in_node(
      conditions = data_value != 1)

  # Expect that the node `4`
  # is the current selection
  expect_equal(
    get_selection(graph), 4)

  # Starting at edge `3` -> `4`,
  # attempt to traverse to node `4`
  # using a match expression that won't
  # yield a match
  graph <-
    graph %>%
    clear_selection() %>%
    select_edges(
      from = 3,
      to = 4) %>%
    trav_in_node(
      conditions = data_value != 5)

  # Expect that the edge `3` -> `4`
  # is the current selection (since
  # no traversal had occurred)
  expect_equal(
    get_selection(graph), 3)

  # Starting at node `4`, traverse to
  # edge between nodes `3` and `4` using
  # a match on a character field
  graph <-
    graph %>%
    clear_selection() %>%
    select_edges(
      from = 3,
      to = 4) %>%
    trav_in_node(
      conditions = shape == "square")

  # Expect that the node `4` is
  # the current selection
  expect_equal(
    get_selection(graph), 4)

  # Starting at edge `3` -> `4`,
  # attempt to traverse to node `4`
  # using a match on a character field
  # that won't yield a match
  graph <-
    graph %>%
    clear_selection() %>%
    select_edges(
      from = 3,
      to = 4) %>%
    trav_in_node(
      conditions = shape == "triangle")

  # Expect that the edge `3` -> `4`
  # is the current selection (since
  # no traversal had occurred)
  expect_equal(
    get_selection(graph), 3)
})

test_that("selective traversals with `trav_out_node()` are possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_edge(
      from = 1,
      to = 2) %>%
    add_edge(
      from = 2,
      to = 3) %>%
    add_edge(
      from = 3,
      to = 4) %>%
    select_nodes() %>%
    set_node_attrs_ws(
      node_attr = type,
      value = "circle") %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = c(2, 3)) %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 10) %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    set_node_attrs_ws(
      node_attr = shape,
      value = "square") %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 5) %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 1) %>%
    set_node_attrs_ws(
      node_attr = shape,
      value = "triangle") %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 5) %>%
    clear_selection() %>%
    select_edges() %>%
    set_edge_attrs_ws(
      edge_attr = data_value,
      value = 5) %>%
    set_edge_attrs_ws(
      edge_attr = rel,
      value = "related_to") %>%
    clear_selection()

  # Starting at edge `1` -> `2`,
  # traverse to node `1` with a
  # match expression (==)
  graph <-
    graph %>%
    select_edges(
      from = 1,
      to = 2) %>%
    trav_out_node(
      conditions = data_value == 5)

  # Expect that the node `1` is
  # the current selection
  expect_equal(
    get_selection(graph), 1)

  # Starting at edge `1` -> `2`,
  # traverse to node `1` with a
  # match expression (<)
  graph <-
    graph %>%
    clear_selection() %>%
    select_edges(
      from = 1,
      to = 2) %>%
    trav_out_node(
      conditions = data_value < 10)

  # Expect that the node `1` is
  # the current selection
  expect_equal(
    get_selection(graph), 1)

  # Starting at edge `1` -> `2`,
  # traverse to node `1` with a
  # match expression (>)
  graph <-
    graph %>%
    clear_selection() %>%
    select_edges(
      from = 1,
      to = 2) %>%
    trav_out_node(
      conditions = data_value > 1)

  # Expect that the node `1` is
  # the current selection
  expect_equal(
    get_selection(graph), 1)

  # Starting at edge `1` -> `2`,
  # traverse to node `1` with a
  # match expression (!=)
  graph <-
    graph %>%
    clear_selection() %>%
    select_edges(
      from = 1,
      to = 2) %>%
    trav_out_node(
      conditions = data_value != 1)

  # Expect that the node `1` is
  # the current selection
  expect_equal(
    get_selection(graph), 1)

  # Starting at edge `1` -> `2`,
  # attempt to traverse to node `1`
  # using a match expression that
  # won't yield a match
  graph <-
    graph %>%
    clear_selection() %>%
    select_edges(
      from = 1,
      to = 2) %>%
    trav_out_node(
      conditions = data_value != 5)

  # Expect that the node `1`
  # is the current selection (since
  # no traversal had occurred)
  expect_equal(
    get_selection(graph), 1)

  # Starting at edge `1` -> `2`,
  # traverse to node `1` using a
  # match on a character field
  graph <-
    graph %>%
    clear_selection() %>%
    select_edges(
      from = 1,
      to = 2) %>%
    trav_out_node(
      conditions = shape == "triangle")

  # Expect that the node `1`
  # is the current selection
  expect_equal(
    get_selection(graph), 1)

  # Starting at edge `1` -> `2`,
  # attempt to traverse to node `1`
  # using a match on a character field
  # that won't yield a match
  graph <-
    graph %>%
    clear_selection() %>%
    select_edges(
      from = 1,
      to = 2) %>%
    trav_out_node(
      conditions = shape == "circle")

  # Expect that the edge `1` -> `2`
  # is the current selection (since no
  # traversal had occurred)
  expect_equal(
    get_selection(graph), 1)
})

test_that("selective traversals with `trav_both()` are possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_edge(
      from = 1,
      to = 2) %>%
    add_edge(
      from = 2,
      to = 3) %>%
    add_edge(
      from = 3,
      to = 4) %>%
    select_nodes() %>%
    set_node_attrs_ws(
      node_attr = type,
      value = "circle") %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = c(2, 3)) %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 10) %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    set_node_attrs_ws(
      node_attr = shape,
      value = "square") %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 5) %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 1) %>%
    set_node_attrs_ws(
      node_attr = shape,
      value = "triangle") %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 5) %>%
    clear_selection() %>%
    select_edges() %>%
    set_edge_attrs_ws(
      edge_attr = data_value,
      value = 5) %>%
    set_edge_attrs_ws(
      edge_attr = rel,
      value = "related_to") %>%
    clear_selection()

  # Starting at node `3`, traverse
  # to nodes `2` and `4` with a
  # match expression (==)
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 3) %>%
    trav_both(
      conditions = data_value == 10)

  # Expect that node `2` is in
  #@ the current selection
  expect_equal(
    get_selection(graph), 2)

  # Starting at node `3`, traverse
  # to nodes `2` and `4` with a
  # match expression (<)
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 3) %>%
    trav_both(
      conditions = data_value < 15)

  # Expect that nodes `2` and `4`
  # are in the current selection
  expect_equal(
    get_selection(graph), c(2, 4))

  # Starting at node `3`, traverse
  # to nodes `2` and `4` with a
  # match expression (>)
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 3) %>%
    trav_both(
      conditions = data_value > 5)

  # Expect that node `2` is in
  # the current selection
  expect_equal(
    get_selection(graph), 2)

  # Starting at node `3`, traverse
  # to nodes `2` and `4` with a
  # match expression (!=)
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 3) %>%
    trav_both(
      conditions = data_value != 5)

  # Expect that node `2` is in
  # the current selection
  expect_equal(
    get_selection(graph), 2)

  # Starting at node `3`, attempt
  # to traverse to nodes `2` and `4`
  # with a match expression that won't
  # yield a match in one direction
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 3) %>%
    trav_both(
      conditions = data_value != 10)

  # Expect that the node `4` is
  # the current selection
  expect_equal(
    get_selection(graph), 4)

  # Starting at node `3`, traverse to
  # nodes `2` and `4` with a character
  # match expression
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 3) %>%
    trav_both(
      conditions = type == "circle")

  # Expect that nodes `2` and `4`
  # are in the current selection
  expect_equal(
    get_selection(graph), c(2, 4))

  # Starting at node `3`, attempt to
  # traverse to nodes `2` and `4` with
  # a character match expression that
  # won't yield a match in one direction
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 3) %>%
    trav_both(
      conditions = shape == "square")

  # Expect that the node `4` is
  # the current selection
  expect_equal(
    get_selection(graph), 4)

  # Starting at node `2`, traverse
  # to nodes `1` and `3` with a
  # character match expression
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 2) %>%
    trav_both(
      conditions = shape == "triangle")

  # Expect that the node `1` is
  # the current selection
  expect_equal(
    get_selection(graph), 1)

  # Starting at node `2`, traverse to
  # node `2` with a character match expression
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 2) %>%
    trav_both(
      conditions = shape == "circle")

  # Expect that the node `2` is
  # the current selection
  expect_equal(
    get_selection(graph), 2)
})

test_that("selective traversals with `trav_both_edge()` are possible", {

  # Create a graph
  graph <-
    create_graph() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_node() %>%
    add_edge(
      from = 1,
      to = 2) %>%
    add_edge(
      from = 2,
      to = 3) %>%
    add_edge(
      from = 3,
      to = 4) %>%
    select_nodes() %>%
    set_node_attrs_ws(
      node_attr = type,
      value = "circle") %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = c(2, 3)) %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 10) %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 4) %>%
    set_node_attrs_ws(
      node_attr = shape,
      value = "square") %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 5) %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 1) %>%
    set_node_attrs_ws(
      node_attr = shape,
      value = "triangle") %>%
    set_node_attrs_ws(
      node_attr = data_value,
      value = 5) %>%
    clear_selection() %>%
    select_edges() %>%
    set_edge_attrs_ws(
      edge_attr = data_value,
      value = c(1, 2, 3)) %>%
    set_edge_attrs_ws(
      edge_attr = rel,
      value = "related_to") %>%
    clear_selection()

  # Starting at node `3`, traverse
  # to edges `3` -> `4` and `2` -> `3`
  # with a match expression (==)
  graph <-
    graph %>%
    clear_selection() %>%
    select_nodes_by_id(nodes = 3) %>%
    trav_both_edge(
      conditions = data_value == 2)

  # Expect that edge `2` is in
  # the current selection
  expect_equal(
    get_selection(graph), 2)

  # Expect an error if the graph
  # has no selection of nodes
  expect_error(
    graph %>%
      clear_selection() %>%
      trav_both_edge(
        conditions = data_value == 2))

  # Return the same graph if a
  # traversal with conditions does
  # not result in a traversal
  expect_equal(
    graph %>%
      clear_selection() %>%
      select_nodes_by_id(nodes = 3) %>%
      trav_both_edge(
        conditions = data_value == 6) %>%
      get_selection(),
    3)
})

test_that("traversing to reverse edges with an edge selection is possible", {

  # Create a node data frame (ndf)
  ndf <-
    create_node_df(
      n = 4,
      type = "basic",
      label = TRUE)

  # Create an edge data frame (edf)
  edf <-
    create_edge_df(
      from = c(1, 4, 2, 3, 3),
      to = c(4, 1, 3, 2, 1))

  # Create a graph with the ndf and edf
  graph <-
    create_graph(
      nodes_df = ndf,
      edges_df = edf)

  # Explicitly select the edges `1`->`4`
  # and `2`->`3`
  graph <-
    graph %>%
    select_edges(
      from = 1,
      to = 4) %>%
    select_edges(
      from = 2,
      to = 3)

  # Add to the selection the reverse edges
  graph_add_selection <-
    graph %>%
    trav_reverse_edge(
      add_to_selection = TRUE)

  # Expect certain edges to be available
  # in the selection
  expect_equal(
    get_selection(graph_add_selection),
    c(1, 2, 3, 4))

  # Flip the selection to the reverse edge
  graph_flip_selection <-
    graph %>%
    trav_reverse_edge(
      add_to_selection = FALSE)

  # Expect certain edges to be available
  # in the selection
  expect_equal(
    get_selection(graph_flip_selection),
    c(2, 4))

  # Expect an error if there is no active edge
  # selection in the graph
  expect_error(
    graph %>%
      clear_selection() %>%
      trav_reverse_edge(
        add_to_selection = TRUE))
})

test_that("selective traversals with `trav_out_until()` are possible", {

  # Create a path graph and add
  # values of 1 to 10 across the
  # nodes from beginning to end;
  # select the first path node
  graph <-
    create_graph() %>%
    add_path(
      n = 10,
      node_data = node_data(
        value = 1:10)) %>%
    select_nodes_by_id(
      nodes = 1)

  # Traverse outward, node-by-node
  # until stopping at a node where
  # the `value` attribute is 8
  graph <-
    graph %>%
    trav_out_until(
      conditions =
        value == 8)

  # Expect that the graph's node
  # selection is node `8` (which
  # has a node attr `value` of 8)
  expect_equal(
    graph %>%
      get_selection(), 8)

  # Create two cycles in a new graph
  # and add values of 1 to 6 to the
  # first cycle, and values 7 to
  # 12 in the second; select nodes
  # `1` and `7`
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
      nodes = c(1, 7))

  # Traverse outward, node-by-node
  # from `1` and `7` until stopping
  # at the first nodes where the
  # `value` attribute is 5, 6, or 15;
  # specify that we should only
  # keep the finally traversed to
  # nodes that satisfy the conditions
  graph <-
    graph %>%
    trav_out_until(
      conditions =
        value %in% c(5, 6, 9),
      exclude_unmatched = TRUE)

  # Expect that the graph's node
  # selection contains nodes
  # `5` and `9`
  expect_equal(
    graph %>%
      get_selection(), c(5, 9))

  # Expect an error if there isn't
  # an active node selection
  expect_error(
    create_graph() %>%
      add_path(
        n = 10,
        node_data = node_data(
          value = 1:10)) %>%
      trav_out_until(
        conditions =
          value == 8))
})

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
  expect_error(
    create_graph() %>%
      add_path(
        n = 10,
        node_data = node_data(
          value = 1:10)) %>%
      trav_in_until(
        conditions =
          value == 1))
})
