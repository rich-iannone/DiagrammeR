# Print graph summary

get_printed_output <- function(graph, line = NULL) {

  captured_output <-
    capture_output(
      graph %>% print()) %>%
    stringr::str_split(pattern = "\n") %>%
    unlist()

  if (!is.null(line)) {
    captured_output <- captured_output[line]
  }

  captured_output
}

test_that("Printing a summary of an empty graph works", {

  graph <- create_graph()

  expect_equal(
    graph %>% get_printed_output(1),
    "DiagrammeR Graph // no nodes")

  expect_equal(
    graph %>% get_printed_output(2),
    "  -- empty graph (mode: directed)")

  expect_equal(
    create_graph(directed = FALSE) %>% get_printed_output(2),
    "  -- empty graph (mode: undirected)")

  expect_equal(
    graph %>% get_printed_output(3), "")

  expect_equal(
    graph %>%
      get_printed_output(4) %>% substr(1, 45) %>% stringr::str_trim(),
    "NODES / type: <unused> / label: <unused>")

  expect_equal(
    graph %>% get_printed_output(5) %>% substr(1, 45) %>% stringr::str_trim(),
    "-- no additional node attributes")

  expect_equal(
    graph %>%
      get_printed_output(6) %>% substr(1, 45) %>% stringr::str_trim(),
    "EDGES / rel: <unused>")

  expect_equal(
    graph %>%
      get_printed_output(7) %>% substr(1, 45) %>% stringr::str_trim(),
    "-- no additional edge attributes")

  expect_equal(
    graph %>%
      get_printed_output(8) %>% substr(1, 45) %>% stringr::str_trim(),
    "SELECTION / <none>")

  expect_equal(
    graph %>%
      get_printed_output(9) %>% substr(1, 45) %>% stringr::str_trim(),
    "CACHE / <none>")

  expect_equal(
    graph %>%
      get_printed_output(10) %>% substr(1, 30) %>% stringr::str_trim(),
    "GLOBAL ATTRS / 17 are set")

  expect_equal(
    graph %>%
      get_printed_output(11) %>% substr(1, 45) %>% stringr::str_trim(),
    "GRAPH ACTIONS / <none>")

  expect_equal(
    graph %>%
      get_printed_output(12) %>% substr(1, 45) %>% stringr::str_trim(),
    "GRAPH LOG / create_graph()")
})

test_that("Printing a summary of node types works", {

  graph_type_complete <-
    create_graph() %>%
    add_path(
      n = 4,
      type = "a")

  graph_type_incomplete <-
    create_graph() %>%
    add_path(
      n = 4,
      type = c("a", "a", NA, NA))

  expect_equal(
    graph_type_complete %>% get_printed_output(4) %>% stringr::str_trim(),
    "NODES / type: 1 vals - complete / label: 4 vals - complete & unique")

  expect_equal(
    graph_type_incomplete %>% get_printed_output(4) %>% stringr::str_trim(),
    "NODES / type: 1 val / label: 4 vals - complete & unique")
})

test_that("Printing a summary of node labels works", {

  graph_label_complete <-
    create_graph() %>%
    add_path(
      n = 4,
      label = c(1, 2, 3, 4))

  graph_label_incomplete <-
    create_graph() %>%
    add_path(
      n = 4,
      label = c(1, 2, NA, NA))

  expect_equal(
    graph_label_complete %>%
      get_printed_output(4) %>% substr(1, 60) %>% stringr::str_trim(),
    "NODES / type: <unused> / label: 4 vals - complete & unique")

  expect_equal(
    graph_label_incomplete %>%
      get_printed_output(4) %>% substr(1, 45) %>% stringr::str_trim(),
    "NODES / type: <unused> / label: 2 vals")
})

test_that("Printing a summary of extra node attrs works", {

  graph_node_attr_1 <-
    create_graph() %>%
    add_path(
      n = 4,
      node_data = node_data(
        value_1 = c(1, 2, 3, 4)))

  graph_node_attr_2 <-
    create_graph() %>%
    add_path(
      n = 4,
      node_data = node_data(
        value_1 = c(1, 2, 3, 4),
        value_2 = c(5, 6, 7, 9)))

  graph_node_attr_4 <-
    create_graph() %>%
    add_path(
      n = 4,
      node_data = node_data(
        value_1 = c(1, 2, 3, 4),
        value_2 = c(5, 6, 7, 9),
          ctg_1 = c("a", "b", "c", "d"),
          ctg_2 = c("e", "f", "g", "h")))

  expect_equal(
    graph_node_attr_1 %>%
      get_printed_output(5) %>% stringr::str_trim(),
    "-- 1 additional node attribute (value_1)")

  expect_equal(
    graph_node_attr_2 %>%
      get_printed_output(5) %>% stringr::str_trim(),
    "-- 2 additional node attributes (value_1, value_2)")

  expect_equal(
    graph_node_attr_4 %>%
      get_printed_output(5) %>% stringr::str_trim(),
    "-- 4 additional node attributes (value_1, value_2, ctg_1 + 1 more)")
})

test_that("Printing a summary of edge rel values works", {

  graph_rel_complete <-
    create_graph() %>%
    add_path(
      n = 4,
      label = c(1, 2, 3, 4),
      rel = c("a", "a", "b"))

  graph_rel_incomplete <-
    create_graph() %>%
    add_path(
      n = 4,
      label = c(1, 2, 3, 4),
      rel = c("a", "b", NA))

  expect_equal(
    graph_rel_complete %>%
      get_printed_output(6) %>% substr(1, 45) %>% stringr::str_trim(),
    "EDGES / rel: 2 vals - complete")

  expect_equal(
    graph_rel_incomplete %>%
      get_printed_output(4) %>% substr(1, 60) %>% stringr::str_trim(),
    "NODES / type: <unused> / label: 4 vals - complete & unique")
})

test_that("Printing a summary of extra edge attrs works", {

  graph_edge_attr_1 <-
    create_graph() %>%
    add_path(
      n = 5,
      edge_data = edge_data(
        value_1 = c(1, 2, 3, 4)))

  graph_edge_attr_2 <-
    create_graph() %>%
    add_path(
      n = 5,
      edge_data = edge_data(
        value_1 = c(1, 2, 3, 4),
        value_2 = c(5, 6, 7, 9)))

  graph_edge_attr_4 <-
    create_graph() %>%
    add_path(
      n = 5,
      edge_data = edge_data(
        value_1 = c(1, 2, 3, 4),
        value_2 = c(5, 6, 7, 9),
        ctg_1 = c("a", "b", "c", "d"),
        ctg_2 = c("e", "f", "g", "h")))

  expect_equal(
    graph_edge_attr_1 %>%
      get_printed_output(7) %>% stringr::str_trim(),
    "-- 1 additional edge attribute (value_1)")

  expect_equal(
    graph_edge_attr_2 %>%
      get_printed_output(7) %>% stringr::str_trim(),
    "-- 2 additional edge attributes (value_1, value_2)")

  expect_equal(
    graph_edge_attr_4 %>%
      get_printed_output(7) %>% stringr::str_trim(),
    "-- 4 additional edge attributes (value_1, value_2, ctg_1 + 1 more)")
})

test_that("Describing if a graph is weighted works", {

  graph_weighted <-
    create_graph() %>%
    add_path(
      n = 5,
      edge_data = edge_data(
        weight = c(1, 2, 3, 4)))

  graph_unweighted <-
    create_graph() %>%
    add_path(n = 5)

  expect_true(
    stringr::str_detect(
      graph_weighted %>% get_printed_output(2),
      "weighted"))

  expect_false(
    stringr::str_detect(
      graph_unweighted %>% get_printed_output(2),
      "weighted"))
})

test_that("Describing if a graph is a DAG works", {

  graph_dag <-
    create_graph() %>%
    add_path(n = 5)

  graph_cycle <-
    create_graph() %>%
    add_cycle(n = 5)

  expect_true(
    stringr::str_detect(
      graph_dag %>% get_printed_output(2),
      "DAG"))

  expect_false(
    stringr::str_detect(
      graph_cycle %>% get_printed_output(2),
      "DAG"))
})

test_that("Describing if a graph is a property graph works", {

  graph_pg <-
    create_graph() %>%
    add_path(
      n = 4,
      type = "a",
      label = 1:4,
      rel = c("a", "a", "b", "b"))

  graph_not_pg <-
    create_graph() %>%
    add_path(
      n = 4,
      label = 1:4,
      rel = c("a", "a", "b", "b"))

  expect_true(
    stringr::str_detect(
      graph_pg %>% get_printed_output(2),
      "property graph"))

  expect_false(
    stringr::str_detect(
      graph_not_pg %>% get_printed_output(2),
      "property graph"))
})

test_that("Describing if a graph is a simple graph works", {

  graph_simple <-
    create_graph() %>%
    add_path(
      n = 4)

  graph_not_simple <-
    create_graph() %>%
    add_path(
      n = 4) %>%
    add_edges_w_string(
      edges = "1->2")

  expect_match(
    graph_simple %>% get_printed_output(2),
    "simple")

  expect_no_match(
    graph_not_simple %>% get_printed_output(2),
    "simple")
})

test_that("Describing if a graph is a connected or not works", {

  graph_connected <-
    create_graph() %>%
    add_path(
      n = 4)

  graph_not_connected <-
    create_graph() %>%
    add_n_nodes(n = 4)

  expect_match(
    graph_connected %>% get_printed_output(2),
    " connected ")

  expect_match(
    graph_not_connected %>% get_printed_output(2),
    " disconnected ")
})

test_that("The number of reported nodes is correct", {

  graph_no_nodes <-
    create_graph()

  graph_1_node <-
    create_graph() %>%
    add_node()

  graph_4_nodes <-
    create_graph() %>%
    add_n_nodes(n = 4)

  expect_true(
    stringr::str_detect(
      graph_no_nodes %>% get_printed_output(1),
      " no nodes"))

  expect_true(
    stringr::str_detect(
      graph_1_node %>% get_printed_output(1),
      " 1 node"))

  expect_true(
    stringr::str_detect(
      graph_4_nodes %>% get_printed_output(1),
      " 4 nodes"))
})

test_that("The number of reported edges is correct", {

  graph_no_edges <-
    create_graph() %>%
    add_n_nodes(n = 4)

  graph_1_edge <-
    create_graph() %>%
    add_path(n = 2)

  graph_3_edges <-
    create_graph() %>%
    add_path(n = 4)

  expect_false(
    stringr::str_detect(
      graph_no_edges %>% get_printed_output(1),
      " edge"))

  expect_true(
    stringr::str_detect(
      graph_1_edge %>% get_printed_output(1),
      " 1 edge"))

  expect_true(
    stringr::str_detect(
      graph_3_edges %>% get_printed_output(1),
      " 3 edges"))

  expect_true(
    stringr::str_detect(
      graph_3_edges %>% get_printed_output(1),
      "DiagrammeR Graph // 4 nodes / 3 edges"))
})

test_that("Printing a summary line for node/edge selections works", {

  graph_all_nodes_selected <-
    create_graph() %>%
    add_path(n = 4) %>%
    select_nodes()

  graph_2_nodes_selected <-
    create_graph() %>%
    add_path(n = 4) %>%
    select_nodes_by_id(nodes = c(1, 2))

  graph_all_edges_selected <-
    create_graph() %>%
    add_path(n = 4) %>%
    select_edges()

  graph_2_edges_selected <-
    create_graph() %>%
    add_path(n = 4) %>%
    select_edges_by_edge_id(edges = c(1, 2))

  expect_equal(
    graph_all_nodes_selected %>%
      get_printed_output(8) %>% substr(1, 45) %>% stringr::str_trim(),
    "SELECTION / 4 nodes selected (ALL)")

  expect_equal(
    graph_2_nodes_selected %>%
      get_printed_output(8) %>% substr(1, 45) %>% stringr::str_trim(),
    "SELECTION / 2 nodes selected")

  expect_equal(
    graph_all_edges_selected %>%
      get_printed_output(8) %>% substr(1, 45) %>% stringr::str_trim(),
    "SELECTION / 3 edges selected (ALL)")

  expect_equal(
    graph_2_edges_selected %>%
      get_printed_output(8) %>% substr(1, 45) %>% stringr::str_trim(),
    "SELECTION / 2 edges selected")
})

test_that("Printing a summary line for graph caches works", {

  graph_no_cache <-
    create_graph() %>%
    add_path(n = 4)

  graph_w_1_cache <-
    create_graph() %>%
    add_path(n = 4) %>%
    set_cache(
      name = "cache",
      to_cache = get_node_ids(.))

  graph_w_2_caches <-
    create_graph() %>%
    add_path(n = 4) %>%
    set_cache(
      name = "cache_1",
      to_cache = get_node_ids(.)) %>%
    set_cache(
      name = "cache_2",
      to_cache = get_node_ids(.) * 2)

  expect_equal(
    graph_no_cache %>%
      get_printed_output(9) %>% substr(1, 45) %>% stringr::str_trim(),
    "CACHE / <none>")

  expect_equal(
    graph_w_1_cache %>%
      get_printed_output(9) %>% substr(1, 45) %>% stringr::str_trim(),
    "CACHE / 1 cache")

  expect_equal(
    graph_w_2_caches %>%
      get_printed_output(9) %>% substr(1, 45) %>% stringr::str_trim(),
    "CACHE / 2 caches")
})

test_that("Printing a summary line for global graph attributes works", {

  graph_default_global_attrs <-
    create_graph()

  graph_no_global_attrs <-
    create_graph(
      attr_theme = NULL)

  expect_equal(
    graph_default_global_attrs %>%
      get_printed_output(10) %>% substr(1, 30) %>% stringr::str_trim(),
    "GLOBAL ATTRS / 17 are set")

  expect_equal(
    graph_no_global_attrs %>%
      get_printed_output(10) %>% substr(1, 30) %>% stringr::str_trim(),
    "GLOBAL ATTRS / <none>")
})

test_that("Printing a summary line for graph actions works", {

  graph_no_actions <-
    create_graph()

  graph_w_1_action <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      set_seed = 23) %>%
    add_graph_action(
      fcn = "set_node_attr_w_fcn",
      node_attr_fcn = "get_betweenness",
      column_name = "btwns",
      action_name = "get_btwns")

  graph_w_2_actions <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      set_seed = 23) %>%
    add_graph_action(
      fcn = "set_node_attr_w_fcn",
      node_attr_fcn = "get_pagerank",
      column_name = "pagerank",
      action_name = "get_pagerank") %>%
    add_graph_action(
      fcn = "rescale_node_attrs",
      node_attr_from = "pagerank",
      node_attr_to = "width",
      action_name = "pagerank_to_width")

  expect_equal(
    graph_no_actions %>%
      get_printed_output(11) %>% substr(1, 30) %>% stringr::str_trim(),
    "GRAPH ACTIONS / <none>")

  expect_equal(
    graph_w_1_action %>%
      get_printed_output(11) %>% substr(1, 30) %>% stringr::str_trim(),
    "GRAPH ACTIONS / 1 is set")

  expect_equal(
    graph_w_2_actions %>%
      get_printed_output(11) %>% substr(1, 30) %>% stringr::str_trim(),
    "GRAPH ACTIONS / 2 are set")
})
