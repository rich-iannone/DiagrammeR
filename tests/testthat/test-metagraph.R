context("Displaying the graph's metagraph")

test_that("the display of the metagraph works", {

  # Create a randomized property
  # graph with 1000 nodes and 1350 edges
  property_graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 100,
      m = 135,
      set_seed = 23) %>%
    select_nodes_by_degree(
      expressions = "deg >= 3") %>%
    set_node_attrs_ws(
      node_attr = type,
      value = "a") %>%
    clear_selection() %>%
    select_nodes_by_degree(
      expressions = "deg < 3") %>%
    set_node_attrs_ws(
      node_attr = type,
      value = "b") %>%
    clear_selection() %>%
    select_nodes_by_degree(
      expressions = "deg == 0") %>%
    set_node_attrs_ws(
      node_attr = type,
      value = "c") %>%
    set_node_attr_to_display(
      attr = type) %>%
    select_edges_by_node_id(
      nodes =
        get_node_ids(.) %>%
        sample(
          size = 0.15 * length(.) %>%
            floor())) %>%
    set_edge_attrs_ws(
      edge_attr = rel,
      value = "r_1") %>%
    invert_selection() %>%
    set_edge_attrs_ws(
      edge_attr = rel,
      value = "r_2") %>%
    clear_selection() %>%
    copy_edge_attrs(
      edge_attr_from = rel,
      edge_attr_to = label)

  # Get the metagraph object
  metagraph_object <- display_metagraph(property_graph)

  # Expect that the object has the classes of `grViz` and `htmlwidget`
  expect_is(
    metagraph_object, c("grViz", "htmlwidget"))
})
