test_that("No warning is produced.", {
  # this seems not to pass all the time.
  skip_on_cran()
  # before changing render_graph circa line 177, there was a warn
  withr::local_seed(10)
  expect_no_warning(create_graph() %>%
    add_balanced_tree(
      k = 2, h = 3) %>%
    render_graph(layout = "tree"))

  # example_graph from README
  example_graph <-
    create_graph() %>%
    add_pa_graph(
      n = 50, m = 1,
      set_seed = 23
    ) %>%
    add_gnp_graph(
      n = 50, p = 1 / 100,
      set_seed = 23
    ) %>%
    join_node_attrs(df = get_betweenness(.)) %>%
    join_node_attrs(df = get_degree_total(.)) %>%
    colorize_node_attrs(
      node_attr_from = total_degree,
      node_attr_to = fillcolor,
      palette = "Greens",
      alpha = 90
    ) %>%
    rescale_node_attrs(
      node_attr_from = betweenness,
      to_lower_bound = 0.5,
      to_upper_bound = 1.0,
      node_attr_to = height
    ) %>%
    select_nodes_by_id(nodes = get_articulation_points(.)) %>%
    set_node_attrs_ws(node_attr = peripheries, value = 2) %>%
    set_node_attrs_ws(node_attr = penwidth, value = 3) %>%
    clear_selection() %>%
    set_node_attr_to_display(attr = NULL)

  expect_no_warning(render_graph(example_graph, layout = "nicely"))
  expect_no_warning(render_graph(example_graph, layout = "circle"))
  expect_no_warning(render_graph(example_graph, layout = "kk"))
  expect_no_warning(render_graph(example_graph, layout = "tree"))

  expect_no_warning(render_graph(example_graph, layout = "fr"))

})

test_that("Output to svg works as expected", {
  skip("doesn't seem to work.")
  skip_if_not_installed("DiagrammeRsvg")
    # example_graph from README
  example_graph <-
    create_graph() %>%
    add_pa_graph(
      n = 50, m = 1,
      set_seed = 23
    ) %>%
    add_gnp_graph(
      n = 50, p = 1 / 100,
      set_seed = 23
    ) %>%
    join_node_attrs(df = get_betweenness(.)) %>%
    join_node_attrs(df = get_degree_total(.)) %>%
    colorize_node_attrs(
      node_attr_from = total_degree,
      node_attr_to = fillcolor,
      palette = "Greens",
      alpha = 90
    ) %>%
    rescale_node_attrs(
      node_attr_from = betweenness,
      to_lower_bound = 0.5,
      to_upper_bound = 1.0,
      node_attr_to = height
    ) %>%
    select_nodes_by_id(nodes = get_articulation_points(.)) %>%
    set_node_attrs_ws(node_attr = peripheries, value = 2) %>%
    set_node_attrs_ws(node_attr = penwidth, value = 3) %>%
    clear_selection() %>%
    set_node_attr_to_display(attr = NULL)
  expect_no_error(render_graph(example_graph, as_svg = TRUE))
})

test_that("render_graph errors on incorrect graph and layout input", {
  g <- create_graph() %>%
    add_balanced_tree(
      k = 2, h = 3)

  expect_error(
    render_graph(g, layout = "xx")
  )
  expect_error(
    render_graph(g, output = "xx")
  )
})
