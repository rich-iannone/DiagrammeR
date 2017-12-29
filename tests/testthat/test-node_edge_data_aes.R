context("Defining node and edge aesthetics and data attributes")

test_that("creating node data attribute values is possible", {

  # Create a node data list object
  # using the `node_data()` function
  node_data <-
    node_data(
      values_1 = c(1, 2, 4, 5),
      values_2 = c("one", "two", "four", "five"))

  # Expect that a list object has
  # been generated
  expect_is(
    node_data, "list")

  # Expect specific names in the object
  expect_equal(
    names(node_data),
    c("values_1", "values_2"))

  # Expect specific values in each
  # of the list components
  expect_equal(
    node_data[[1]],
    c(1, 2, 4, 5))

  expect_equal(
    node_data[[2]],
    c("one", "two", "four", "five"))

  # Expect the function to return
  # an error when supplying data
  # attributes that are also node
  # aesthetic attributes
  expect_error(
    node_data(
      penwidth = c(1, 2, 4, 5)))

  # Expect an error if using the
  # attributes named `x` or `y`
  expect_error(
    node_data(
      x = c(1, 2, 4, 5)))

  expect_error(
    node_data(
      y = c(1, 2, 4, 5)))
})

test_that("creating edge data attribute values is possible", {

  # Create a edge data list object
  # using the `edge_data()` function
  edge_data <-
    edge_data(
      values_1 = c(1, 2, 4, 5),
      values_2 = c("one", "two", "four", "five"))

  # Expect that a list object has
  # been generated
  expect_is(
    edge_data, "list")

  # Expect specific names in the object
  expect_equal(
    names(edge_data),
    c("values_1", "values_2"))

  # Expect specific values in each
  # of the list components
  expect_equal(
    edge_data[[1]],
    c(1, 2, 4, 5))

  expect_equal(
    edge_data[[2]],
    c("one", "two", "four", "five"))

  # Expect the function to return
  # an error when supplying data
  # attributes that are also edge
  # aesthetic attributes
  expect_error(
    edge_data(
      penwidth = c(1, 2, 4, 5)))
})

test_that("creating node aesthetic attribute values is possible", {

  # Create a node aesthetic list object
  # using the `node_aes()` function
  node_aes <-
    node_aes(
      shape = c("circle", "rectangle"),
      style = c("filled", "filled"),
      penwidth = c(2.3, 5.3),
      color = c("red", "#FFFFFF"),
      fillcolor = c("orange", "#000000"),
      fontname = c("Helvetica", "Helvetica"),
      fontsize = c(12, 10.5),
      fontcolor = c("gray25", "#000000"),
      peripheries = c(1, 2),
      height = c(6.2, 5),
      width = c(5.5, 6.0),
      x = c(2.1, 5.3),
      y = c(2, 4.7),
      group = c(1, 1),
      tooltip = c("tooltip_1", "tooltip_2"),
      xlabel = c("xlabel_1", "xlabel_2"),
      URL = c("http://www.website.net", "http:://www.internet.org"),
      sides = c(4, 5),
      orientation = c(0, 45.5),
      skew = c(0.4, -0.8),
      distortion = c(-0.7, 0.3),
      gradientangle = c(1.2, 45.0),
      fixedsize = c(TRUE, FALSE),
      labelloc = c("T", "C"),
      margin = c("0.5,0.01", "0.1,0.05"))

  # Expect that a list object has
  # been generated
  expect_is(
    node_aes, "list")

  # Expect specific names in the object
  expect_equal(
    names(node_aes),
    c("shape", "style", "penwidth", "color",
      "fillcolor", "fontname", "fontsize",
      "fontcolor", "peripheries", "height",
      "width", "x", "y", "group", "tooltip",
      "xlabel", "URL", "sides", "orientation",
      "skew", "distortion", "gradientangle",
      "fixedsize", "labelloc", "margin"))

  # Expect specific values in each
  # of the list components
  expect_equal(node_aes$shape, c("circle", "rectangle"))
  expect_equal(node_aes$style, c("filled", "filled"))
  expect_equal(node_aes$penwidth, c(2.3, 5.3))
  expect_equal(node_aes$color, c("red", "#FFFFFF"))
  expect_equal(node_aes$fillcolor, c("orange", "#000000"))
  expect_equal(node_aes$fontname, c("Helvetica", "Helvetica"))
  expect_equal(node_aes$fontsize, c(12, 10.5))
  expect_equal(node_aes$fontcolor, c("gray25", "#000000"))
  expect_equal(node_aes$peripheries, c(1, 2))
  expect_equal(node_aes$height, c(6.2, 5))
  expect_equal(node_aes$width, c(5.5, 6.0))
  expect_equal(node_aes$x, c(2.1, 5.3))
  expect_equal(node_aes$y, c(2, 4.7))
  expect_equal(node_aes$group, c(1, 1))
  expect_equal(node_aes$tooltip, c("tooltip_1", "tooltip_2"))
  expect_equal(node_aes$xlabel, c("xlabel_1", "xlabel_2"))
  expect_equal(node_aes$URL, c("http://www.website.net", "http:://www.internet.org"))
  expect_equal(node_aes$sides, c(4, 5))
  expect_equal(node_aes$orientation, c(0, 45.5))
  expect_equal(node_aes$skew, c(0.4, -0.8))
  expect_equal(node_aes$distortion, c(-0.7, 0.3))
  expect_equal(node_aes$gradientangle, c(1.2, 45.0))
  expect_equal(node_aes$fixedsize, c(TRUE, FALSE))
  expect_equal(node_aes$labelloc, c("T", "C"))
  expect_equal(node_aes$margin, c("0.5,0.01", "0.1,0.05"))

  # Expect the function to return
  # an error when supplying any
  # argument name that is not an
  # aesthetic attribute
  expect_error(
    node_aes(
      value_1 = c(1, 2, 4, 5)))
})

test_that("creating edge aesthetic attribute values is possible", {

  # Create an edge aesthetic list object
  # using the `edge_aes()` function
  edge_aes <-
    edge_aes(
      style = c("bold", "dashed"),
      penwidth = c(1.0, 1.5),
      color = c("red", "#FFFFFF"),
      arrowsize = c(0.5, 1.0),
      arrowhead = c("vee", "tee"),
      arrowtail = c("normal", "dot"),
      fontname = c("Helvetica", "Helvetica"),
      fontsize = c(12, 10.5),
      fontcolor = c("gray25", "#000000"),
      len = c(1.0, 0.5),
      tooltip = c("tooltip_1", "tooltip_2"),
      URL = c("http://www.website.net", "http:://www.internet.org"),
      label = c("label_1", "label_2"),
      labelfontname = c("Helvetica", "Helvetica"),
      labelfontsize = c(12, 10.5),
      labelfontcolor = c("gray25", "#000000"),
      labeltooltip = c("tooltip_1", "tooltip_2"),
      labelURL = c("http://www.website.net", "http:://www.internet.org"),
      edgetooltip = c("tooltip_1", "tooltip_2"),
      edgeURL = c("http://www.website.net", "http:://www.internet.org"),
      dir = c("forward", "back"),
      headtooltip = c("tooltip_1", "tooltip_2"),
      headURL = c("http://www.website.net", "http:://www.internet.org"),
      headclip = c(TRUE, FALSE),
      headlabel = c("label_1", "label_2"),
      headport = c("n", "ne"),
      tailtooltip = c("tooltip_1", "tooltip_2"),
      tailURL = c("http://www.website.net", "http:://www.internet.org"),
      tailclip = c(TRUE, FALSE),
      taillabel = c("label_1", "label_2"),
      tailport = c("s", "nw"),
      decorate = c(FALSE, TRUE))

  # Expect that a list object has
  # been generated
  expect_is(
    edge_aes, "list")

  # Expect specific names in the object
  expect_equal(
    names(edge_aes) %>% sort(),
    c(
      "arrowhead", "arrowsize", "arrowtail", "color",
      "decorate" , "dir", "edgetooltip", "edgeURL",
      "fontcolor", "fontname", "fontsize", "headclip",
      "headlabel", "headport", "headtooltip", "headURL",
      "label", "labelfontcolor", "labelfontname", "labelfontsize",
      "labeltooltip", "labelURL", "len", "penwidth",
      "style", "tailclip", "taillabel", "tailport",
      "tailtooltip", "tailURL", "tooltip", "URL"))

  # Expect specific values in each
  # of the list components
  expect_equal(edge_aes$style, c("bold", "dashed"))
  expect_equal(edge_aes$penwidth, c(1.0, 1.5))
  expect_equal(edge_aes$color, c("red", "#FFFFFF"))
  expect_equal(edge_aes$arrowsize, c(0.5, 1.0))
  expect_equal(edge_aes$arrowhead, c("vee", "tee"))
  expect_equal(edge_aes$arrowtail, c("normal", "dot"))
  expect_equal(edge_aes$fontname, c("Helvetica", "Helvetica"))
  expect_equal(edge_aes$fontsize, c(12, 10.5))
  expect_equal(edge_aes$fontcolor, c("gray25", "#000000"))
  expect_equal(edge_aes$len, c(1.0, 0.5))
  expect_equal(edge_aes$tooltip, c("tooltip_1", "tooltip_2"))
  expect_equal(edge_aes$URL, c("http://www.website.net", "http:://www.internet.org"))
  expect_equal(edge_aes$label, c("label_1", "label_2"))
  expect_equal(edge_aes$labelfontname, c("Helvetica", "Helvetica"))
  expect_equal(edge_aes$labelfontsize, c(12, 10.5))
  expect_equal(edge_aes$labelfontcolor, c("gray25", "#000000"))
  expect_equal(edge_aes$labeltooltip, c("tooltip_1", "tooltip_2"))
  expect_equal(edge_aes$labelURL, c("http://www.website.net", "http:://www.internet.org"))
  expect_equal(edge_aes$edgetooltip, c("tooltip_1", "tooltip_2"))
  expect_equal(edge_aes$edgeURL, c("http://www.website.net", "http:://www.internet.org"))
  expect_equal(edge_aes$headtooltip, c("tooltip_1", "tooltip_2"))
  expect_equal(edge_aes$headURL, c("http://www.website.net", "http:://www.internet.org"))
  expect_equal(edge_aes$headclip, c(TRUE, FALSE))
  expect_equal(edge_aes$headlabel, c("label_1", "label_2"))
  expect_equal(edge_aes$headport, c("n", "ne"))
  expect_equal(edge_aes$tailtooltip, c("tooltip_1", "tooltip_2"))
  expect_equal(edge_aes$tailURL, c("http://www.website.net", "http:://www.internet.org"))
  expect_equal(edge_aes$tailclip, c(TRUE, FALSE))
  expect_equal(edge_aes$taillabel, c("label_1", "label_2"))
  expect_equal(edge_aes$tailport, c("s", "nw"))
  expect_equal(edge_aes$dir, c("forward", "back"))
  expect_equal(edge_aes$decorate, c(FALSE, TRUE))

  # Expect the function to return
  # an error when supplying any
  # argument name that is not an
  # aesthetic attribute
  expect_error(
    edge_aes(
      value_1 = c(1, 2, 4, 5)))
})

test_that("binding data and aesthetic attribute values is always possible", {

  node_data_ <-
    node_data(
      node_value = 1)

  edge_data_ <-
    edge_data(
      edge_value = 1)

  node_aes_ <-
    node_aes(
      fillcolor = "steelblue")

  edge_aes_ <-
    edge_aes(
      color = "red")

  graph_w_attrs_1 <-
    create_graph() %>%
    add_balanced_tree(
      k = 2, h = 2,
      node_aes = node_aes_, edge_aes = edge_aes_,
      node_data = node_data_, edge_data = edge_data_)

  graph_w_attrs_2 <-
    create_graph() %>%
    add_cycle(
      n = 4,
      node_aes = node_aes_, edge_aes = edge_aes_,
      node_data = node_data_, edge_data = edge_data_)

  graph_w_attrs_3 <-
    create_graph() %>%
    add_path(
      n = 4,
      node_aes = node_aes_, edge_aes = edge_aes_,
      node_data = node_data_, edge_data = edge_data_)

  graph_w_attrs_4 <-
    create_graph() %>%
    add_prism(
      n = 4,
      node_aes = node_aes_, edge_aes = edge_aes_,
      node_data = node_data_, edge_data = edge_data_)

  graph_w_attrs_5 <-
    create_graph() %>%
    add_star(
      n = 4,
      node_aes = node_aes_, edge_aes = edge_aes_,
      node_data = node_data_, edge_data = edge_data_)

  graph_w_attrs_6 <-
    create_graph() %>%
    add_full_graph(
      n = 4,
      node_aes = node_aes_, edge_aes = edge_aes_,
      node_data = node_data_, edge_data = edge_data_)

  graph_w_attrs_7 <-
    create_graph() %>%
    add_grid_2d(
      x = 4, y = 4,
      node_aes = node_aes_, edge_aes = edge_aes_,
      node_data = node_data_, edge_data = edge_data_)

  graph_w_attrs_8 <-
    create_graph() %>%
    add_grid_3d(
      x = 4, y = 4, z = 4,
      node_aes = node_aes_, edge_aes = edge_aes_,
      node_data = node_data_, edge_data = edge_data_)

  graph_w_attrs_9 <-
    create_graph() %>%
    add_gnm_graph(
      n = 20, m = 20,
      node_aes = node_aes_, edge_aes = edge_aes_,
      node_data = node_data_, edge_data = edge_data_)

  graph_w_attrs_10 <-
    create_graph() %>%
    add_gnp_graph(
      n = 20, p = 0.05,
      node_aes = node_aes_, edge_aes = edge_aes_,
      node_data = node_data_, edge_data = edge_data_)

  graph_w_attrs_11 <-
    create_graph() %>%
    add_growing_graph(
      n = 100, m = 1,
      node_aes = node_aes_, edge_aes = edge_aes_,
      node_data = node_data_, edge_data = edge_data_)

  graph_w_attrs_12 <-
    create_graph() %>%
    add_islands_graph(
      n_islands = 4, island_size = 10, p = 0.5, edges_between = 1,
      node_aes = node_aes_, edge_aes = edge_aes_,
      node_data = node_data_, edge_data = edge_data_)

  graph_w_attrs_13 <-
    create_graph() %>%
    add_pa_graph(
      n = 50, m = 1,
      node_aes = node_aes_, edge_aes = edge_aes_,
      node_data = node_data_, edge_data = edge_data_)

  graph_w_attrs_14 <-
    create_graph() %>%
    add_smallworld_graph(
      dimension = 1, size = 50, neighborhood = 1, p = 0.05,
      node_aes = node_aes_, edge_aes = edge_aes_,
      node_data = node_data_, edge_data = edge_data_)

  graph_w_attrs_15 <-
    create_graph() %>%
    add_node(
      node_aes = node_aes_,
      node_data = node_data_)

  graph_w_attrs_16 <-
    create_graph() %>%
    add_n_nodes(
      n = 2,
      node_aes = node_aes_,
      node_data = node_data_)

  graph_w_attrs_17 <-
    create_graph() %>%
    add_node() %>%
    select_nodes() %>%
    add_n_nodes_ws(
      n = 1,
      direction = "from",
      node_aes = node_aes_, edge_aes = edge_aes_,
      node_data = node_data_, edge_data = edge_data_)

  graph_w_attrs_18 <-
    create_graph() %>%
    add_path(n = 2) %>%
    select_edges() %>%
    add_reverse_edges_ws(
      edge_aes = edge_aes_,
      edge_data = edge_data_)

  expect_true("fillcolor" %in% (graph_w_attrs_1$nodes_df %>% names()))
  expect_true("fillcolor" %in% (graph_w_attrs_2$nodes_df %>% names()))
  expect_true("fillcolor" %in% (graph_w_attrs_3$nodes_df %>% names()))
  expect_true("fillcolor" %in% (graph_w_attrs_4$nodes_df %>% names()))
  expect_true("fillcolor" %in% (graph_w_attrs_5$nodes_df %>% names()))
  expect_true("fillcolor" %in% (graph_w_attrs_6$nodes_df %>% names()))
  expect_true("fillcolor" %in% (graph_w_attrs_7$nodes_df %>% names()))
  expect_true("fillcolor" %in% (graph_w_attrs_8$nodes_df %>% names()))
  expect_true("fillcolor" %in% (graph_w_attrs_9$nodes_df %>% names()))
  expect_true("fillcolor" %in% (graph_w_attrs_10$nodes_df %>% names()))
  expect_true("fillcolor" %in% (graph_w_attrs_11$nodes_df %>% names()))
  expect_true("fillcolor" %in% (graph_w_attrs_12$nodes_df %>% names()))
  expect_true("fillcolor" %in% (graph_w_attrs_13$nodes_df %>% names()))
  expect_true("fillcolor" %in% (graph_w_attrs_14$nodes_df %>% names()))
  expect_true("fillcolor" %in% (graph_w_attrs_15$nodes_df %>% names()))
  expect_true("fillcolor" %in% (graph_w_attrs_16$nodes_df %>% names()))
  expect_true("fillcolor" %in% (graph_w_attrs_17$nodes_df %>% names()))

  expect_true("node_value" %in% (graph_w_attrs_1$nodes_df %>% names()))
  expect_true("node_value" %in% (graph_w_attrs_2$nodes_df %>% names()))
  expect_true("node_value" %in% (graph_w_attrs_3$nodes_df %>% names()))
  expect_true("node_value" %in% (graph_w_attrs_4$nodes_df %>% names()))
  expect_true("node_value" %in% (graph_w_attrs_5$nodes_df %>% names()))
  expect_true("node_value" %in% (graph_w_attrs_6$nodes_df %>% names()))
  expect_true("node_value" %in% (graph_w_attrs_7$nodes_df %>% names()))
  expect_true("node_value" %in% (graph_w_attrs_8$nodes_df %>% names()))
  expect_true("node_value" %in% (graph_w_attrs_9$nodes_df %>% names()))
  expect_true("node_value" %in% (graph_w_attrs_10$nodes_df %>% names()))
  expect_true("node_value" %in% (graph_w_attrs_11$nodes_df %>% names()))
  expect_true("node_value" %in% (graph_w_attrs_12$nodes_df %>% names()))
  expect_true("node_value" %in% (graph_w_attrs_13$nodes_df %>% names()))
  expect_true("node_value" %in% (graph_w_attrs_14$nodes_df %>% names()))
  expect_true("node_value" %in% (graph_w_attrs_15$nodes_df %>% names()))
  expect_true("node_value" %in% (graph_w_attrs_16$nodes_df %>% names()))
  expect_true("node_value" %in% (graph_w_attrs_17$nodes_df %>% names()))

  expect_true("color" %in% (graph_w_attrs_1$edges_df %>% names()))
  expect_true("color" %in% (graph_w_attrs_2$edges_df %>% names()))
  expect_true("color" %in% (graph_w_attrs_3$edges_df %>% names()))
  expect_true("color" %in% (graph_w_attrs_4$edges_df %>% names()))
  expect_true("color" %in% (graph_w_attrs_5$edges_df %>% names()))
  expect_true("color" %in% (graph_w_attrs_6$edges_df %>% names()))
  expect_true("color" %in% (graph_w_attrs_7$edges_df %>% names()))
  expect_true("color" %in% (graph_w_attrs_8$edges_df %>% names()))
  expect_true("color" %in% (graph_w_attrs_9$edges_df %>% names()))
  expect_true("color" %in% (graph_w_attrs_10$edges_df %>% names()))
  expect_true("color" %in% (graph_w_attrs_11$edges_df %>% names()))
  expect_true("color" %in% (graph_w_attrs_12$edges_df %>% names()))
  expect_true("color" %in% (graph_w_attrs_13$edges_df %>% names()))
  expect_true("color" %in% (graph_w_attrs_14$edges_df %>% names()))
  expect_true("color" %in% (graph_w_attrs_17$edges_df %>% names()))
  expect_true("color" %in% (graph_w_attrs_18$edges_df %>% names()))

  expect_true("edge_value" %in% (graph_w_attrs_1$edges_df %>% names()))
  expect_true("edge_value" %in% (graph_w_attrs_2$edges_df %>% names()))
  expect_true("edge_value" %in% (graph_w_attrs_3$edges_df %>% names()))
  expect_true("edge_value" %in% (graph_w_attrs_4$edges_df %>% names()))
  expect_true("edge_value" %in% (graph_w_attrs_5$edges_df %>% names()))
  expect_true("edge_value" %in% (graph_w_attrs_6$edges_df %>% names()))
  expect_true("edge_value" %in% (graph_w_attrs_7$edges_df %>% names()))
  expect_true("edge_value" %in% (graph_w_attrs_8$edges_df %>% names()))
  expect_true("edge_value" %in% (graph_w_attrs_9$edges_df %>% names()))
  expect_true("edge_value" %in% (graph_w_attrs_10$edges_df %>% names()))
  expect_true("edge_value" %in% (graph_w_attrs_11$edges_df %>% names()))
  expect_true("edge_value" %in% (graph_w_attrs_12$edges_df %>% names()))
  expect_true("edge_value" %in% (graph_w_attrs_13$edges_df %>% names()))
  expect_true("edge_value" %in% (graph_w_attrs_14$edges_df %>% names()))
  expect_true("edge_value" %in% (graph_w_attrs_17$edges_df %>% names()))
  expect_true("edge_value" %in% (graph_w_attrs_18$edges_df %>% names()))

})
