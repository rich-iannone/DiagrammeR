# Copying, dropping, mutating, renaming, and recoding attrs

test_that("Copying node attributes is possible", {

  # Create a starting graph
  graph <-
    create_graph() %>%
    add_node() %>%
    add_node() %>%
    set_node_attrs(
      node_attr = value,
      values = 1)

  # Make a copy the `value` node attribute as
  # the `value_2` node attribute
  graph <-
    graph %>%
    copy_node_attrs(
      node_attr_from = value,
      node_attr_to = value_2)

  # Verify that the new column has been made
  expect_contains(colnames(graph$nodes_df), "value_2")

  # Expect that the values in `value` and `value_2`
  # are identical
  expect_identical(
    graph$nodes_df$value,
    graph$nodes_df$value_2)

  # Expect an error if `node_attr_from` and
  # `node_attr_to` are identical
  expect_error(
    graph %>%
    copy_node_attrs(
      node_attr_from = value_2,
      node_attr_to = value_2))

  # Expect an error if `node_attr_to` is
  # `nodes` or `node`
  expect_error(
    graph %>%
      copy_node_attrs(
        node_attr_from = value_2,
        node_attr_to = nodes))

  expect_error(
    graph %>%
      copy_node_attrs(
        node_attr_from = value_2,
        node_attr_to = node))

  # Expect an error if `node_attr_from` is not one
  # of the graph's columns
  expect_error(
    graph %>%
    copy_node_attrs(
      node_attr_from = values,
      node_attr_to = value_3))
})

test_that("Copying edge attributes is possible", {

  # Create a starting graph
  graph <-
    create_graph() %>%
    add_n_nodes(n = 4) %>%
    add_edges_w_string(
      edges = "1->3 2->4 1->4 3->2") %>%
    set_edge_attrs(
      edge_attr = value,
      values = 1)

  # Make a copy the `value` node attribute as
  # the `value_2` node attribute
  graph <-
    graph %>%
    copy_edge_attrs(
      edge_attr_from = value,
      edge_attr_to = value_2)

  # Verify that the new column has been made
  expect_contains(colnames(graph$edges_df), "value_2")

  # Expect that the values in `value` and `value_2`
  # are identical
  expect_identical(
    graph$edges_df$value,
    graph$edges_df$value_2)

  # Expect an error if `edge_attr_from` and
  # `edge_attr_to` are identical
  expect_error(
    graph %>%
    copy_edge_attrs(
      edge_attr_from = value_2,
      edge_attr_to = value_2))

  # Expect an error if `edge_attr_to` is
  # `from` or `to`
  expect_error(
    graph %>%
    copy_edge_attrs(
      edge_attr_from = value_2,
      edge_attr_to = from))

  expect_error(
    graph %>%
    copy_edge_attrs(
      edge_attr_from = value_2,
      edge_attr_to = to))

  # Expect an error if `edge_attr_from` is not one
  # of the graph's columns
  expect_error(
    graph %>%
    copy_edge_attrs(
      edge_attr_from = values,
      edge_attr_to = value_3))
})

test_that("Dropping node attributes is possible", {

  # Create a starting graph
  graph <-
    create_graph() %>%
    add_node() %>%
    add_node() %>%
    set_node_attrs(
      node_attr = value,
      values = 1)

  # Expect an error if a column to drop does not exist
  expect_error(
    graph %>%
    drop_node_attrs(
      node_attr = value_2))

  # Drop the `value` node attribute
  graph <-
    graph %>%
    drop_node_attrs(node_attr = value)

  # Verify that the `value` column is not present
  expect_false(
    "value" %in% colnames(graph$nodes_df))

  # Expect an error if `node_attr` is any of
  # `nodes`, `node`, `type`, or `label`
  expect_error(
    drop_node_attrs(graph, node_attr = nodes))

  expect_error(
    drop_node_attrs(graph, node_attr = node))

  expect_error(
    drop_node_attrs(graph, node_attr = type))

  expect_error(
    drop_node_attrs(graph, node_attr = label))
})

test_that("Dropping edge attributes is possible", {

  # Create a starting graph
  graph <-
    create_graph() %>%
    add_n_nodes(n = 4) %>%
    add_edges_w_string(
      edges = "1->3 2->4 1->4 3->2") %>%
    set_edge_attrs(
      edge_attr = value,
      values = 1)

  # Expect an error if a column to drop does not exist
  expect_error(
    graph %>%
    drop_edge_attrs(edge_attr = value_2))

  # Drop the `value` edge attribute
  graph <-
    graph %>%
    drop_edge_attrs(edge_attr = value)

  # Verify that the `value` column is not present
  expect_false(
    "value" %in% colnames(graph$edges_df))

  # Expect an error if `edge_attr` is any of
  # `from`, `to`, or `rel`
  expect_error(
    drop_edge_attrs(graph, edge_attr = from))

  expect_error(
    drop_edge_attrs(graph, edge_attr = to))

  expect_error(
    drop_edge_attrs(graph, edge_attr = rel))
})

test_that("Renaming node attributes is possible", {

  # Create a random graph with extra node attrs
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      set_seed = 23) %>%
    set_node_attrs(
      node_attr = shape,
      values = "circle")

  # Rename the `shape` attribute as `color`
  graph <-
    graph %>%
    rename_node_attrs(
      node_attr_from = shape,
      node_attr_to = color)

  # Expect that the `color` node attr is in the
  # graph's node data frame
  expect_contains(colnames(graph$nodes_df), "color")

  # Expect that the `shape` node attr is not in the
  # graph's node data frame
  expect_false(
    "shape" %in% colnames(graph$nodes_df))

  # Expect an error if trying to rename a node
  # attribute that doesn't exist in the graph
  expect_error(
    graph %>%
    rename_node_attrs(
      node_attr_from = perhipheries,
      node_attr_to = shape))

  # Expect an error if `node_attr_from` and
  # `node_attr_to` are identical
  expect_error(
    graph %>%
    rename_node_attrs(
      node_attr_from = color,
      node_attr_to = color))

  # Expect an error if `node_attr_to` is `id`
  expect_error(
    graph %>%
    rename_node_attrs(
      node_attr_from = color,
      node_attr_to = id))

  # Expect an error if `node_attr_to` is any of
  # the existing node attrs
  expect_error(
    graph %>%
    rename_node_attrs(
      node_attr_from = value,
      node_attr_to = color))

  # Expect an error if `node_attr_from` is any of
  # `id`, `type`, or `label`
  expect_error(
    graph %>%
    rename_node_attrs(
      node_attr_from = id,
      node_attr_to = node_id))

  expect_error(
    graph %>%
    rename_node_attrs(
      node_attr_from = type,
      node_attr_to = type_2))

  expect_error(
    graph %>%
    rename_node_attrs(
      node_attr_from = label,
      node_attr_to = label_2))
})

test_that("Renaming edge attributes is possible", {

  # Create a random graph with extra edge attrs
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 5,
      m = 10,
      set_seed = 23) %>%
    set_edge_attrs(
      edge_attr = penwidth,
      values = 5) %>%
    set_edge_attrs(
      edge_attr = arrowhead,
      values = "dot")

  # Rename the `penwidth` attribute as `width`
  graph <-
    graph %>%
    rename_edge_attrs(
      edge_attr_from = penwidth,
      edge_attr_to = width)

  # Expect that the `width` node attr is in the
  # graph's edge data frame
  expect_contains(colnames(graph$edges_df), "width")

  # Expect that the `penwidth` node attr is not in the
  # graph's node data frame
  expect_false(
    "penwidth" %in% colnames(graph$edges_df))

  # Expect an error if trying to rename an edge
  # attribute that doesn't exist in the graph
  expect_error(
    graph %>%
    rename_edge_attrs(
      edge_attr_from = perhipheries,
      edge_attr_to = shape))

  # Expect an error if `edge_attr_from` and
  # `edge_attr_to` are identical
  expect_error(
    graph %>%
    rename_edge_attrs(
      edge_attr_from = width,
      edge_attr_to = width))

  # Expect an error if `node_attr_to` is `from` or `to`
  expect_error(
    graph %>%
    rename_edge_attrs(
      edge_attr_from = width,
      edge_attr_to = from))

  expect_error(
    graph %>%
    rename_edge_attrs(
      edge_attr_from = width,
      edge_attr_to = to))

  # Expect an error if `edge_attr_to` is any of
  # the existing edge attrs
  expect_error(
    graph %>%
    rename_edge_attrs(
      edge_attr_from = width,
      edge_attr_to = arrowhead))

  # Expect an error if `edge_attr_from` is any of
  # `from`, `to`, or `rel`
  expect_error(
    graph %>%
    rename_edge_attrs(
      edge_attr_from = from,
      edge_attr_to = from_2))

  expect_error(
    graph %>%
    rename_edge_attrs(
      edge_attr_from = to,
      edge_attr_to = to_2))

  expect_error(
    graph %>%
    rename_edge_attrs(
      edge_attr_from = rel,
      edge_attr_to = rel_2))
})

test_that("Mutating node attributes is possible", {

  # Create a starting graph
  graph <-
    create_graph() %>%
    add_node() %>%
    add_node() %>%
    set_node_attrs(
      node_attr = value,
      values = 1) %>%
    set_node_attrs(
      node_attr = shape,
      values = "square")

  # Mutate the `value` node attribute
  graph <-
    graph %>%
    mutate_node_attrs(
      value = value * 2)

  # Expect each value in `value` to be 2
  expect_equal(
    graph$nodes_df$value,
    c(2, 2))

  # Create node attribute `value_2`
  graph <-
    graph %>%
    mutate_node_attrs(
      value_2 = value * 2)

  # Verify that the new column has been made
  expect_contains(colnames(graph$nodes_df), "value_2")

  # Expect each value in `value_2` to be 4
  expect_equal(
    graph$nodes_df$value_2,
    c(4, 4))

  # Expect an error if `node_attr_to` is `id`
  expect_error(
    mutate_node_attrs(
      id = value * 2))

  # Overwrite `value_2` with new values
  graph <-
    graph %>%
    mutate_node_attrs(
      value_2 = value * 4)

  # Expect each value in `value_2` to be 8
  expect_equal(
    graph$nodes_df$value_2,
    c(8, 8))
})

test_that("Mutating edge attributes is possible", {

  # Create a starting graph
  graph <-
    create_graph() %>%
    add_n_nodes(n = 4) %>%
    add_edges_w_string(
      edges = "1->3 2->4 1->4 3->2") %>%
    set_edge_attrs(
      edge_attr = value,
      values = 1) %>%
    set_edge_attrs(
      edge_attr = color,
      values = "red")

  # Mutate the `value` edge attribute
  graph <-
    graph %>%
    mutate_edge_attrs(
      value = value * 2)

  # Expect each value in `value` to be 2
  expect_equal(
    graph$edges_df$value,
    c(2, 2, 2, 2))

  # Create node attribute `value_2`
  graph <-
    graph %>%
    mutate_edge_attrs(
      value_2 = value * 2)

  # Verify that the new column has been made
  expect_contains(colnames(graph$edges_df), "value_2")

  # Expect each value in `value_2` to be 4
  expect_equal(
    graph$edges_df$value_2,
    c(4, 4, 4, 4))

  # Expect an error if `edge_attr_to`
  # is `id`, `from`, or `to`
  expect_error(
    mutate_edge_attrs(
      id = value * 2))

  expect_error(
    mutate_edge_attrs(
      from = value * 2))

  expect_error(
    mutate_edge_attrs(
      to = value * 2))

  # Overwrite `value_2` with new values
  graph <-
    graph %>%
    mutate_edge_attrs(
      value_2 = value * 4)

  # Expect each value in `value_2` to be 8
  expect_equal(
    graph$edges_df$value_2,
    c(8, 8, 8, 8))
})

test_that("Mutating node attributes with a selection is possible", {

  # Create a starting graph
  graph <-
    create_graph() %>%
    add_node() %>%
    add_node() %>%
    set_node_attrs(
      node_attr = value,
      values = 1) %>%
    set_node_attrs(
      node_attr = shape,
      values = "square") %>%
    select_nodes_by_id(nodes = 1)

  # Mutate the `value` node attribute
  # for the selected node
  graph <-
    graph %>%
    mutate_node_attrs_ws(
      value = value * 2)

  # Expect specific values in `value`
  expect_equal(
    graph$nodes_df$value,
    c(2, 1))
})

test_that("Mutating edge attributes with a selection is possible", {

  # Create a starting graph
  graph <-
    create_graph() %>%
    add_n_nodes(n = 4) %>%
    add_edges_w_string(
      edges = "1->3 2->4 1->4 3->2") %>%
    set_edge_attrs(
      edge_attr = value,
      values = 1) %>%
    set_edge_attrs(
      edge_attr = color,
      values = "red") %>%
    select_edges_by_edge_id(edges = 1)

  # Mutate the `value` edge attribute
  # for the selected edge
  graph <-
    graph %>%
    mutate_edge_attrs_ws(
      value = value * 2)

  # Expect specific values in `value`
  expect_equal(
    graph$edges_df$value,
    c(2, 1, 1, 1))
})

test_that("Recoding node attributes is possible", {

  # Create a starting graph
  graph <-
    create_graph() %>%
    add_node() %>%
    add_node() %>%
    set_node_attrs(
      node_attr = value,
      values = 1) %>%
    set_node_attrs(
      node_attr = shape,
      values = "square")

  # Expect an error if `node_attr_from`
  # is not one of the graph's columns
  expect_error(
    recode_node_attrs(
      graph, "value_2", "1->2"))

  # Recode the `value` node attribute
  graph <-
    recode_node_attrs(
      graph, "value", "1->2")

  # Expect each value in `value` to be 2
  expect_equal(
    graph$nodes_df$value,
    c(2, 2))

  # Recode the `value` node attribute again but
  # also copy recoded values into `value_2`
  graph <-
    graph %>%
    recode_node_attrs(
      node_attr_from = value,
      "2->4",
      node_attr_to = value_2)

  # Verify that the new column has been made
  expect_contains(colnames(graph$nodes_df), "value_2")

  # Expect each value in `value_2` to be 4
  expect_equal(
    graph$nodes_df$value_2,
    c(4, 4))

  # Expect an error if `node_attr_to` is `id` or `node`
  expect_error(
    graph %>%
    recode_node_attrs(
      node_attr_from = value,
      "2->4",
      node_attr_to = id))

  expect_error(
    graph %>%
    recode_node_attrs(
      node_attr_from = value,
      "2->4",
      node_attr_to = nodes))

  # Recode the `value` node attribute and
  # copy and overwrite recoded values into `value_2`
  graph <-
    graph %>%
    recode_node_attrs(
      node_attr_from = value,
      "2->8",
      node_attr_to = value_2)

  # Expect each value in `value_2` to be 8
  expect_equal(
    graph$nodes_df$value_2,
    c(8, 8))

  # Create another graph but use different node
  # attribute values
  graph <-
    create_graph() %>%
    add_n_nodes(n = 4) %>%
    set_node_attrs(
      node_attr = value,
      values = c(1, 1, 2, 3)) %>%
    set_node_attrs(
      node_attr = shape,
      values = c("square", "square",
                 "circle", "triangle"))

  # Recode the `value` node attribute, using the
  # `otherwise` argument
  graph <-
    graph %>%
    recode_node_attrs(
      node_attr_from = value,
      "1->2",
      otherwise = 4)

  # Expect values in `value` to be 2, 2, 4, 4
  expect_equal(
    graph$nodes_df$value,
    c(2, 2, 4, 4))

  # Recode the `value` node attribute again but
  # also copy recoded values into `value_2`
  graph <-
    graph %>%
    recode_node_attrs(
      node_attr_from = value,
      "2->4",
      otherwise = 8,
      node_attr_to = value_2)

  # Expect values in `value_2` to be 4, 4, 8, 8
  expect_equal(
    graph$nodes_df$value_2,
    c(4, 4, 8, 8))

  # Recode the `value` node attribute and
  # copy and overwrite recoded values into `value_2`
  graph <-
    graph %>%
    recode_node_attrs(
      node_attr_from = value,
      "2->8",
      otherwise = 16,
      node_attr_to = value_2)

  # Expect values in `value_2` to be 8, 8, 16, 16
  expect_equal(
    graph$nodes_df$value_2,
    c(8, 8, 16, 16))

  # Recode the `shape` node attribute
  graph <-
    graph %>%
    recode_node_attrs(
      node_attr_from = shape,
      "square -> rectangle")

  # Expect certain values in `shape`
  expect_equal(
    graph$nodes_df$shape,
    c("rectangle", "rectangle",
      "circle", "triangle"))

  # Recode the `shape` node attribute using the
  # `otherwise` argument
  graph <-
    graph %>%
    recode_node_attrs(
      node_attr_from = shape,
      "rectangle -> circle",
      "circle -> square",
      otherwise = "diamond")

  # Expect certain values in `shape`
  expect_equal(
    graph$nodes_df$shape,
    c("circle", "circle",
      "square", "diamond"))

  # Recode the `shape` node attribute and copy into
  # results to `shape_2`
  graph <-
    graph %>%
    recode_node_attrs(
      node_attr_from = shape,
      "circle -> rectangle",
      "square -> circle",
      node_attr_to = shape_2)

  # Expect certain values in `shape_2`
  expect_equal(
    graph$nodes_df$shape_2,
    c("rectangle", "rectangle",
      "circle", "diamond"))

  # Recode the `shape` node attribute and overwrite
  # results in `shape_2`
  graph <-
    graph %>%
    recode_node_attrs(
      node_attr_from = shape,
      "circle -> rectangle",
      "square -> circle",
      otherwise = "triangle",
      node_attr_to = shape_2)

  # Expect certain values in `shape_2`
  expect_equal(
    graph$nodes_df$shape_2,
    c("rectangle", "rectangle",
      "circle", "triangle"))
})

test_that("Recoding edge attributes is possible", {

  # Create a starting graph
  graph <-
    create_graph() %>%
    add_n_nodes(n = 4) %>%
    add_edges_w_string(
      edges = "1->3 2->4 1->4 3->2") %>%
    set_edge_attrs(
      edge_attr = value,
      values = c(1, 1, 2, 3)) %>%
    set_edge_attrs(
      edge_attr = color,
      values = c("red", "red",
                 "blue", "black"))

  # Expect an error if `edge_attr_from` is not one
  # of the graph's columns
  expect_error(
    graph %>%
    recode_edge_attrs(
      edge_attr_from = value_2,
      "1->2"))

  # Recode the `value` edge attribute
  graph <-
    graph %>%
    recode_edge_attrs(
      edge_attr_from = value,
      "1->2")

  # Expect values in `value` to be 2, 2, 2, 3
  expect_equal(
    graph$edges_df$value,
    c(2, 2, 2, 3))

  # Recode the `value` edge attribute again but
  # also copy recoded values into `value_2`
  graph <-
    graph %>%
    recode_edge_attrs(
      edge_attr_from = value,
      "2->4",
      edge_attr_to = value_2)

  # Verify that the new column has been made
  expect_contains(colnames(graph$edges_df), "value_2")

  # Expect certain values in `value_2`
  expect_equal(
    graph$edges_df$value_2,
    c(4, 4, 4, 3))

  # Expect an error if `edge_attr_to` is `from` or `to`
  expect_error(
    graph %>%
    recode_edge_attrs(
      edge_attr_from = value,
      "2->4",
      edge_attr_to = from))

  expect_error(
    graph %>%
    recode_edge_attrs(
      edge_attr_from = value,
      "2->4",
      edge_attr_to = to))

  # Recode the `value` edge attribute and
  # copy and overwrite recoded values into `value_2`
  graph <-
    graph %>%
    recode_edge_attrs(
      edge_attr_from = value,
      "2->8",
      edge_attr_to = value_2)

  # Expect certain values in `value_2`
  expect_equal(
    graph$edges_df$value_2,
    c(8, 8, 8, 3))

  # Recode the `value` edge attribute, using the
  # `otherwise` argument
  graph <-
    graph %>%
    recode_edge_attrs(
      edge_attr_from = value,
      "2->4",
      otherwise = 8)

  # Expect certain values in `value`
  expect_equal(
    graph$edges_df$value,
    c(4, 4, 4, 8))

  # Recode the `value` edge attribute again but
  # also copy recoded values into `value_3`
  graph <-
    graph %>%
    recode_edge_attrs(
      edge_attr_from = value,
      "4->8",
      otherwise = 16,
      edge_attr_to = value_3)

  # Expect certain values in `value_3`
  expect_equal(
    graph$edges_df$value_3,
    c(8, 8, 8, 16))

  # Recode the `value` edge attribute and
  # copy and overwrite recoded values into `value_3`
  graph <-
    graph %>%
    recode_edge_attrs(
      edge_attr_from = "value",
      "4->12",
      otherwise = 16,
      edge_attr_to = value_3)

  # Expect certain values in `value_3`
  expect_equal(
    graph$edges_df$value_3,
    c(12, 12, 12, 16))

  # Recode the `color` edge attribute
  graph <-
    graph %>%
    recode_edge_attrs(
      edge_attr_from = color,
      "red->gray")

  # Expect certain values in `color`
  expect_equal(
    graph$edges_df$color,
    c("gray", "gray",
      "blue", "black"))

  # Recode the `color` edge attribute using the
  # `otherwise` argument
  graph <-
    graph %>%
    recode_edge_attrs(
      edge_attr_from = color,
      "gray -> blue",
      "blue -> green",
      otherwise = "yellow")

  # Expect certain values in `color`
  expect_equal(
    graph$edges_df$color,
    c("blue", "blue",
      "green", "yellow"))

  # Recode the `color` edge attribute and copy into
  # results to `color_2`
  graph <-
    graph %>%
    recode_edge_attrs(
      edge_attr_from = color,
      "blue -> yellow",
      "green -> blue",
      edge_attr_to = color_2)

  # Expect certain values in `color_2`
  expect_equal(
    graph$edges_df$color_2,
    c("yellow", "yellow",
      "blue", "yellow"))

  # Recode the `color` node attribute and overwrite
  # results in `color_2`
  graph <-
    graph %>%
    recode_edge_attrs(
      edge_attr_from = color,
      "blue -> green",
      otherwise = "black",
      edge_attr_to = color_2)

  # Expect certain values in `color_2`
  expect_equal(
    graph$edges_df$color_2,
    c("green", "green",
      "black", "black"))
})
