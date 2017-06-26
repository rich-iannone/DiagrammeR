context("Copying, dropping, mutating, renaming, and recoding attrs")

test_that("Copying node attributes is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add two nodes to the graph
  graph <- add_node(graph)
  graph <- add_node(graph)

  # Add a node attribute
  graph <- set_node_attrs(graph, "value", 1)

  # Make a copy the `value` node attribute as
  # the `value_2` node attribute
  graph <-
    copy_node_attrs(
      graph, "value", "value_2")

  # Verify that the new column has been made
  expect_true("value_2" %in% colnames(graph$nodes_df))

  # Expect that the values in `value` and `value_2`
  # are identical
  expect_identical(
    graph$nodes_df$value,
    graph$nodes_df$value_2)

  # Expect an error if `node_attr_from` and
  # `node_attr_to` are identical
  expect_error(
    copy_node_attrs(
      graph, "value_2", "value_2"))

  # Expect an error if `node_attr_to` is
  # `nodes` or `node`
  expect_error(
    copy_node_attrs(
      graph, "value_2", "nodes"))

  expect_error(
    copy_node_attrs(
      graph, "value_2", "node"))

  # Expect an error if `node_attr_from` is not one
  # of the graph's columns
  expect_error(
    copy_node_attrs(
      graph, "values", "value_3"))
})

test_that("Copying edge attributes is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add 4 nodes to the graph
  graph <- add_n_nodes(graph, 4)

  # Add 5 edges to the graph
  graph <-
    add_edges_w_string(
      graph, "1->3 2->4 1->4 3->2")

  # Add an edge attribute
  graph <- set_edge_attrs(graph, "value", 1)

  # Make a copy the `value` node attribute as
  # the `value_2` node attribute
  graph <-
    copy_edge_attrs(
      graph, "value", "value_2")

  # Verify that the new column has been made
  expect_true("value_2" %in% colnames(graph$edges_df))

  # Expect that the values in `value` and `value_2`
  # are identical
  expect_identical(
    graph$edges_df$value,
    graph$edges_df$value_2)

  # Expect an error if `edge_attr_from` and
  # `edge_attr_to` are identical
  expect_error(
    copy_edge_attrs(
      graph, "value_2", "value_2"))

  # Expect an error if `edge_attr_to` is
  # `from` or `to`
  expect_error(
    copy_edge_attrs(
      graph, "value_2", "from"))

  expect_error(
    copy_edge_attrs(
      graph, "value_2", "to"))

  # Expect an error if `edge_attr_from` is not one
  # of the graph's columns
  expect_error(
    copy_edge_attrs(
      graph, "values", "value_3"))
})

test_that("Dropping node attributes is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add two nodes to the graph
  graph <- add_node(graph)
  graph <- add_node(graph)

  # Add a node attribute
  graph <- set_node_attrs(graph, "value", 1)

  # Expect an error if the length of `node_attr` > 1
  expect_error(
    drop_node_attrs(graph, c("value", "label")))

  # Expect an error if a column to drop does not exist
  expect_error(
    drop_node_attrs(graph, "value_2"))

  # Drop the `value` node attribute
  graph <- drop_node_attrs(graph, "value")

  # Verify that the `value` column is not present
  expect_true(!("value" %in% colnames(graph$nodes_df)))

  # Expect an error if `node_attr` is any of
  # `nodes`, `node`, `type`, or `label`
  expect_error(drop_node_attrs(graph, "nodes"))
  expect_error(drop_node_attrs(graph, "node"))
  expect_error(drop_node_attrs(graph, "type"))
  expect_error(drop_node_attrs(graph, "label"))
})

test_that("Dropping edge attributes is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add 4 nodes to the graph
  graph <- add_n_nodes(graph, 4)

  # Add 5 edges to the graph
  graph <-
    add_edges_w_string(
      graph, "1->3 2->4 1->4 3->2")

  # Add an edge attribute
  graph <- set_edge_attrs(graph, "value", 1)

  # Expect an error if the length of `edge_attr` > 1
  expect_error(
    drop_edge_attrs(graph, c("value", "rel")))

  # Expect an error if a column to drop does not exist
  expect_error(
    drop_edge_attrs(graph, "value_2"))

  # Drop the `value` edge attribute
  graph <- drop_edge_attrs(graph, "value")

  # Verify that the `value` column is not present
  expect_true(!("value" %in% colnames(graph$edges_df)))

  # Expect an error if `edge_attr` is any of
  # `from`, `to`, or `rel`
  expect_error(drop_edge_attrs(graph, "from"))
  expect_error(drop_edge_attrs(graph, "to"))
  expect_error(drop_edge_attrs(graph, "rel"))
})

test_that("Renaming node attributes is possible", {

  # Create a random graph with extra node attrs
  graph <-
    create_random_graph(
      5, 10, set_seed = 3) %>%
    set_node_attrs("shape", "circle")

  # Rename the `shape` attribute as `color`
  graph <- rename_node_attrs(graph, "shape", "color")

  # Expect that the `color` node attr is in the
  # graph's node data frame
  expect_true("color" %in% colnames(graph$nodes_df))

  # Expect that the `shape` node attr is not in the
  # graph's node data frame
  expect_false("shape" %in% colnames(graph$nodes_df))

  # Expect an error if trying to rename a node
  # attribute that doesn't exist in the graph
  expect_error(
    rename_node_attrs(graph, "perhipheries", "shape"))

  # Expect an error if `node_attr_from` and
  # `node_attr_to` are identical
  expect_error(
    rename_node_attrs(graph, "color", "color"))

  # Expect an error if `node_attr_to` is `id`
  expect_error(
    rename_node_attrs(graph, "color", "id"))

  # Expect an error if `node_attr_to` is any of
  # the existing node attrs
  expect_error(
    rename_node_attrs(graph, "value", "color"))

  # Expect an error if `node_attr_from` is any of
  # `id`, `type`, or `label`
  expect_error(
    rename_node_attrs(graph, "id", "node_id"))

  expect_error(
    rename_node_attrs(graph, "type", "type_2"))

  expect_error(
    rename_node_attrs(graph, "label", "label_2"))
})

test_that("Renaming edge attributes is possible", {

  # Create a random graph with extra edge attrs
  graph <-
    create_random_graph(
      5, 10, set_seed = 3) %>%
    set_edge_attrs("penwidth", 5) %>%
    set_edge_attrs("arrowhead", "dot")

  # Rename the `penwidth` attribute as `width`
  graph <- rename_edge_attrs(graph, "penwidth", "width")

  # Expect that the `width` node attr is in the
  # graph's edge data frame
  expect_true("width" %in% colnames(graph$edges_df))

  # Expect that the `penwidth` node attr is not in the
  # graph's node data frame
  expect_false("penwidth" %in% colnames(graph$edges_df))

  # Expect an error if trying to rename an edge
  # attribute that doesn't exist in the graph
  expect_error(
    rename_edge_attrs(graph, "perhipheries", "shape"))

  # Expect an error if `edge_attr_from` and
  # `edge_attr_to` are identical
  expect_error(
    rename_edge_attrs(graph, "width", "width"))

  # Expect an error if `node_attr_to` is `from` or `to`
  expect_error(
    rename_edge_attrs(graph, "width", "from"))

  expect_error(
    rename_edge_attrs(graph, "width", "to"))

  # Expect an error if `edge_attr_to` is any of
  # the existing edge attrs
  expect_error(
    rename_edge_attrs(graph, "width", "arrowhead"))

  # Expect an error if `edge_attr_from` is any of
  # `from`, `to`, or `rel`
  expect_error(
    rename_edge_attrs(graph, "from", "from_2"))

  expect_error(
    rename_edge_attrs(graph, "to", "to_2"))

  expect_error(
    rename_edge_attrs(graph, "rel", "rel_2"))
})

test_that("Mutating node attributes is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add two nodes to the graph
  graph <- add_node(graph)
  graph <- add_node(graph)

  # Add node attributes
  graph <- set_node_attrs(graph, "value", 1)
  graph <- set_node_attrs(graph, "shape", "square")

  # Expect an error if `node_attr_from` is not one
  # of the graph's columns
  expect_error(
    mutate_node_attrs(
      graph, "value_2", "~ * 2"))

  # Mutate the `value` node attribute
  graph <-
    mutate_node_attrs(
      graph, "value", "~ * 2")

  # Expect each value in `value` to be 2
  expect_equal(graph$nodes_df$value, c(2, 2))

  # Mutate the `value` node attribute again but
  # also copy mutated values into `value_2`
  graph <-
    mutate_node_attrs(
      graph, "value", "~ * 2",
      node_attr_to = "value_2")

  # Verify that the new column has been made
  expect_true("value_2" %in% colnames(graph$nodes_df))

  # Expect each value in `value_2` to be 4
  expect_equal(graph$nodes_df$value_2, c(4, 4))

  # Expect an error if `node_attr_to` is `id`
  expect_error(
    mutate_node_attrs(
      graph, "value", "~ * 2",
      node_attr_to = "id"))

  # Mutate the `value` node attribute and
  # copy and overwrite mutated values into `value_2`
  graph <-
    mutate_node_attrs(
      graph, "value", "~ * 4",
      node_attr_to = "value_2")

  # Expect each value in `value_2` to be 8
  expect_equal(graph$nodes_df$value_2, c(8, 8))
})

test_that("Mutating edge attributes is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add 4 nodes to the graph
  graph <- add_n_nodes(graph, 4)

  # Add 5 edges to the graph
  graph <-
    add_edges_w_string(
      graph, "1->3 2->4 1->4 3->2")

  # Add edge attributes
  graph <- set_edge_attrs(graph, "value", 1)
  graph <- set_edge_attrs(graph, "color", "red")

  # Expect an error if `edge_attr_from` is not one
  # of the graph's columns
  expect_error(
    mutate_edge_attrs(
      graph, "value_2", "~ * 2"))

  # Mutate the `value` edge attribute
  graph <-
    mutate_edge_attrs(
      graph, "value", "~ * 2")

  # Expect each value in `value` to be 2
  expect_equal(graph$edges_df$value,
               c(2, 2, 2, 2))

  # Mutate the `value` edge attribute again but
  # also copy mutated values into `value_2`
  graph <-
    mutate_edge_attrs(
      graph, "value", "~ * 2",
      edge_attr_to = "value_2")

  # Verify that the new column has been made
  expect_true("value_2" %in% colnames(graph$edges_df))

  # Expect each value in `value_2` to be 4
  expect_equal(graph$edges_df$value_2,
               c(4, 4, 4, 4))

  # Expect an error if `edge_attr_to` is `from` or `to`
  expect_error(
    mutate_edge_attrs(
      graph, "value", "~ * 2",
      edge_attr_to = "from"))

  expect_error(
    mutate_edge_attrs(
      graph, "value", "~ * 2",
      edge_attr_to = "to"))

  # Mutate the `value` edge attribute and
  # copy and overwrite mutated values into `value_2`
  graph <-
    mutate_edge_attrs(
      graph, "value", "~ * 4",
      edge_attr_to = "value_2")

  # Expect each value in `value_2` to be 8
  expect_equal(graph$edges_df$value_2,
               c(8, 8, 8, 8))
})

test_that("Recoding node attributes is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add two nodes to the graph
  graph <- add_node(graph)
  graph <- add_node(graph)

  # Add node attributes
  graph <- set_node_attrs(graph, "value", 1)
  graph <- set_node_attrs(graph, "shape", "square")

  # Expect an error if `node_attr_from` is not one
  # of the graph's columns
  expect_error(
    recode_node_attrs(
      graph, "value_2", "1->2"))

  # Recode the `value` node attribute
  graph <-
    recode_node_attrs(
      graph, "value", "1->2")

  # Expect each value in `value` to be 2
  expect_equal(graph$nodes_df$value, c(2, 2))

  # Recode the `value` node attribute again but
  # also copy recoded values into `value_2`
  graph <-
    recode_node_attrs(
      graph, "value", "2->4",
      node_attr_to = "value_2")

  # Verify that the new column has been made
  expect_true("value_2" %in% colnames(graph$nodes_df))

  # Expect each value in `value_2` to be 4
  expect_equal(graph$nodes_df$value_2, c(4, 4))

  # Expect an error if `node_attr_to` is `id` or `node`
  expect_error(
    recode_node_attrs(
      graph, "value", "2->4",
      node_attr_to = "id"))

  expect_error(
    recode_node_attrs(
      graph, "value", "2->4",
      node_attr_to = "nodes"))

  # Recode the `value` node attribute and
  # copy and overwrite recoded values into `value_2`
  graph <-
    recode_node_attrs(
      graph, "value", "2->8",
      node_attr_to = "value_2")

  # Expect each value in `value_2` to be 8
  expect_equal(graph$nodes_df$value_2, c(8, 8))

  # Create another graph but use different node
  # attribute values
  graph <- create_graph()

  graph <- add_n_nodes(graph, 4)

  graph <-
    set_node_attrs(graph, "value",
                   c(1, 1, 2, 3))

  graph <-
    set_node_attrs(graph, "shape",
                   c("square", "square",
                     "circle", "triangle"))

  # Recode the `value` node attribute, using the
  # `otherwise` argument
  graph <-
    recode_node_attrs(
      graph, "value", "1->2", otherwise = 4)

  # Expect values in `value` to be 2, 2, 4, 4
  expect_equal(graph$nodes_df$value,
               c(2, 2, 4, 4))

  # Recode the `value` node attribute again but
  # also copy recoded values into `value_2`
  graph <-
    recode_node_attrs(
      graph, "value", "2->4",
      otherwise = 8, node_attr_to = "value_2")

  # Expect values in `value_2` to be 4, 4, 8, 8
  expect_equal(graph$nodes_df$value_2,
               c(4, 4, 8, 8))

  # Recode the `value` node attribute and
  # copy and overwrite recoded values into `value_2`
  graph <-
    recode_node_attrs(
      graph, "value", "2->8",
      otherwise = 16, node_attr_to = "value_2")

  # Expect values in `value_2` to be 8, 8, 16, 16
  expect_equal(graph$nodes_df$value_2,
               c(8, 8, 16, 16))

  # Recode the `shape` node attribute
  graph <-
    recode_node_attrs(
      graph, "shape", "square -> rectangle")

  # Expect certain values in `shape`
  expect_equal(graph$nodes_df$shape,
               c("rectangle", "rectangle",
                 "circle", "triangle"))

  # Recode the `shape` node attribute using the
  # `otherwise` argument
  graph <-
    recode_node_attrs(
      graph, "shape",
      "rectangle -> circle",
      "circle -> square",
      otherwise = "diamond")

  # Expect certain values in `shape`
  expect_equal(graph$nodes_df$shape,
               c("circle", "circle",
                 "square", "diamond"))

  # Recode the `shape` node attribute and copy into
  # results to `shape_2`
  graph <-
    recode_node_attrs(
      graph, "shape",
      "circle -> rectangle",
      "square -> circle",
      node_attr_to = "shape_2")

  # Expect certain values in `shape_2`
  expect_equal(graph$nodes_df$shape_2,
               c("rectangle", "rectangle",
                 "circle", "diamond"))

  # Recode the `shape` node attribute and overwrite
  # results in `shape_2`
  graph <-
    recode_node_attrs(
      graph, "shape",
      "circle -> rectangle",
      "square -> circle",
      otherwise = "triangle",
      node_attr_to = "shape_2")

  # Expect certain values in `shape_2`
  expect_equal(graph$nodes_df$shape_2,
               c("rectangle", "rectangle",
                 "circle", "triangle"))
})

test_that("Recoding edge attributes is possible", {

  # Create an empty graph
  graph <- create_graph()

  # Add 4 nodes to the graph
  graph <- add_n_nodes(graph, 4)

  # Add 5 edges to the graph
  graph <-
    add_edges_w_string(
      graph, "1->3 2->4 1->4 3->2")

  # Add edge attributes
  graph <-
    set_edge_attrs(graph, "value",
                   c(1, 1, 2, 3))

  graph <-
    set_edge_attrs(graph, "color",
                   c("red", "red",
                     "blue", "black"))

  # Expect an error if `edge_attr_from` is not one
  # of the graph's columns
  expect_error(
    recode_edge_attrs(
      graph, "value_2", "1->2"))

  # Recode the `value` edge attribute
  graph <-
    recode_edge_attrs(
      graph, "value", "1->2")

  # Expect values in `value` to be 2, 2, 2, 3
  expect_equal(graph$edges_df$value,
               c(2, 2, 2, 3))

  # Recode the `value` edge attribute again but
  # also copy recoded values into `value_2`
  graph <-
    recode_edge_attrs(
      graph, "value", "2->4",
      edge_attr_to = "value_2")

  # Verify that the new column has been made
  expect_true("value_2" %in% colnames(graph$edges_df))

  # Expect certain values in `value_2`
  expect_equal(graph$edges_df$value_2,
               c(4, 4, 4, 3))

  # Expect an error if `edge_attr_to` is `from` or `to`
  expect_error(
    recode_edge_attrs(
      graph, "value", "2->4",
      edge_attr_to = "from"))

  expect_error(
    recode_edge_attrs(
      graph, "value", "2->4",
      edge_attr_to = "to"))

  # Recode the `value` edge attribute and
  # copy and overwrite recoded values into `value_2`
  graph <-
    recode_edge_attrs(
      graph, "value", "2->8",
      edge_attr_to = "value_2")

  # Expect certain values in `value_2`
  expect_equal(graph$edges_df$value_2,
               c(8, 8, 8, 3))

  # Recode the `value` edge attribute, using the
  # `otherwise` argument
  graph <-
    recode_edge_attrs(
      graph, "value", "2->4", otherwise = 8)

  # Expect certain values in `value`
  expect_equal(graph$edges_df$value,
               c(4, 4, 4, 8))

  # Recode the `value` edge attribute again but
  # also copy recoded values into `value_3`
  graph <-
    recode_edge_attrs(
      graph, "value", "4->8",
      otherwise = 16, edge_attr_to = "value_3")

  # Expect certain values in `value_3`
  expect_equal(graph$edges_df$value_3,
               c(8, 8, 8, 16))

  # Recode the `value` edge attribute and
  # copy and overwrite recoded values into `value_3`
  graph <-
    recode_edge_attrs(
      graph, "value", "4->12",
      otherwise = 16, edge_attr_to = "value_3")

  # Expect certain values in `value_3`
  expect_equal(graph$edges_df$value_3,
               c(12, 12, 12, 16))

  # Recode the `color` edge attribute
  graph <-
    recode_edge_attrs(
      graph, "color", "red->gray")

  # Expect certain values in `color`
  expect_equal(graph$edges_df$color,
               c("gray", "gray",
                 "blue", "black"))

  # Recode the `color` edge attribute using the
  # `otherwise` argument
  graph <-
    recode_edge_attrs(
      graph, "color",
      "gray -> blue",
      "blue -> green",
      otherwise = "yellow")

  # Expect certain values in `color`
  expect_equal(graph$edges_df$color,
               c("blue", "blue",
                 "green", "yellow"))

  # Recode the `color` edge attribute and copy into
  # results to `color_2`
  graph <-
    recode_edge_attrs(
      graph, "color",
      "blue -> yellow",
      "green -> blue",
      edge_attr_to = "color_2")

  # Expect certain values in `color_2`
  expect_equal(
    graph$edges_df$color_2,
    c("yellow", "yellow",
      "blue", "yellow"))

  # Recode the `color` node attribute and overwrite
  # results in `color_2`
  graph <-
    recode_edge_attrs(
      graph, "color",
      "blue -> green",
      otherwise = "black",
      edge_attr_to = "color_2")

  # Expect certain values in `color_2`
  expect_equal(graph$edges_df$color_2,
               c("green", "green",
                 "black", "black"))
})
