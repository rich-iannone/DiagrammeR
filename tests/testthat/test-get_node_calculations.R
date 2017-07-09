context("Get node calculations")

test_that("Getting betweenness is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 1)

  betweenness_vals <- get_betweenness(graph)

  # Expect a data frame as output
  expect_is(
    betweenness_vals, "data.frame")

  # Expect 2 columns in the df
  expect_equal(
    ncol(betweenness_vals), 2)

  # Expect 10 rows in the df
  expect_equal(
    nrow(betweenness_vals), 10)

  # Expect node ID values in the first column
  expect_identical(
    betweenness_vals[, 1],
    as.character(1:10))

  # Expect numerical values in the
  # second column
  expect_is(
    betweenness_vals[, 2],
    "numeric")
})

test_that("Getting bridging is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 1)

  bridging_vals <- get_bridging(graph)

  # Expect a data frame as output
  expect_is(
    bridging_vals, "data.frame")

  # Expect 2 columns in the df
  expect_equal(
    ncol(bridging_vals), 2)

  # Expect 10 rows in the df
  expect_equal(
    nrow(bridging_vals), 10)

  # Expect node ID values in the first column
  expect_identical(
    bridging_vals[,1],
    as.character(1:10))

  # Expect numerical values in the
  # second column
  expect_is(
    bridging_vals[, 2],
    "numeric")
})

test_that("Getting closeness is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 1,
      directed = TRUE)

  # Get closness values with `direction = all`
  closeness_vals_all <- get_closeness(graph)

  # Expect a data frame as output
  expect_is(
    closeness_vals_all, "data.frame")

  # Expect 2 columns in the df
  expect_equal(
    ncol(closeness_vals_all), 2)

  # Expect 10 rows in the df
  expect_equal(
    nrow(closeness_vals_all), 10)

  # Expect node ID values in the first column
  expect_identical(
    closeness_vals_all[,1],
    as.character(1:10))

  # Get closness values with `direction = out`
  closeness_vals_out <-
    get_closeness(
      graph,
      direction = "out")

  # Expect a data frame as output
  expect_is(
    closeness_vals_out, "data.frame")

  # Expect 2 columns in the df
  expect_equal(
    ncol(closeness_vals_out), 2)

  # Expect 10 rows in the df
  expect_equal(
    nrow(closeness_vals_out), 10)

  # Expect node ID values in the first column
  expect_identical(
    closeness_vals_out[, 1],
    as.character(1:10))

  # Expect numerical values in the
  # second column
  expect_is(
    closeness_vals_out[, 2],
    "numeric")

  # Get closness values with `direction = in`
  closeness_vals_in <-
    get_closeness(
      graph,
      direction = "in")

  # Expect a data frame as output
  expect_is(
    closeness_vals_in, "data.frame")

  # Expect 2 columns in the df
  expect_equal(
    ncol(closeness_vals_in), 2)

  # Expect 10 rows in the df
  expect_equal(
    nrow(closeness_vals_in), 10)

  # Expect node ID values in the first column
  expect_identical(
    closeness_vals_in[, 1],
    as.character(1:10))

  # Expect numerical values in the
  # second column
  expect_is(
    closeness_vals_in[, 2],
    "numeric")
})

test_that("Getting authority centrality is possible", {

  set.seed(23)

  # Create a random graph
  graph_1 <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 1)

  graph_2 <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 1) %>%
    select_edges() %>%
    set_edge_attrs_ws(
      edge_attr = "weight",
      value = rnorm(n = 22, mean = 3, sd = 0.5)) %>%
    clear_selection()

  auth_central_vals <-
    get_authority_centrality(graph = graph_1)

  auth_central_vals_weight_1 <-
    get_authority_centrality(graph = graph_2, weights_attr = "weight")

  auth_central_vals_weight_2 <-
    get_authority_centrality(graph = graph_2)

  # Expect a data frame as output for all
  expect_is(
    auth_central_vals, "data.frame")

  expect_is(
    auth_central_vals_weight_1, "data.frame")

  expect_is(
    auth_central_vals_weight_2, "data.frame")

  # Expect 2 columns in the df
  expect_equal(
    ncol(auth_central_vals), 2)

  expect_equal(
    ncol(auth_central_vals_weight_1), 2)

  expect_equal(
    ncol(auth_central_vals_weight_2), 2)

  # Expect 10 rows in the df
  expect_equal(
    nrow(auth_central_vals), 10)

  expect_equal(
    nrow(auth_central_vals_weight_1), 10)

  expect_equal(
    nrow(auth_central_vals_weight_2), 10)

  # Expect node ID values in the first column
  expect_identical(
    auth_central_vals[, 1],
    as.character(1:10))

  expect_identical(
    auth_central_vals_weight_1[, 1],
    as.character(1:10))

  expect_identical(
    auth_central_vals_weight_1[, 1],
    as.character(1:10))

  # Expect numerical values in the
  # second column
  expect_is(
    auth_central_vals[, 2],
    "numeric")

  expect_is(
    auth_central_vals_weight_1[, 2],
    "numeric")

  expect_is(
    auth_central_vals_weight_2[, 2],
    "numeric")
})

test_that("Getting constraint values is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 1)

  # Get constraint values for all
  # nodes in the graph
  constraint_vals <- get_constraint(graph)

  # Expect a data frame as output
  expect_is(
    constraint_vals, "data.frame")

  # Expect 2 columns in the df
  expect_equal(
    ncol(constraint_vals), 2)

  # Expect 10 rows in the df
  expect_equal(
    nrow(constraint_vals), 10)

  # Expect node ID values in the first column
  expect_identical(
    constraint_vals[, 1],
    as.character(1:10))

  # Expect numerical values in the
  # second column
  expect_is(
    constraint_vals[, 2],
    "numeric")

  # Get constraint values for specific nodes
  constraint_vals_selected <-
    get_constraint(
      graph,
      nodes = 1:5)

  # Expect a data frame as output
  expect_is(
    constraint_vals_selected, "data.frame")

  # Expect 2 columns in the df
  expect_equal(
    ncol(constraint_vals_selected), 2)

  # Expect 5 rows in the df
  expect_equal(
    nrow(constraint_vals_selected), 5)

  # Expect node ID values in the first column
  expect_identical(
    constraint_vals_selected[, 1],
    as.character(1:5))

  # Expect numerical values in the
  # second column
  expect_is(
    constraint_vals_selected[, 2],
    "numeric")

  # Expect an error if supplying nodes that don't exist
  expect_error(
    get_constraint(
      graph,
      nodes = 20))
})

test_that("Getting articulation points is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 30, m = 50,
      set_seed = 1)

  # Get articulation points for the graph
  articulation_points <-
    get_articulation_points(graph)

  # Expect an integer vector as output
  expect_is(
    articulation_points, "integer")

  # Expect 4 values in the vector
  expect_equal(
    length(articulation_points), 4)
})

test_that("Getting weakly connected components is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 30, m = 50,
      set_seed = 1)

  # Get connected components for the graph
  connected_components <-
    get_w_connected_cmpts(graph)

  # Expect a data frame as output
  expect_is(
    connected_components, "data.frame")

  # Expect 2 columns in the df
  expect_equal(
    ncol(connected_components), 2)

  # Expect 30 rows in the df
  expect_equal(
    nrow(connected_components), 30)

  # Expect numerical values in the
  # second column
  expect_is(
    connected_components[, 2],
    "numeric")
})

test_that("Getting strongly connected components is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 5, m = 10,
      set_seed = 1,
      directed = TRUE)

  # Get connected components for the graph
  s_connected_components <-
    get_s_connected_cmpts(graph)

  # Expect a data frame as output
  expect_is(
    s_connected_components, "data.frame")

  # Expect 2 columns in the df
  expect_equal(
    ncol(s_connected_components), 2)

  # Expect 5 rows in the df
  expect_equal(
    nrow(s_connected_components), 5)

  # Expect numerical values in the
  # second column
  expect_is(
    s_connected_components[, 2],
    "numeric")
})
