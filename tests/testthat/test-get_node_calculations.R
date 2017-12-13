context("Get node calculations")

test_that("Getting betweenness is possible", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 10,
      set_seed = 23)

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
    as.integer(1:10))

  # Expect numerical values in the
  # second column
  expect_is(
    betweenness_vals[, 2],
    "numeric")
})

test_that("Getting bridging is possible", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 10,
      set_seed = 23)

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
    as.integer(1:10))

  # Expect numerical values in the
  # second column
  expect_is(
    bridging_vals[, 2],
    "numeric")
})

test_that("Getting closeness is possible", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 10,
      set_seed = 23)

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
    as.integer(1:10))

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
    as.integer(1:10))

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
    as.integer(1:10))

  # Expect numerical values in the
  # second column
  expect_is(
    closeness_vals_in[, 2],
    "numeric")

  # Expect an error if value for `direction`
  # is not any of `all`, `in`, or `out`
  expect_error(
    get_closeness(
      graph = graph,
      direction = "away"))
})

test_that("Getting coreness values is possible", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 10,
      set_seed = 23)

  # Get coreness values in all directions
  coreness_vals_all <- get_coreness(graph)

  # Expect a data frame as output
  expect_is(
    coreness_vals_all, "data.frame")

  # Expect 2 columns in the df
  expect_equal(
    ncol(coreness_vals_all), 2)

  # Expect 10 rows in the df
  expect_equal(
    nrow(coreness_vals_all), 10)

  # Expect node ID values in the first column
  expect_identical(
    coreness_vals_all[, 1],
    as.integer(1:10))

  # Expect numeric values in the second column
  expect_is(
    coreness_vals_all[, 2],
    "numeric")

  # Get coreness values in the `in` direction
  coreness_vals_in <-
    get_coreness(
      graph = graph,
      direction = "in")

  # Expect a data frame as output
  expect_is(
    coreness_vals_in, "data.frame")

  # Expect 2 columns in the df
  expect_equal(
    ncol(coreness_vals_in), 2)

  # Expect 10 rows in the df
  expect_equal(
    nrow(coreness_vals_in), 10)

  # Expect node ID values in the first column
  expect_identical(
    coreness_vals_in[, 1],
    as.integer(1:10))

  # Expect numeric values in the second column
  expect_is(
    coreness_vals_in[, 2],
    "numeric")

  # Get coreness values in the `out` direction
  coreness_vals_out <-
    get_coreness(
      graph = graph,
      direction = "out")

  # Expect a data frame as output
  expect_is(
    coreness_vals_out, "data.frame")

  # Expect 2 columns in the df
  expect_equal(
    ncol(coreness_vals_out), 2)

  # Expect 10 rows in the df
  expect_equal(
    nrow(coreness_vals_out), 10)

  # Expect node ID values in the first column
  expect_identical(
    coreness_vals_out[, 1],
    as.integer(1:10))

  # Expect numeric values in the second column
  expect_is(
    coreness_vals_out[, 2],
    "numeric")

  # Expect an error if value for `direction`
  # is not any of `all`, `in`, or `out`
  expect_error(
    get_coreness(
      graph = graph,
      direction = "away"))
})

test_that("Getting closeness vitality is possible", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 10,
      set_seed = 23)

  # Get closness vitality values
  closeness_vitality_vals <- get_closeness_vitality(graph)

  # Expect a data frame as output
  expect_is(
    closeness_vitality_vals, "data.frame")

  # Expect 2 columns in the df
  expect_equal(
    ncol(closeness_vitality_vals), 2)

  # Expect 10 rows in the df
  expect_equal(
    nrow(closeness_vitality_vals), 10)

  # Expect node ID values in the first column
  expect_identical(
    closeness_vitality_vals[, 1],
    as.integer(1:10))

  # Expect numeric values in the second column
  expect_is(
    closeness_vitality_vals[, 2],
    "numeric")
})

test_that("Getting alpha centrality is possible", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 15,
      m = 15,
      set_seed = 23)

  alpha_central_vals <-
    get_alpha_centrality(graph)

  # Expect a data frame as output
  expect_is(
    alpha_central_vals, "data.frame")

  # Expect 2 columns in the df
  expect_equal(
    ncol(alpha_central_vals), 2)

  # Expect 10 rows in the df
  expect_equal(
    nrow(alpha_central_vals), 15)

  # Expect node ID values in the first column
  expect_identical(
    alpha_central_vals[,1],
    as.integer(1:15))

  # Expect numerical values in the
  # second column
  expect_is(
    alpha_central_vals[, 2],
    "numeric")
})

test_that("Getting leverage centrality is possible", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 10,
      set_seed = 23)

  leverage_central_vals <-
    get_leverage_centrality(graph)

  # Expect a data frame as output
  expect_is(
    leverage_central_vals, "data.frame")

  # Expect 2 columns in the df
  expect_equal(
    ncol(leverage_central_vals), 2)

  # Expect 10 rows in the df
  expect_equal(
    nrow(leverage_central_vals), 10)

  # Expect node ID values in the first column
  expect_identical(
    leverage_central_vals[,1],
    as.integer(1:10))

  # Expect numerical values in the
  # second column
  expect_is(
    leverage_central_vals[, 2],
    "numeric")
})

test_that("Getting authority centrality is possible", {

  # Create a random graph
  graph_1 <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      set_seed = 23)

  # Create a random graph
  graph_2 <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      set_seed = 23) %>%
    select_edges() %>%
    set_edge_attrs_ws(
      edge_attr = "weight",
      value = rnorm(n = 22, mean = 3, sd = 0.5)) %>%
    clear_selection()

  auth_central_vals <-
    get_authority_centrality(
      graph = graph_1)

  auth_central_vals_weight_1 <-
    get_authority_centrality(
      graph = graph_2,
      weights_attr = "weight")

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
    as.integer(1:10))

  expect_identical(
    auth_central_vals_weight_1[, 1],
    as.integer(1:10))

  expect_identical(
    auth_central_vals_weight_1[, 1],
    as.integer(1:10))

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

test_that("Getting eigenvector centrality is possible", {

  set.seed(23)

  # Create a random graph
  graph_1 <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      set_seed = 23)

  # Create a random graph
  graph_2 <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      set_seed = 23) %>%
    select_edges() %>%
    set_edge_attrs_ws(
      edge_attr = "weight",
      value = rnorm(n = 22, mean = 3, sd = 0.5)) %>%
    clear_selection()

  eigen_central_vals <-
    get_eigen_centrality(
      graph = graph_1)

  eigen_central_vals_weight_1 <-
    get_eigen_centrality(
      graph = graph_2,
      weights_attr = "weight")

  eigen_central_vals_weight_2 <-
    get_eigen_centrality(graph = graph_2)

  # Expect a data frame as output for all
  expect_is(
    eigen_central_vals, "data.frame")

  expect_is(
    eigen_central_vals_weight_1, "data.frame")

  expect_is(
    eigen_central_vals_weight_2, "data.frame")

  # Expect 2 columns in the df
  expect_equal(
    ncol(eigen_central_vals), 2)

  expect_equal(
    ncol(eigen_central_vals_weight_1), 2)

  expect_equal(
    ncol(eigen_central_vals_weight_2), 2)

  # Expect 10 rows in the df
  expect_equal(
    nrow(eigen_central_vals), 10)

  expect_equal(
    nrow(eigen_central_vals_weight_1), 10)

  expect_equal(
    nrow(eigen_central_vals_weight_2), 10)

  # Expect node ID values in the first column
  expect_identical(
    eigen_central_vals[, 1],
    as.integer(1:10))

  expect_identical(
    eigen_central_vals_weight_1[, 1],
    as.integer(1:10))

  expect_identical(
    eigen_central_vals_weight_1[, 1],
    as.integer(1:10))

  # Expect numerical values in the
  # second column
  expect_is(
    eigen_central_vals[, 2],
    "numeric")

  expect_is(
    eigen_central_vals_weight_1[, 2],
    "numeric")

  expect_is(
    eigen_central_vals_weight_2[, 2],
    "numeric")
})

test_that("Getting constraint values is possible", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      set_seed = 23)

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
    as.integer(1:10))

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
    as.integer(1:5))

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

test_that("Getting radiality values is possible", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      set_seed = 23)

  # Get constraint values for all
  # nodes in the graph
  radiality_vals <- get_radiality(graph)

  # Expect a data frame as output
  expect_is(
    radiality_vals, "data.frame")

  # Expect 2 columns in the df
  expect_equal(
    ncol(radiality_vals), 2)

  # Expect 10 rows in the df
  expect_equal(
    nrow(radiality_vals), 10)

  # Expect node ID values in the first column
  expect_identical(
    radiality_vals[, 1],
    as.integer(1:10))

  # Expect numerical values in the
  # second column
  expect_is(
    radiality_vals[, 2],
    "numeric")

  # Expect certain radiality values depending
  # on the `direction` parameter
  expect_gte(
    get_radiality(
      graph = graph,
      direction = "all")[, 2] %>% min(),
    2.1111)

  expect_lte(
    get_radiality(
      graph = graph,
      direction = "all")[, 2] %>% max(),
    3.1111)

  expect_gte(
    get_radiality(
      graph = graph,
      direction = "in")[, 2] %>% min(),
    3.4444)

  expect_lte(
    get_radiality(
      graph = graph,
      direction = "in")[, 2] %>% max(),
    4.6667)

  expect_gte(
    get_radiality(
      graph = graph,
      direction = "out")[, 2] %>% min(),
    0.6667)

  expect_lte(
    get_radiality(
      graph = graph,
      direction = "out")[, 2] %>% max(),
    5.2222)

  # Expect an error if using a `direction`
  # value that isn't valid
  expect_error(
    get_radiality(
      graph = graph,
      direction = "away"))
})

test_that("Getting PageRank values is possible", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      set_seed = 23)

  # Get constraint values for all
  # nodes in the graph
  pagerank_vals <- get_pagerank(graph)

  # Expect a data frame as output
  expect_is(
    pagerank_vals, "data.frame")

  # Expect 2 columns in the df
  expect_equal(
    ncol(pagerank_vals), 2)

  # Expect 10 rows in the df
  expect_equal(
    nrow(pagerank_vals), 10)

  # Expect node ID values in the first column
  expect_identical(
    pagerank_vals[, 1],
    as.integer(1:10))

  # Expect numerical values in the
  # second column
  expect_is(
    pagerank_vals[, 2],
    "numeric")
})

test_that("Getting articulation points is possible", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      set_seed = 23)

  # Get articulation points for the graph
  articulation_points <-
    get_articulation_points(graph)

  # Expect an integer vector as output
  expect_is(
    articulation_points, "integer")

  # Expect 4 values in the vector
  expect_equal(
    length(articulation_points), 2)
})

test_that("Getting weakly connected components is possible", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      set_seed = 23)

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
    nrow(connected_components), 10)

  # Expect numerical values in the
  # second column
  expect_is(
    connected_components[, 2],
    "numeric")
})

test_that("Getting strongly connected components is possible", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 5,
      m = 10,
      set_seed = 23)

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
