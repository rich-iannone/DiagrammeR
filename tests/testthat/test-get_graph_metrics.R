context("Get graph metrics")

test_that("Getting a degree histogram is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 23)

  # Get the degree histogram for total degree
  degree_hist_all <-
    get_degree_histogram(
      graph = graph,
      mode = "all")

  # Expect that `degree_hist_all` inherits from `data.frame`
  expect_is(
    degree_hist_all, "data.frame")

  # Expect certain column names for the `degree_hist_all` object
  expect_identical(
    colnames(degree_hist_all),
    c("degree", "total_degree_hist"))

  # Get the degree histogram for in-degree
  degree_hist_in <-
    get_degree_histogram(
      graph = graph,
      mode = "in")

  # Expect that `degree_hist_in` inherits from `data.frame`
  expect_is(
    degree_hist_in, "data.frame")

  # Expect certain column names for the `degree_hist_in` object
  expect_identical(
    colnames(degree_hist_in),
    c("degree", "indegree_hist"))

  # Get the degree histogram for out-degree
  degree_hist_out <-
    get_degree_histogram(
      graph = graph,
      mode = "out")

  # Expect that `degree_hist_out` inherits from `data.frame`
  expect_is(
    degree_hist_out, "data.frame")

  # Expect certain column names for the `degree_hist_out` object
  expect_identical(
    colnames(degree_hist_out),
    c("degree", "outdegree_hist"))
})

test_that("Getting degree distribution is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 23)

  # Get the degree distribution for total degree
  degree_dist_all <-
    get_degree_distribution(
      graph = graph,
      mode = "all")

  # Expect that `degree_dist_all` inherits from `data.frame`
  expect_is(
    degree_dist_all, "data.frame")

  # Expect certain column names for the `degree_dist_all` object
  expect_identical(
    colnames(degree_dist_all),
    c("degree", "total_degree_dist"))

  # Get the degree distribution for in-degree
  degree_dist_in <-
    get_degree_distribution(
      graph = graph,
      mode = "in")

  # Expect that `degree_dist_in` inherits from `data.frame`
  expect_is(
    degree_dist_in, "data.frame")

  # Expect certain column names for the `degree_dist_in` object
  expect_identical(
    colnames(degree_dist_in),
    c("degree", "indegree_dist"))

  # Get the degree distribution for out-degree
  degree_dist_out <-
    get_degree_distribution(
      graph = graph,
      mode = "out")

  # Expect that `degree_dist_out` inherits from `data.frame`
  expect_is(
    degree_dist_out, "data.frame")

  # Expect certain column names for the `degree_dist_out` object
  expect_identical(
    colnames(degree_dist_out),
    c("degree", "outdegree_dist"))
})

test_that("Getting maximum eccentricity is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 23)

  max_eccen <- get_max_eccentricity(graph)

  # Expect that `max_eccen` is a numeric vector
  expect_is(
    max_eccen, "numeric")

  # Expect that `max_eccen` is of length 1
  expect_equal(
    length(max_eccen), 1)

  # Expect that `max_eccen` has the value 4
  expect_equal(
    max_eccen, 3)
})

test_that("Getting minimum eccentricity is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 23)

  min_eccen <- get_min_eccentricity(graph)

  # Expect that `min_eccen` is a numeric vector
  expect_is(
    min_eccen, "numeric")

  # Expect that `min_eccen` is of length 1
  expect_equal(
    length(min_eccen), 1)

  # Expect that `min_eccen` has the value 4
  expect_equal(
    min_eccen, 2)
})

test_that("Getting graph eccentricity is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 23)

  graph_eccen <- get_eccentricity(graph)

  # Expect that `graph_eccen` is a data frame
  expect_is(
    graph_eccen, "data.frame")

  # Expect that `graph_eccen` has 10 rows
  # this case
  expect_equal(
    nrow(graph_eccen), 10)

  # Expect certain column names for the `graph_eccen` object
  expect_equal(
    colnames(graph_eccen), c("id", "eccentricity"))

  # Expect that `eccentricity` column is numeric
  expect_is(
    graph_eccen$eccentricity, "numeric")

  # Expect that `id` column has integer values
  expect_is(
    graph_eccen$id, "integer")
})

test_that("Getting graph periphery is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 23)

  graph_periphery <- get_periphery(graph)

  # Expect that `graph_periphery` is an integer vector
  expect_is(
    graph_periphery, "integer")

  # Expect that `graph_periphery` has length 3 in
  # this case
  expect_equal(
    length(graph_periphery), 2)

  # Expect certain values for the
  # `graph_periphery` object
  expect_equal(
    graph_periphery, c(3, 5))
})

test_that("Getting graph info is possible", {

  # Create a random graph
  graph <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 23)

  # Add a graph name
  graph <-
    set_graph_name(
      graph = graph,
      name = "random")

  # Add a graph time
  graph <-
    set_graph_time(
      graph = graph,
      time = "2015-10-25 15:23:00")

  # Use the `graph_info()` function to create a
  # data frame with graph metrics
  graph_i <- graph_info(graph)

  # Expect that `graph_i` is a data frame
  expect_is(
    graph_i, "data.frame")

  # Expect that `graph_i` has 9 columns
  expect_equal(
    ncol(graph_i), 9)

  # Expect that `graph_i` has 1 row
  expect_equal(
    nrow(graph_i), 1)

  # Expect that `graph_i` has certain column names
  expect_identical(
    colnames(graph_i),
    c("name", "n", "e", "dens", "mn_deg",
      "mx_deg", "avg_deg", "time", "tz"))
})

test_that("Checking whether the graph is connected is possible", {

  # Create a random graph
  graph_connected <-
    create_random_graph(
      n = 10, m = 22,
      set_seed = 23)

  # Test that the graph is indeed connected
  expect_true(
    is_graph_connected(graph_connected))

  graph_not_connected <-
    create_random_graph(
      n = 10, m = 8,
      set_seed = 1)

  # Test that the graph is indeed connected
  expect_false(
    is_graph_connected(graph_not_connected))
})

test_that("Getting graph adhesion is possible", {

  # Create a cycle graph
  graph_cycle <-
    create_graph() %>%
    add_cycle(n = 5)

  # Create a full graph
  graph_full <-
    create_graph() %>%
    add_full_graph(n = 8)

  # Create an empty graph
  graph_empty <-
    create_graph()

  # Expect specific adhesion values for
  # the two non-empty graphs
  expect_equal(
    get_adhesion(graph_cycle), 1)

  expect_equal(
    get_adhesion(graph_full), 7)

  # Expect NA for the empty graph
  expect_equal(
    get_adhesion(graph_empty), as.numeric(NA))
})

test_that("Getting a count of graph automorphisms is possible", {

  # Create a cycle graph
  graph_cycle <-
    create_graph() %>%
    add_cycle(n = 5)

  # Create a full graph
  graph_full <-
    create_graph() %>%
    add_full_graph(n = 8)

  # Create an empty graph
  graph_empty <-
    create_graph()

  # Expect specific automorphism counts
  # for the two non-empty graphs
  expect_equal(
    count_automorphisms(graph_cycle), 10)

  expect_equal(
    count_automorphisms(graph_full), 40320)

  # Expect NA for the empty graph
  expect_equal(
    count_automorphisms(graph_empty), as.numeric(NA))
})

test_that("Getting a count of asymmetric node pairs is possible", {

  # Create a cycle graph
  graph_cycle <-
    create_graph() %>%
    add_cycle(n = 5)

  # Create a full graph
  graph_full <-
    create_graph() %>%
    add_full_graph(n = 8)

  # Create an empty graph
  graph_empty <-
    create_graph()

  # Expect specific automorphism counts
  # for the two non-empty graphs
  expect_equal(
    count_asymmetric_node_pairs(graph_cycle), 5)

  expect_equal(
    count_asymmetric_node_pairs(graph_full), 0)

  # Expect NA for the empty graph
  expect_equal(
    count_asymmetric_node_pairs(graph_empty), as.numeric(NA))
})

test_that("Getting a count of mutual node pairs is possible", {

  # Create a cycle graph
  graph_cycle <-
    create_graph() %>%
    add_cycle(n = 5)

  # Create a full graph
  graph_full <-
    create_graph() %>%
    add_full_graph(n = 8)

  # Create an empty graph
  graph_empty <-
    create_graph()

  # Expect specific automorphism counts
  # for the two non-empty graphs
  expect_equal(
    count_mutual_node_pairs(graph_cycle), 0)

  expect_equal(
    count_mutual_node_pairs(graph_full), 28)

  # Expect NA for the empty graph
  expect_equal(
    count_mutual_node_pairs(graph_empty), as.numeric(NA))
})

test_that("Getting a count of unconnected node pairs is possible", {

  # Create a cycle graph
  graph_cycle <-
    create_graph() %>%
    add_cycle(n = 5)

  # Create a full graph
  graph_full <-
    create_graph() %>%
    add_full_graph(n = 8)

  # Create an empty graph
  graph_empty <-
    create_graph()

  # Expect specific automorphism counts
  # for the two non-empty graphs
  expect_equal(
    count_unconnected_node_pairs(graph_cycle), 5)

  expect_equal(
    count_unconnected_node_pairs(graph_full), 0)

  # Expect NA for the empty graph
  expect_equal(
    count_unconnected_node_pairs(graph_empty), as.numeric(NA))
})
