context("Get graph metrics")

test_that("Getting a degree histogram is possible", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
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
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
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
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 22,
      set_seed = 23)

  max_eccen <- get_max_eccentricity(graph)

  # Expect that `max_eccen` is a numeric vector
  expect_is(
    max_eccen, "numeric")

  # Expect that `max_eccen` is of length 1
  expect_equal(
    length(max_eccen), 1)

  # Expect that `max_eccen` has the value 5
  expect_equal(
    max_eccen, 5)

  # Expect NA if there aren't any nodes
  # in the graph
  expect_equal(
    get_max_eccentricity(
      graph = create_graph()),
    as.numeric(NA))
})

test_that("Getting minimum eccentricity is possible", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 23,
      set_seed = 23)

  min_eccen <-
    get_min_eccentricity(
      graph = graph,
      direction = "all")

  # Expect that `min_eccen` is a numeric vector
  expect_is(
    min_eccen, "numeric")

  # Expect that `min_eccen` is of length 1
  expect_equal(
    length(min_eccen), 1)

  # Expect that `min_eccen` has the value 2
  expect_equal(
    min_eccen, 2)

  # Expect certain values with different
  # values provided for `direction`
  expect_equal(
    get_min_eccentricity(
      graph = create_graph() %>% add_cycle(n = 6),
      direction = "out"),
    5)

  expect_equal(
    get_min_eccentricity(
      graph = create_graph() %>% add_cycle(n = 6),
      direction = "in"),
    5)

  # Expect an error if `direction` is not valid
  expect_error(
    get_min_eccentricity(
      graph = graph,
      direction = "away"))

  # Expect NA if there aren't any nodes
  # in the graph
  expect_equal(
    get_min_eccentricity(
      graph = create_graph()),
    NA_real_
  )
})

test_that("Getting graph eccentricity is possible", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 23,
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
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 23,
      set_seed = 23)

  graph_periphery <- get_periphery(graph)

  # Expect that `graph_periphery` is an integer vector
  expect_is(
    graph_periphery, "integer")

  # Expect that `graph_periphery` has length 3 in
  # this case
  expect_equal(
    length(graph_periphery), 3)

  # Expect certain values for the
  # `graph_periphery` object
  expect_equal(
    graph_periphery, c(3, 7, 10))
})

test_that("Getting graph info is possible", {

  # Create a random graph
  graph <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 23,
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

  # Use the `get_graph_info()` function to create a
  # data frame with graph metrics
  graph_i <-
    graph %>%
    get_graph_info()

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
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 25,
      set_seed = 23)

  # Test that the graph is indeed connected
  expect_true(
    is_graph_connected(graph_connected))

  graph_not_connected <-
    create_graph() %>%
    add_gnm_graph(
      n = 10,
      m = 8,
      set_seed = 23)

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
    get_adhesion(graph_empty), NA_real_)
})

test_that("Getting graph girth is possible", {

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

  # Expect specific girth values for
  # the two non-empty graphs
  expect_equal(
    get_girth(graph_cycle), 5)

  expect_equal(
    get_girth(graph_full), 3)

  # Expect NA for the empty graph
  expect_equal(
    get_girth(graph_empty), NA_real_)
})

test_that("Getting the mean distance for a graph is possible", {

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

  # Expect specific mean distance values for
  # the two non-empty graphs
  expect_equal(
    get_mean_distance(graph_cycle), 2.5)

  expect_equal(
    get_mean_distance(graph_full), 1)

  # Expect NA for the empty graph
  expect_equal(
    get_mean_distance(graph_empty), NA_real_)
})

test_that("Getting the reciprocity for a graph is possible", {

  # Define a graph where 2 edge definitions
  # have pairs of reciprocal edges
  graph <-
   create_graph() %>%
   add_cycle(n = 3) %>%
   add_node(
     from = 1,
       to = 1) %>%
   add_node(
     from = 1,
       to = 1)

  # Expect that the reciprocity
  # will be the ratio of reciprocating
  # edges (4) to the total number
  # of graph edges (7)
  expect_equal(
    get_reciprocity(graph), 4/7)

  # Expect that a graph with no
  # edges will return NA
  expect_true(
    is.na(get_reciprocity(
      create_graph() %>%
        add_n_nodes(n = 5))))
})

test_that("Getting the minimum cut between nodes is possible", {

  # Set a seed
  suppressWarnings(RNGversion("3.5.0"))
  set.seed(23)

  # Create a cycle graph
  graph_cycle <-
    create_graph() %>%
    add_cycle(n = 5)

  # Create a cycle graph with a
  # `capacity` edge attribute
  graph_capacity <-
    create_graph() %>%
    add_cycle(n = 5) %>%
    select_edges() %>%
    set_edge_attrs_ws(
      edge_attr = "capacity",
      value =
        rnorm(
          n = count_edges(graph = .),
          mean = 5,
          sd = 1)) %>%
    clear_selection()

  # Create an empty graph
  graph_empty <-
    create_graph()

  # Expect specific minimum cut values
  # with different pairs of nodes from
  # the two non-empty graphs
  expect_equal(
    get_min_cut_between(
      graph = graph_cycle,
      from = 1,
      to = 2),
    1)

  expect_equal(
    get_min_cut_between(
      graph = graph_capacity,
      from = 1,
      to = 2),
    tolerance = 0.02,
    expected = 4.479822)

  # Expect NA for the empty graph
  expect_equal(
    get_min_cut_between(graph_empty), NA_real_)
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
    count_automorphisms(graph_empty), NA_real_)
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
    count_asymmetric_node_pairs(graph_empty), NA_real_)
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
    count_mutual_node_pairs(graph_empty), NA_real_)
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
    count_unconnected_node_pairs(graph_empty), NA_real_)
})
