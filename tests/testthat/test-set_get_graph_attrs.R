context("Setting graph/global attributes")

test_that("Setting a graph name can be done", {

  # Create an empty graph
  graph <- create_graph()

  # Set a graph name
  graph_name <-
    graph %>%
    set_graph_name(name = "test_that_name")

  # Expect that the name was set
  expect_true(
    graph_name$graph_info$graph_name == "test_that_name")

  # Add 2 nodes and an edge and then
  # select a node
  graph_1 <-
    graph %>%
    add_node() %>%
    add_node() %>%
    add_edge(
      from = 1,
      to = 2) %>%
    select_nodes_by_id(nodes = 1)

  # Set a graph name
  graph_name_1 <-
    set_graph_name(
      graph = graph_1,
      name = "test_that_name_again")

  # Expect that the name was set
  expect_true(
    graph_name_1$graph_info$graph_name == "test_that_name_again")
})

test_that("Setting a time for the graph can be done", {

  # Create an empty graph
  graph <- create_graph()

  # Provide the new graph with a timestamp (`tz` not supplied so
  # `GMT` is used as the time zone)
  graph_1 <-
    set_graph_time(
      graph = graph,
      time = "2015-10-25 15:23:00")

  # Expect that the time value was passed in properly
  expect_true(
    graph_1$graph_info$graph_time == "2015-10-25 15:23:00")

  # Expect that the time zone is set to `GMT`
  expect_true(
    graph_1$graph_info$graph_tz == "GMT")

  # Update tz when a timestamp is already present
  graph_2 <-
    set_graph_time(
      graph = graph_1,
      tz = "America/Los_Angeles")

  # Expect that the time zone has been changed
  expect_true(
    graph_2$graph_info$graph_tz == "America/Los_Angeles")

  # Expect an error when setting a time zone that is
  # not in `OlsonNames()`
  expect_error(
    set_graph_time(
      graph = graph_2,
      tz = "Moon/Moon"))

  # Create a graph with a node and a node selection
  graph_selection <-
    create_graph() %>%
    add_node() %>%
    select_nodes() %>%
    set_graph_time("2015-10-25 15:23:00")
})

test_that("Getting the graph name is possible", {

  # Create a new graph and set a graph name
  graph <-
    create_graph() %>%
    set_graph_name(name = "test_graph")

  # Verify that the graph name returned is a
  # character vector
  expect_is(
    get_graph_name(graph), "character")

  # Expect that the returned vector has a length of 1
  expect_equal(
    length(get_graph_name(graph)), 1)

  # Expect that the graph name that was set is returned
  expect_equal(
    get_graph_name(graph), "test_graph")
})

test_that("Getting the graph time is possible", {

  # Create a graph with a time
  graph <-
    create_graph() %>%
    set_graph_time(
      time = "2015-10-25 15:23:00")

  # Expect a graph time as POSIXct
  expect_is(
    get_graph_time(graph), "POSIXct")

  # Expect that the returned vector has a length of 1
  expect_equal(
    length(get_graph_time(graph)), 1)

  # Create a graph without a time set
  graph <- create_graph()

  # Verify that a POSIXct time is returned
  expect_is(
    get_graph_time(graph), "POSIXct")
})

test_that("Setting global graph attrs is possible", {

  # Create an empty graph with no global graph
  # parameters
  graph <- create_graph(attr_theme = FALSE)

  # Set 3 global graph attrs
  graph <-
    graph %>%
    set_global_graph_attrs(
      attr = c("overlap", "color", "penwidth"),
      value = c("true", "red", "5"),
      attr_type = c("graph", "node", "edge"))

  # Expect 3 rows and three columns in the
  # `global_attrs` data frame
  expect_equal(
    nrow(graph$global_attrs), 3)

  expect_equal(
    ncol(graph$global_attrs), 3)

  # Expect certain column names to be present
  # in the `global_attrs` data frame
  expect_equal(
    colnames(graph$global_attrs),
    c("attr", "value", "attr_type"))

  # Expect certain values to be present
  # in the `attr` column
  expect_equal(
    graph$global_attrs$attr,
    c("overlap", "color", "penwidth"))

  # Expect certain values to be present
  # in the `value` column
  expect_equal(
    graph$global_attrs$value,
    c("true", "red", "5"))

  # Expect certain values to be present
  # in the `attr_type` column
  expect_equal(
    graph$global_attrs$attr_type,
    c("graph", "node", "edge"))

  # Create another empty graph with no
  # global graph parameters
  graph <- create_graph(attr_theme = FALSE)

  # Set a single global graph attr
  graph <-
    graph %>%
    set_global_graph_attrs(
      attr = "overlap",
      value = TRUE,
      attr_type = "graph")

  # Expect 1 row and three columns in the
  # `global_attrs` data frame
  expect_equal(
    nrow(graph$global_attrs), 1)

  expect_equal(
    ncol(graph$global_attrs), 3)

  # Expect that the `TRUE` logical value
  # was coerced to character `true`
  expect_equal(
    graph$global_attrs$value, "true")

  # Expect an error if setting global graph
  # attributes with unequal vector lengths
  expect_error(
    graph %>%
      set_global_graph_attrs(
        attr = c("overlap", "color"),
        value = c("true", "red", "5"),
        attr_type = c("graph", "node", "edge")))
})

test_that("Getting global graph attrs is possible", {

  # Create an empty graph with no global graph
  # parameters
  graph <- create_graph(attr_theme = FALSE)

  # Expect an NA value if getting global graph
  # attributes where there are none set
  expect_equal(
    graph %>%
      get_global_graph_attr_info() %>%
      nrow(), 0)

  # Set 3 global graph attrs
  graph <-
    graph %>%
    set_global_graph_attrs(
      attr = c("overlap", "color", "penwidth"),
      value = c("true", "red", "5"),
      attr_type = c("graph", "node", "edge"))

  # Get a table with the attributes
  # using `get_global_graph_attrs()`
  global_graph_attrs <-
    graph %>%
    get_global_graph_attr_info()

  # Expect that the returned table is
  # equivalent to the table stored in
  # the graph object
  expect_equal(
    graph$global_attrs %>% as_tibble(),
    global_graph_attrs)
})

test_that("Adding global graph attrs is possible", {

  # Create an empty graph with the default global
  # graph attributes
  graph <- create_graph()

  # Add 2 global graph attrs
  graph_add_2 <-
    graph %>%
    add_global_graph_attrs(
      attr = c("overlap", "penwidth"),
      value = c("true", "5"),
      attr_type = c("graph", "edge"))

  # Expect that the new graph object has 2 more
  # global graph attributes than the original
  expect_equal(
    nrow(graph_add_2$global_attrs) -
      nrow(graph$global_attrs), 2)

  # Expect that the new graph attributes are
  # the last 2 rows in the `global_attrs` df
  expect_equal(
    tail(graph_add_2$global_attrs, 2)[, 1],
    c("overlap", "penwidth"))

  expect_equal(
    tail(graph_add_2$global_attrs, 2)[, 2],
    c("true", "5"))

  expect_equal(
    tail(graph_add_2$global_attrs, 2)[, 3],
    c("graph", "edge"))

  # Add 1 global graph attribute
  graph_add_1 <-
    graph %>%
    add_global_graph_attrs(
      attr = "overlap",
      value = TRUE,
      attr_type = "graph")

  # Expect that the new graph object has 1 more
  # global graph attribute than the original
  expect_equal(
    nrow(graph_add_1$global_attrs) -
      nrow(graph$global_attrs), 1)

  # Expect that the new graph attribute is
  # in the last row in the `global_attrs` df
  expect_equal(
    tail(graph_add_1$global_attrs, 1)[, 1],
    "overlap")

  expect_equal(
    tail(graph_add_1$global_attrs, 1)[, 2],
    "true")

  expect_equal(
    tail(graph_add_1$global_attrs, 1)[, 3],
    "graph")
})

test_that("Deleting global graph attrs is possible", {

  # Create an empty graph with the default global
  # graph attributes
  graph <- create_graph()

  # Remove a single global graph attr
  graph_del_1 <-
    graph %>%
    delete_global_graph_attrs(
      attr = "layout",
      attr_type = "graph")

  # Expect that the new graph object has 1 less
  # global graph attribute than the original
  expect_equal(
    nrow(graph$global_attrs) -
      nrow(graph_del_1$global_attrs), 1)

  # Remove 2 global graph attributes
  graph_del_2 <-
    graph %>%
    delete_global_graph_attrs(
      attr = c("layout", "outputorder"),
      attr_type = c("graph", "graph"))

  # Expect that the new graph object has 2 less
  # global graph attributes than the original
  expect_equal(
    nrow(graph$global_attrs) -
      nrow(graph_del_2$global_attrs), 2)

  # Expect an error if deleting with an invalid
  # `attr_type` (using `nodes` instead of `node`)
  expect_error(
    graph %>%
      delete_global_graph_attrs(
        attr = "layout",
        attr_type = "nodes"))
})
