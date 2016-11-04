context("Setting graph/global attributes")

test_that("Setting a graph name can be done", {

  # Create an empty graph
  graph <- create_graph()

  # Set a graph name
  graph_name <-
    graph %>%
    set_graph_name("test_that_name")

  # Expect that the name was set
  expect_true(graph_name$graph_name == "test_that_name")

  # Add 2 nodes and an edge and then
  # select a node
  graph_1 <-
    graph %>%
    add_node %>%
    add_node %>%
    add_edge(1, 2) %>%
    select_nodes_by_id(1)

  # Set a graph name
  graph_name_1 <-
    set_graph_name(graph_1, "test_that_name_again")

  # Expect that the name was set
  expect_true(
    graph_name_1$graph_name == "test_that_name_again")
})

test_that("Setting a time for the graph can be done", {

  # Create an empty graph
  graph <- create_graph()

  # Provide the new graph with a timestamp (`tz` not supplied so
  # `GMT` is used as the time zone)
  graph_1 <-
    set_graph_time(
      graph, time = "2015-10-25 15:23:00")

  # Expect that the time value was passed in properly
  expect_true(graph_1$graph_time == "2015-10-25 15:23:00")

  # Expect that the time zone is set to `GMT`
  expect_true(graph_1$graph_tz == "GMT")

  # Update tz when a timestamp is already present
  graph_2 <-
    set_graph_time(
      graph_1, tz = "America/Los_Angeles")

  # Expect that the time zone has been changed
  expect_true(graph_2$graph_tz == "America/Los_Angeles")

  # Expect an error when setting a time zone that is
  # not in `OlsonNames()`
  expect_error(
    set_graph_time(
      graph_2, tz = "Moon/Moon"))

  # Create a graph with a node and a node selection
  graph_selection <-
    create_graph() %>%
    add_node %>%
    select_nodes %>%
    set_graph_time("2015-10-25 15:23:00")

  # Expect that the selection is retained after setting
  # date-time and time zone
  expect_true(!is.null(graph_selection$selection$nodes))
})

test_that("Getting the graph name is possible", {

  # Create a new graph and set a graph name
  graph <-
    create_graph() %>%
    set_graph_name("test_graph")

  # Verify that the graph name returned is a
  # character vector
  expect_is(get_graph_name(graph), "character")

  # Expect that the returned vector has a length of 1
  expect_equal(length(get_graph_name(graph)), 1)

  # Expect that the graph name that was set is returned
  expect_equal(get_graph_name(graph), "test_graph")

  # Create a new graph and don't set a name
  graph <- create_graph()

  # Verify that when there is no name set for the
  # graph, a logical vector is returned
  expect_is(get_graph_name(graph), "logical")

  # Expect that an NA is returned
  expect_true(is.na(get_graph_name(graph)))
})

test_that("Getting the graph time is possible", {

  # Create a graph with a time
  graph <-
    create_graph() %>%
    set_graph_time(time = "2015-10-25 15:23:00")

  # Expect a graph time as POSIXct
  expect_is(get_graph_time(graph), "POSIXct")

  # Expect that the returned vector has a length of 1
  expect_equal(length(get_graph_time(graph)), 1)

  # When requesting just the time zone, expect
  # it to be returned as a character vector
  expect_is(get_graph_time(graph, get_tz = TRUE),
            "character")

  # Expect the tz to be `GMT` in this case
  expect_equal(get_graph_time(graph, get_tz = TRUE),
               "GMT")

  # Create a graph without a time set
  graph <- create_graph()

  # Verify that when there is no time set for the
  # graph, a POSIXct vector is returned
  expect_is(get_graph_time(graph), "POSIXct")

  # Expect that an NA is returned
  expect_true(is.na(get_graph_time(graph)))
})

test_that("Setting global graph attrs is possible", {

  # Create an empty graph with no global graph
  # parameters
  graph <- create_graph(attr_theme = FALSE)

  # Set 3 global graph attrs
  graph <-
    graph %>%
    set_global_graph_attrs(
      c("overlap", "color", "penwidth"),
      c("true", "red", "5"),
      c("graph", "node", "edge"))

  # Expect 3 rows and three columns in the
  # `global_attrs` data frame
  expect_equal(nrow(graph$global_attrs), 3)
  expect_equal(ncol(graph$global_attrs), 3)

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
      "overlap", TRUE, "graph")

  # Expect 1 row and three columns in the
  # `global_attrs` data frame
  expect_equal(nrow(graph$global_attrs), 1)
  expect_equal(ncol(graph$global_attrs), 3)

  # Expect that the `TRUE` logical value
  # was coerced to character `true`
  expect_equal(
    graph$global_attrs$value, "true")
})

test_that("Getting global graph attrs is possible", {

  # Create an empty graph with no global graph
  # parameters
  graph <- create_graph(attr_theme = FALSE)

  # Set 3 global graph attrs
  graph <-
    graph %>%
    set_global_graph_attrs(
      c("overlap", "color", "penwidth"),
      c("true", "red", "5"),
      c("graph", "node", "edge"))

  # Get a data frame with the attributes
  # using `get_global_graph_attrs()`
  global_graph_attrs <-
    get_global_graph_attrs(graph)

  # Expect that the data frame returned
  # by the function is equivalent to the
  # data frame stored in the graph object
  expect_equal(
    graph$global_attrs, global_graph_attrs)
})
