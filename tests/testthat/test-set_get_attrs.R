context("Setting graph/global attributes")

test_that("setting a graph name can be done", {

  library(magrittr)

  graph <- create_graph()

  # Set a graph name
  graph_name <- set_graph_name(graph, "test_that_name")

  # Expect that the name was set
  expect_true(graph_name$graph_name == "test_that_name")

  # Add a nodes/edges and then a node selection to the original graph
  graph_1 <- graph %>% add_node(1) %>% add_node(2) %>%
    add_edge(1, 2) %>% select_nodes(nodes = 1)

  # Set a graph name
  graph_name_1 <- set_graph_name(graph_1, "test_that_name_again")

  # Expect that the name was set
  expect_true(graph_name_1$graph_name == "test_that_name_again")
})

test_that("setting a time for the graph can be done", {

  # Create an empty graph
  graph <- create_graph()

  # Provide the new graph with a timestamp (`tz` not supplied so
  # `GMT` is used as the time zone)
  graph_1 <-
    set_graph_time(graph,
                   time = "2015-10-25 15:23:00")

  # Expect that the time value was passed in properly
  expect_true(graph_1$graph_time == "2015-10-25 15:23:00")

  # Expect that the time zone is set to "GMT"
  expect_true(graph_1$graph_tz == "GMT")

  # Update tz when a timestamp is already present
  graph_2 <-
    set_graph_time(graph_1,
                   tz = "America/Los_Angeles")

  # Expect that the time zone has been changed
  expect_true(graph_2$graph_tz == "America/Los_Angeles")

  # Expect an error when setting a time zone that is
  # not in `OlsonNames()`
  expect_error(
    set_graph_time(graph_2,
                   tz = "Moon/Moon")
  )

  # Create a graph with a node and a node selection
  graph_selection <-
    create_graph() %>% add_node("A") %>% select_nodes() %>%
    set_graph_time("2015-10-25 15:23:00")

  # Expect that the selection is retained after setting
  # date-time and time zone
  expect_true(!is.null(graph_selection$selection$nodes))
})

test_that("setting/getting global graph attributes can be done", {

  # Create a new graph and set some global attributes
  graph <- create_graph() %>%
    set_global_graph_attr("graph", "overlap", "true") %>%
    set_global_graph_attr("node", "fontname", "Helvetica") %>%
    set_global_graph_attr("edge", "color", "gray")

  # Verify that the global attributes have been set
  expect_true(graph$graph_attrs[1] == "overlap = true")
  expect_equal(length(graph$graph_attrs), 1)

  expect_true(graph$node_attrs[1] == "fontname = Helvetica")
  expect_equal(length(graph$node_attrs), 1)

  expect_true(graph$edge_attrs[1] == "color = gray")
  expect_equal(length(graph$edge_attrs), 1)

  # Expect that getting the global attributes is possible
  expect_equal(length(get_global_graph_attr(graph)), 3)

  expect_true(get_global_graph_attr(graph)[1][[1]] == "overlap = true")
  expect_true(get_global_graph_attr(graph)[2][[1]] == "fontname = Helvetica")
  expect_true(get_global_graph_attr(graph)[3][[1]] == "color = gray")
})
