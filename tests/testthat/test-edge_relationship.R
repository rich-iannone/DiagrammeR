context("Edge relationships")

test_that("a specified edge relationship can be read from graph objects", {

  # Create a node data frame
  nodes <-
    create_nodes(nodes = c("a", "b", "c", "d"),
                 label = FALSE,
                 type = "lower",
                 style = "filled",
                 color = "aqua",
                 shape = c("circle", "circle",
                           "rectangle", "rectangle"),
                 data = c(3.5, 2.6, 9.4, 2.7))

  # Create an edge data frame
  edges <-
    create_edges(from = c("a", "b", "c"),
                 to = c("d", "c", "a"),
                 rel = "leading_to")

  # Create the graph object using the node and edge data frames
  graph <- create_graph(nodes_df = nodes,
                        edges_df = edges,
                        graph_attrs = "layout = dot",
                        node_attrs = "fontname = Helvetica",
                        edge_attrs = c("color = blue",
                                       "arrowsize = 2"))

  # Expect that using a 'read' action for an edge relationship for
  # an edge that doesn't exist will throw an error
  expect_error(edge_rel(graph,
                        from = "a",
                        to = "b",
                        action = "read"))

  # Expect that using a 'read' action for an edge relationship for
  # with a relationship set will return the relationship label
  expect_equal(edge_rel(graph,
                        from = "a",
                        to = "d",
                        action = "read"),
               "leading_to")

  # Create an edge data frame with no edge relationships set
  edges_no_rel <-
    create_edges(from = c("a", "b", "c"),
                 to = c("d", "c", "a"))

  # Create the graph object using the node and edge data frames
  graph_no_rel <-
    create_graph(nodes_df = nodes,
                 edges_df = edges_no_rel,
                 graph_attrs = "layout = dot",
                 node_attrs = "fontname = Helvetica",
                 edge_attrs = c("color = blue",
                                "arrowsize = 2"))

  # Expect an NA value to be returned when reading an unset
  # relationship for an edge
  expect_true(is.na(edge_rel(graph_no_rel,
                             from = "a",
                             to = "d",
                             action = "read")))

  # Expect an FALSE value to be returned when inspecting all
  # edges with the "check" action
  expect_false(edge_rel(graph_no_rel,
                        from = "a",
                        to = "d",
                        action = "check"))

  expect_false(edge_rel(graph_no_rel,
                        from = "b",
                        to = "c",
                        action = "check"))

  expect_false(edge_rel(graph_no_rel,
                        from = "c",
                        to = "a",
                        action = "check"))
})

test_that("removing an edge relationship is possible", {

  # Create a node data frame
  nodes <-
    create_nodes(nodes = c("a", "b", "c", "d"),
                 label = FALSE,
                 type = "lower",
                 style = "filled",
                 color = "aqua",
                 shape = c("circle", "circle",
                           "rectangle", "rectangle"),
                 data = c(3.5, 2.6, 9.4, 2.7))

  # Create an edge data frame
  edges <-
    create_edges(from = c("a", "b", "c"),
                 to = c("d", "c", "a"),
                 rel = "leading_to")

  # Create the graph object using the node and edge data frames
  graph <- create_graph(nodes_df = nodes,
                        edges_df = edges,
                        graph_attrs = "layout = dot",
                        node_attrs = "fontname = Helvetica",
                        edge_attrs = c("color = blue",
                                       "arrowsize = 2"))

  # Remove the edge relationship across the 'a->d' edge using
  # the "delete" action (synonymous with "remove" and "drop")
  graph_ad_remove_1 <-
    edge_rel(graph,
             from = "a",
             to = "d",
             action = "delete")

  # Remove the edge relationship across the 'a->d' edge using
  # the "remove" action (synonymous with "delete" and "drop")
  graph_ad_remove_2 <-
    edge_rel(graph,
             from = "a",
             to = "d",
             action = "remove")

  # Remove the edge relationship across the 'a->d' edge using
  # the "drop" action (synonymous with "delete" and "remove")
  graph_ad_remove_3 <-
    edge_rel(graph,
             from = "a",
             to = "d",
             action = "drop")

  # Expect that the resulting data frames will be the same
  expect_equal(graph_ad_remove_1, graph_ad_remove_2)
  expect_equal(graph_ad_remove_2, graph_ad_remove_3)
  expect_equal(graph_ad_remove_1, graph_ad_remove_3)

  # Expect that a relationship across the 'a->d' edge won't be present
  # in all 3 instances (using both "read" and "check" actions)
  expect_true(is.na(edge_rel(graph_ad_remove_1,
                             from = "a",
                             to = "d",
                             action = "read")))

  expect_true(is.na(edge_rel(graph_ad_remove_2,
                             from = "a",
                             to = "d",
                             action = "read")))

  expect_true(is.na(edge_rel(graph_ad_remove_3,
                             from = "a",
                             to = "d",
                             action = "read")))

  expect_false(edge_rel(graph_ad_remove_1,
                        from = "a",
                        to = "d",
                        action = "check"))

  expect_false(edge_rel(graph_ad_remove_2,
                        from = "a",
                        to = "d",
                        action = "check"))

  expect_false(edge_rel(graph_ad_remove_3,
                        from = "a",
                        to = "d",
                        action = "check"))

  # Attempt to remove an edge relationship across the 'a->d' edge
  # where the relationship had already been removed; the expectation
  # is that nothing will occur and the graph will be returned unchanged
  expect_equal(graph_ad_remove_1,
               edge_rel(graph_ad_remove_1,
                        from = "a",
                        to = "d",
                        action = "delete"))

})

test_that("edge relationships can be set for edges in graph objects", {

  # Create a node data frame
  nodes <-
    create_nodes(nodes = c("a", "b", "c", "d"),
                 label = FALSE,
                 type = "lower",
                 style = "filled",
                 color = "aqua",
                 shape = c("circle", "circle",
                           "rectangle", "rectangle"),
                 data = c(3.5, 2.6, 9.4, 2.7))

  # Create an edge data frame with no edge relationships set
  edges_no_rel <-
    create_edges(from = c("a", "b", "c"),
                 to = c("d", "c", "a"))

  # Create the graph object using the node and edge data frames
  graph_no_rel <-
    create_graph(nodes_df = nodes,
                 edges_df = edges_no_rel,
                 graph_attrs = "layout = dot",
                 node_attrs = "fontname = Helvetica",
                 edge_attrs = c("color = blue",
                                "arrowsize = 2"))

  # Create an edge relationship label across the 'a->d' edge
  graph_ad_rel <-
    edge_rel(graph_no_rel,
             from = "a",
             to = "d",
             action = "add",
             value = "new_rel")

  # Attempt to add an edge relationship across the 'a->d' edge
  # where a relationship already exists; the expectation
  # is that nothing will occur and the graph will be returned unchanged
  # (update must be used to make a change to an extant relationship)
  expect_equal(graph_ad_rel,
               edge_rel(graph_ad_rel,
                        from = "a",
                        to = "d",
                        action = "add",
                        value = "newer_rel"))

  # Expect a TRUE value to be returned when inspecting the
  # 'a->d' edge "check" action but FALSE for the other edges
  expect_true(edge_rel(graph_ad_rel,
                       from = "a",
                       to = "d",
                       action = "check"))

  expect_false(edge_rel(graph_ad_rel,
                        from = "b",
                        to = "c",
                        action = "check"))

  expect_false(edge_rel(graph_ad_rel,
                        from = "c",
                        to = "a",
                        action = "check"))

  # Expect the edge relationship value returned when inspecting the
  # 'a->d' edge with the "read" action
  expect_equal(edge_rel(graph_ad_rel,
                        from = "a",
                        to = "d",
                        action = "read"),
               "new_rel")

  # Update the edge relationship value with a newer value
  graph_ad_newer_rel <-
    edge_rel(graph_ad_rel,
             from = "a",
             to = "d",
             action = "update",
             value = "newer_rel")

  # Expect that the new relationship value is returned when inspecting the
  # 'a->d' edge with the "read" action
  expect_equal(edge_rel(graph_ad_newer_rel,
                        from = "a",
                        to = "d",
                        action = "read"),
               "newer_rel")

  # Attempt to update an edge relationship value where there is no set value
  graph_no_rel_update <-
    edge_rel(graph_no_rel,
             from = "a",
             to = "d",
             action = "update",
             value = "a_relationship")

  # Expect that no change is made to the graph object
  expect_equal(graph_no_rel, graph_no_rel_update)

  # Expect that no edge relationship is present across the 'a->d' edge
  expect_false(edge_rel(graph_no_rel_update,
                        from = "a",
                        to = "d",
                        action = "check"))

  # Create a node data frame
  nodes <-
    create_nodes(nodes = c("a", "b", "c"))

  # Create an edge data frame with edge relationships set as empty values
  edges_rel_empty <-
    create_edges(from = c("a", "b", "c"),
                 to = c("b", "c", "a"),
                 rel = c(""))

  # Create the graph object using the node and edge data frames
  graph_rel_empty <-
    create_graph(nodes_df = nodes,
                 edges_df = edges_rel_empty)

  # Get an edge count and expect 3 edges
  expect_equal(edge_count(graph_rel_empty), 3)

  # Get relationship values to be returned as NAs for all 3 edges
  expect_equal(edge_rel(graph_rel_empty,
                        from = "a",
                        to = "b"),
               NA)

  expect_equal(edge_rel(graph_rel_empty,
                        from = "b",
                        to = "c"),
               NA)

  expect_equal(edge_rel(graph_rel_empty,
                        from = "c",
                        to = "a"),
               NA)
})
