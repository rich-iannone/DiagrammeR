#' Add a node to an existing graph object
#' @description With a graph object of class
#' \code{dgr_graph}, add a new node of a specified type
#' to extant nodes within the graph.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param type an optional string that describes the
#' entity type for the node to be added.
#' @param label a character object for supplying an
#' optional label to the node. Setting to \code{TRUE}
#' ascribes the node ID to the label. Setting to
#' \code{FALSE} yields a blank label.
#' @param from an optional vector containing node IDs
#' from which edges will be directed to the new node.
#' @param to an optional vector containing node IDs to
#' which edges will be directed from the new node.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an empty graph and add 2 nodes by using
#' # the `add_node()` function twice
#' graph <-
#'   create_graph() %>%
#'   add_node %>%
#'   add_node
#'
#' # Get a count of nodes in the graph
#' node_count(graph)
#' #> [1] 2
#'
#' # The nodes added were given ID values `1` and `2`
#' get_node_ids(graph)
#' #> [1] 1 2
#'
#' # Add a node with a `type` value defined
#' graph <- add_node(graph, "person")
#'
#' # View the graph's internal node data frame (ndf)
#' get_node_df(graph)
#' #>   id   type label
#' #> 1  1            1
#' #> 2  2            2
#' #> 3  3 person     3
#' @export add_node

add_node <- function(graph,
                     type = NULL,
                     label = TRUE,
                     from = NULL,
                     to = NULL) {

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Get the node ID for the node to be added
  node <- nodes_created + 1

  # Modify graph if only `from` values provided
  if (!is.null(from) & is.null(to)) {

    from_nodes_available <-
      ifelse(all(from %in% get_node_ids(graph)),
             TRUE, FALSE)

    if (from_nodes_available == FALSE) {
      stop("The nodes from which edges should be applied to the new node are not available.")
    }

    if (from_nodes_available) {

      new_node <-
        create_node_df(
          n = 1,
          label = label,
          type = ifelse(is.null(type), "", type))

      new_node[1, 1] <- node

      if (label == TRUE) {
        new_node[1, 3] <- new_node[1, 1]
      }

      combined_nodes <-
        combine_nodes(graph$nodes_df, new_node)

      if (!is.null(graph$edges_df)) {

        combined_edges <-
          combine_edges(
            graph$edges_df,
            create_edges(
              from = from,
              to = rep(node, length(from))))

        # Create a revised graph
        dgr_graph <-
          create_graph(
            nodes_df = combined_nodes,
            edges_df = combined_edges,
            graph_attrs = graph$graph_attrs,
            node_attrs = graph$node_attrs,
            edge_attrs = graph$edge_attrs,
            directed = ifelse(is_graph_directed(graph),
                              TRUE, FALSE),
            graph_name = graph$graph_name,
            graph_time = graph$graph_time,
            graph_tz = graph$graph_tz)
      }

      if (is.null(graph$edges_df)) {

        # Create a revised graph
        dgr_graph <-
          create_graph(
            nodes_df = combined_nodes,
            edges_df = create_edges(
              from = from,
              to = rep(node, length(from))),
            graph_attrs = graph$graph_attrs,
            node_attrs = graph$node_attrs,
            edge_attrs = graph$edge_attrs,
            directed = ifelse(is_graph_directed(graph),
                              TRUE, FALSE),
            graph_name = graph$graph_name,
            graph_time = graph$graph_time,
            graph_tz = graph$graph_tz)
      }

      # Update the `last_node` counter
      dgr_graph$last_node <- nodes_created + 1

      return(dgr_graph)
    }
  }

  # Modify graph if only `to` values provided
  if (is.null(from) & !is.null(to)) {

    to_nodes_available <-
      ifelse(all(to %in% get_node_ids(graph)),
             TRUE, FALSE)

    if (to_nodes_available == FALSE) {
      stop("The nodes to which edges should be applied from the new node are not available.")
    }

    new_node <-
      create_node_df(
        n = 1,
        label = label,
        type = ifelse(is.null(type), "", type))

    new_node[1, 1] <- node

    if (label == TRUE) {
      new_node[1, 3] <- new_node[1, 1]
    }

    combined_nodes <-
      combine_nodes(graph$nodes_df, new_node)

    if (!is.null(graph$edges_df)) {

      combined_edges <-
        combine_edges(
          graph$edges_df,
          create_edge_df(
            from = rep(node, length(to)),
            to = to))

      # Create a revised graph
      dgr_graph <-
        create_graph(
          nodes_df = combined_nodes,
          edges_df = combined_edges,
          graph_attrs = graph$graph_attrs,
          node_attrs = graph$node_attrs,
          edge_attrs = graph$edge_attrs,
          directed = ifelse(is_graph_directed(graph),
                            TRUE, FALSE),
          graph_name = graph$graph_name,
          graph_time = graph$graph_time,
          graph_tz = graph$graph_tz)
    }

    if (is.null(graph$edges_df)) {

      # Create a revised graph
      dgr_graph <-
        create_graph(
          nodes_df = combined_nodes,
          edges_df = create_edge_df(
            from = rep(node, length(to)),
            to = to),
          graph_attrs = graph$graph_attrs,
          node_attrs = graph$node_attrs,
          edge_attrs = graph$edge_attrs,
          directed = ifelse(is_graph_directed(graph),
                            TRUE, FALSE),
          graph_name = graph$graph_name,
          graph_time = graph$graph_time,
          graph_tz = graph$graph_tz)
    }

    # Update the `last_node` counter
    dgr_graph$last_node <- nodes_created + 1

    return(dgr_graph)
  }

  # Modify graph if both `to` and `from`
  # values provided
  if (!is.null(from) & !is.null(to)) {

    from_nodes_available <-
      ifelse(all(from %in% get_node_ids(graph)),
             TRUE, FALSE)

    to_nodes_available <-
      ifelse(all(to %in% get_node_ids(graph)),
             TRUE, FALSE)

    if (from_nodes_available == FALSE) {
      stop("The nodes from which edges should be applied to the new node are not available.")
    }

    if (to_nodes_available == FALSE) {
      stop("The nodes to which edges should be applied from the new node are not available.")
    }

    if (from_nodes_available & to_nodes_available) {

      combined_nodes <-
        combine_nodes(
          graph$nodes_df,
          create_node_df(
            n = 1,
            label = label,
            type = ifelse(is.null(type), "", type)))

      if (!is.null(graph$edges_df)) {

        combined_edges <-
          combine_edges(
            graph$edges_df,
            create_edge_df(
              from = from,
              to = rep(node, length(from))),
            create_edge_df(
              from = rep(node, length(to)),
              to = to))
      }

      if (is.null(graph$edges_df)) {

        combined_edges <-
          combine_edges(
            create_edge_df(
              from = from,
              to = rep(node, length(from))),
            create_edge_df(
              from = rep(node, length(to)),
              to = to))
      }

      # Create a revised graph and return that graph
      dgr_graph <-
        create_graph(
          nodes_df = combined_nodes,
          edges_df = combined_edges,
          graph_attrs = graph$graph_attrs,
          node_attrs = graph$node_attrs,
          edge_attrs = graph$edge_attrs,
          directed = ifelse(is_graph_directed(graph),
                            TRUE, FALSE),
          graph_name = graph$graph_name,
          graph_time = graph$graph_time,
          graph_tz = graph$graph_tz)

      # Update the `last_node` counter
      dgr_graph$last_node <- nodes_created + 1

      return(dgr_graph)
    }
  }

  # Modify graph if neither `to` nor `from`
  # values provided
  if (is.null(from) & is.null(to)) {
    if (!is.null(type)) {
      if (!is.null(graph$nodes_df)) {

        new_node <-
          create_node_df(
            n = 1,
            label = label,
            type = type)

        new_node[1, 1] <- node

        if (label == TRUE) {
          new_node[1, 3] <- new_node[1, 1]
        }

        combined_nodes <-
          combine_nodes(graph$nodes_df, new_node)

      } else {

        combined_nodes <-
          create_node_df(
            n = 1,
            label = label,
            type = type)
      }
    }

    if (is.null(type)) {
      if (!is.null(graph$nodes_df)) {

        new_node <-
          create_node_df(
            n = 1,
            label = label)

        new_node[1, 1] <- node

        if (label == TRUE) {
          new_node[1, 3] <- new_node[1, 1]
        }

        combined_nodes <-
          combine_nodes(graph$nodes_df, new_node)

      } else {
        combined_nodes <-
          create_node_df(
            n = 1,
            label = label)
      }
    }

    # Create a revised graph and return that graph
    dgr_graph <-
      create_graph(
        nodes_df = combined_nodes,
        edges_df = graph$edges_df,
        graph_attrs = graph$graph_attrs,
        node_attrs = graph$node_attrs,
        edge_attrs = graph$edge_attrs,
        directed = ifelse(is_graph_directed(graph),
                          TRUE, FALSE),
        graph_name = graph$graph_name,
        graph_time = graph$graph_time,
        graph_tz = graph$graph_tz)

    # Update the `last_node` counter
    dgr_graph$last_node <- nodes_created + 1

    return(dgr_graph)
  }
}
