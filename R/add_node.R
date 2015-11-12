#' Add a node to an existing graph object
#' @description With a graph object of class \code{dgr_graph}, add a new node
#' of a specified type to extant nodes within the graph.
#' @param graph a graph object of class \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param type an optional string that describes the entity type for the node
#' to be added.
#' @param label a character object for supplying an optional label to the node.
#' Setting to \code{TRUE} ascribes the node ID to the label. Setting to
#' \code{FALSE} yields a blank label.
#' @param from an optional vector containing node IDs from which edges will be
#' directed to the new node.
#' @param to an optional vector containing node IDs to which edges will be
#' directed from the new node.
#' @param node an optional node ID for the newly connected node. If no value
#' is provided, a node ID will assigned as a monotonically increasing integer.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # Add two nodes
#' graph <- add_node(graph)
#' graph <- add_node(graph)
#'
#' get_nodes(graph)
#' #> [1] "1" "2"
#'
#' # Add a node with 'type' defined
#' graph <- add_node(graph, type = "person")
#'
#' get_node_df(graph)
#' #>   nodes   type label
#' #> 1     1            1
#' #> 2     2            2
#' #> 3     3 person     3
#' }
#' @export add_node

add_node <- function(graph,
                     type = NULL,
                     label = TRUE,
                     from = NULL,
                     to = NULL,
                     node = NULL){

  # If node ID not provided, create a monotonically increasing ID value
  if (is.null(node)){

    if (node_count(graph) == 0){
      node <- 1
    }

    if (node_count(graph) > 0){

      if (!is.na(suppressWarnings(any(as.numeric(get_nodes(graph)))))){

        numeric_components <-
          suppressWarnings(which(!is.na(as.numeric(get_nodes(graph)))))

        node <-
          max(as.integer(as.numeric(get_nodes(graph)[numeric_components]))) + 1
      }

      if (suppressWarnings(all(is.na(as.numeric(get_nodes(graph)))))){
        node <- 1
      }
    }
  }

  # Verify that 'node' is given as a single value
  node_is_single_value <- ifelse(length(node) == 1, TRUE, FALSE)

  # Stop function if node not a single value
  if (node_is_single_value == FALSE){
    stop("Only a single node can be added.")
  }

  # Determine whether node to add is already in graph
  if (node_is_single_value == TRUE){
    can_add_node_id <-
      ifelse(!node_present(graph = graph, node = node), TRUE, FALSE)
  }

  if (can_add_node_id == FALSE){
    return(graph)
  }

  # Modify graph if only 'from' values provided
  if (!is.null(from) & is.null(to)){

    from_nodes_available <-
      ifelse(all(from %in% get_nodes(graph)), TRUE, FALSE)

    if (from_nodes_available == FALSE){

      stop("The nodes from which edges should be applied to the new node are not available.")
    }

    if (from_nodes_available){

      combined_nodes <-
        combine_nodes(graph$nodes_df,
                      create_nodes(nodes = node,
                                   label = label,
                                   type = ifelse(is.null(type), "", type)))

      if (!is.null(graph$edges_df)){

        combined_edges <-
          combine_edges(graph$edges_df,
                        create_edges(from = from,
                                     to = rep(node, length(from))))

        dgr_graph <-
          create_graph(nodes_df = combined_nodes,
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

      if (is.null(graph$edges_df)){

        dgr_graph <-
          create_graph(nodes_df = combined_nodes,
                       edges_df = create_edges(from = from,
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

      # Return the revised graph
      return(dgr_graph)
    }
  }

  # Modify graph if only 'to' values provided
  if (is.null(from) & !is.null(to)){

    to_nodes_available <- ifelse(all(to %in% get_nodes(graph)), TRUE, FALSE)

    if (to_nodes_available == FALSE){

      stop("The nodes to which edges should be applied from the new node are not available.")
    }

    combined_nodes <-
      combine_nodes(graph$nodes_df,
                    create_nodes(nodes = node,
                                 label = label,
                                 type = ifelse(is.null(type), "", type)))

    if (!is.null(graph$edges_df)){

      combined_edges <-
        combine_edges(graph$edges_df,
                      create_edges(from = rep(node, length(to)),
                                   to = to))

      dgr_graph <-
        create_graph(nodes_df = combined_nodes,
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

    if (is.null(graph$edges_df)){

      dgr_graph <-
        create_graph(nodes_df = combined_nodes,
                     edges_df = create_edges(from = rep(node, length(to)),
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

    # Return the revised graph
    return(dgr_graph)
  }

  # Modify graph if both 'to' and 'from' values provided
  if (!is.null(from) & !is.null(to)){

    from_nodes_available <- ifelse(all(from %in% get_nodes(graph)), TRUE, FALSE)

    to_nodes_available <- ifelse(all(to %in% get_nodes(graph)), TRUE, FALSE)

    if (from_nodes_available == FALSE){
      stop("The nodes from which edges should be applied to the new node are not available.")
    }

    if (to_nodes_available == FALSE){
      stop("The nodes to which edges should be applied from the new node are not available.")
    }

    if (from_nodes_available & to_nodes_available){

      combined_nodes <-
        combine_nodes(graph$nodes_df,
                      create_nodes(nodes = node,
                                   label = label,
                                   type = ifelse(is.null(type), "", type)))

      if (!is.null(graph$edges_df)){

        combined_edges <-
          combine_edges(graph$edges_df,
                        create_edges(from = from,
                                     to = rep(node, length(from))),
                        create_edges(from = rep(node, length(to)),
                                     to = to))
      }

      if (is.null(graph$edges_df)){

        combined_edges <-
          combine_edges(create_edges(from = from,
                                     to = rep(node, length(from))),
                        create_edges(from = rep(node, length(to)),
                                     to = to))
      }

      # Create the revised graph object
      dgr_graph <-
        create_graph(nodes_df = combined_nodes,
                     edges_df = combined_edges,
                     graph_attrs = graph$graph_attrs,
                     node_attrs = graph$node_attrs,
                     edge_attrs = graph$edge_attrs,
                     directed = ifelse(is_graph_directed(graph),
                                       TRUE, FALSE),
                     graph_name = graph$graph_name,
                     graph_time = graph$graph_time,
                     graph_tz = graph$graph_tz)

      # Create a revised graph and return that graph
      return(dgr_graph)
    }
  }

  # Modify graph if none of 'to' nor 'from' values provided
  if (is.null(from) & is.null(to)){
    if (!is.null(type)){
      if (!is.null(graph$nodes_df)){

        combined_nodes <-
          combine_nodes(graph$nodes_df,
                        create_nodes(nodes = node,
                                     label = label,
                                     type = type))
      } else {
        combined_nodes <-
          create_nodes(nodes = node,
                       label = label,
                       type = type)
      }
    }

    if (is.null(type)){
      if (!is.null(graph$nodes_df)){

        combined_nodes <-
          combine_nodes(graph$nodes_df,
                        create_nodes(nodes = node,
                                     label = label))
      } else {
        combined_nodes <-
          create_nodes(nodes = node,
                       label = label)
      }
    }

    # Create a revised graph and return that graph
    dgr_graph <-
      create_graph(nodes_df = combined_nodes,
                   edges_df = graph$edges_df,
                   graph_attrs = graph$graph_attrs,
                   node_attrs = graph$node_attrs,
                   edge_attrs = graph$edge_attrs,
                   directed = ifelse(is_graph_directed(graph),
                                     TRUE, FALSE),
                   graph_name = graph$graph_name,
                   graph_time = graph$graph_time,
                   graph_tz = graph$graph_tz)

    return(dgr_graph)
  }
}
