#' Get node degree values from nodes available in a selection
#' @description From a graph object of class \code{dgr_graph}, get a
#' vector of node ID values from nodes available in a selection.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @param degree_type the type of degree, either: \code{in} for the indegree,
#' \code{out} for the outdegree, and \code{both} or \code{degree} for the
#' total degree (i.e., indegree + outdegree).
#' @param degree_values a logical expression with a comparison
#' operator (\code{>}, \code{<}, \code{==}, or \code{!=}) followed by a number
#' for numerical filtering.
#' @param node_attr an optional character vector of node attribute values for
#' filtering the node ID values returned.
#' @param search an option to provide a logical expression with a comparison
#' operator (\code{>}, \code{<}, \code{==}, or \code{!=}) followed by a number
#' for numerical filtering, or, a regular expression for filtering the nodes
#' returned through string matching.
#' @param set_op the set operation to perform upon consecutive selections
#' of graph nodes. This can either be as a \code{union} (the default), as an
#' \code{intersection}, or, as a \code{difference} on the previous selection,
#' if it exists.
#' @param nodes an optional vector of node IDs for filtering list of
#' nodes present in the graph.
#' @return a graph object of class \code{dgr_graph}.
#' @export select_nodes_by_degree

select_nodes_by_degree <- function(graph,
                                   degree_type,
                                   degree_values,
                                   node_attr = NULL,
                                   search = NULL,
                                   set_op = "union",
                                   nodes = NULL){

  if (is_graph_empty(graph)){
    stop("The graph is empty so no selections can be made.")
  }

  nodes_df <- graph$nodes_df

  if (!is.null(node_attr)){
    if (length(node_attr) > 1){
      stop("Only one node attribute can be specified.")
    }

    if (!(node_attr %in% colnames(nodes_df)[-1])){
      stop("The specified attribute is not available.")
    }
  }

  if (is.null(node_attr)){

    nodes_selected <- nodes_df$nodes

    if (!is.null(nodes)){

      if (any(!(nodes %in% nodes_selected))){
        stop("One of more of the nodes specified are not available in the graph.")
      }

      nodes_selected <- nodes
    }
  }

  if (!is.null(node_attr)){

    # Filter nodes_df by node ID values in 'nodes'
    if (!is.null(nodes)){

      if (any(!(nodes %in% nodes_df$nodes))){
        stop("One of more of the nodes specified are not available in the graph.")
      }

      nodes_df <- nodes_df[which(nodes_df$nodes %in% nodes),]
    }

    # Determine the column number for which the 'node_attr' is available
    column_number <-
      which(colnames(nodes_df) %in% node_attr)

    # If a search term provided, filter using a logical expression
    # or a regex match
    if (!is.null(search)){

      if (grepl("^>.*", search) | grepl("^<.*", search) |
          grepl("^==.*", search) | grepl("^!=.*", search)){
        logical_expression <- TRUE } else {
          logical_expression <- FALSE
        }

      # Filter using a logical expression
      if (logical_expression){

        if (grepl("^>.*", search)){
          rows_where_true_le <-
            which(nodes_df[,column_number] >
                    as.numeric(gsub(">(.*)", "\\1", search)))
        }

        if (grepl("^<.*", search)){
          rows_where_true_le <-
            which(nodes_df[,column_number] <
                    as.numeric(gsub("<(.*)", "\\1", search)))
        }

        if (grepl("^==.*", search)){
          rows_where_true_le <-
            which(nodes_df[,column_number] ==
                    as.numeric(gsub("==(.*)", "\\1", search)))
        }

        if (grepl("^!=.*", search)){
          rows_where_true_le <-
            which(nodes_df[,column_number] !=
                    as.numeric(gsub("!=(.*)", "\\1", search)))
        }

        nodes_selected <- nodes_df[rows_where_true_le, 1]
      }

      # Filter using a `search` value as a regular expression
      if (logical_expression == FALSE){

        rows_where_true_regex <-
          which(grepl(search, as.character(nodes_df[,column_number])))

        nodes_selected <- nodes_df[rows_where_true_regex, 1]
      }
    }
  }

  # Filter node ID values by degree type and degree values
  if (grepl("^>.*", degree_values) | grepl("^<.*", degree_values) |
      grepl("^==.*", degree_values) | grepl("^!=.*", degree_values)){
    logical_expression <- TRUE } else {
      logical_expression <- FALSE
    }

  if (logical_expression){

    if (grepl("^>.*", degree_values)){

      if (degree_type == "both" | degree_type == "degree"){
        nodes_selected <-
          node_info(graph)[which(node_info(graph)[which(node_info(graph)[,1] %in%
                                                          nodes_selected), 4] >
                                 as.numeric(gsub(">(.*)", "\\1", degree_values))),1]
      }

      if (degree_type == "in"){
        nodes_selected <-
          node_info(graph)[which(node_info(graph)[which(node_info(graph)[,1] %in%
                                                          nodes_selected), 5] >
                                 as.numeric(gsub(">(.*)", "\\1", degree_values))),1]
      }

      if (degree_type == "out"){
        nodes_selected <-
          node_info(graph)[which(node_info(graph)[which(node_info(graph)[,1] %in%
                                                          nodes_selected), 6] >
                                 as.numeric(gsub(">(.*)", "\\1", degree_values))),1]
      }
    }

    if (grepl("^<.*", degree_values)){

      if (degree_type == "both" | degree_type == "degree"){
        nodes_selected <-
          node_info(graph)[which(node_info(graph)[which(node_info(graph)[,1] %in%
                                                          nodes_selected), 4] <
                                 as.numeric(gsub("<(.*)", "\\1", degree_values))),1]
      }

      if (degree_type == "in"){
        nodes_selected <-
          node_info(graph)[which(node_info(graph)[which(node_info(graph)[,1] %in%
                                                          nodes_selected), 5] <
                                 as.numeric(gsub("<(.*)", "\\1", degree_values))),1]
      }

      if (degree_type == "out"){
        nodes_selected <-
          node_info(graph)[which(node_info(graph)[which(node_info(graph)[,1] %in%
                                                          nodes_selected), 6] <
                                 as.numeric(gsub("<(.*)", "\\1", degree_values))),1]
      }
    }

    if (grepl("^!=.*", degree_values)){

      if (degree_type == "both" | degree_type == "degree"){
        nodes_selected <-
          node_info(graph)[which(node_info(graph)[which(node_info(graph)[,1] %in%
                                                          nodes_selected), 4] ==
                                 as.numeric(gsub("==(.*)", "\\1", degree_values))),1]
      }

      if (degree_type == "in"){
        nodes_selected <-
          node_info(graph)[which(node_info(graph)[which(node_info(graph)[,1] %in%
                                                          nodes_selected), 5] ==
                                 as.numeric(gsub("==(.*)", "\\1", degree_values))),1]
      }

      if (degree_type == "out"){
        nodes_selected <-
          node_info(graph)[which(node_info(graph)[which(node_info(graph)[,1] %in%
                                                          nodes_selected), 6] ==
                                 as.numeric(gsub("==(.*)", "\\1", degree_values))),1]
      }
    }

    if (grepl("^!=.*", degree_values)){

      if (degree_type == "both" | degree_type == "degree"){
        nodes_selected <-
          node_info(graph)[which(node_info(graph)[which(node_info(graph)[,1] %in%
                                                          nodes_selected), 4] !=
                                 as.numeric(gsub("!=(.*)", "\\1", degree_values))),1]
      }

      if (degree_type == "in"){
        nodes_selected <-
          node_info(graph)[which(node_info(graph)[which(node_info(graph)[,1] %in%
                                                          nodes_selected), 5] !=
                                 as.numeric(gsub("!=(.*)", "\\1", degree_values))),1]
      }

      if (degree_type == "out"){
        nodes_selected <-
          node_info(graph)[which(node_info(graph)[which(node_info(graph)[,1] %in%
                                                          nodes_selected), 6] !=
                                 as.numeric(gsub("!=(.*)", "\\1", degree_values))),1]
      }
    }
  }

  if (logical_expression == FALSE){

    if (degree_type == "both" | degree_type == "degree"){
      nodes_selected <-
        node_info(graph)[which(node_info(graph)[which(node_info(graph)[,1] %in%
                                                        nodes_selected), 4] ==
                                 as.numeric(degree_values)),1]
    }

    if (degree_type == "in"){
      nodes_selected <-
        node_info(graph)[which(node_info(graph)[which(node_info(graph)[,1] %in%
                                                        nodes_selected), 5] ==
                                 as.numeric(degree_values)),1]
    }

    if (degree_type == "out"){
      nodes_selected <-
        node_info(graph)[which(node_info(graph)[which(node_info(graph)[,1] %in%
                                                        nodes_selected), 6] ==
                                 as.numeric(degree_values)),1]
    }
  }

  # If no node ID values in `nodes_selected` return the graph
  # without a changed node selection
  if (length(nodes_selected) == 0){
    return(graph)
  }

  # Obtain vector of node IDs selection of nodes already present
  if (!is.null(graph$selection)){
    if (!is.null(graph$selection$nodes)){
      nodes_prev_selection <- graph$selection$nodes
    }
  } else {
    nodes_prev_selection <- vector(mode = "character")
  }

  # Incorporate selected nodes into graph's selection section
  if (set_op == "union"){
    nodes_combined <- union(nodes_prev_selection, nodes_selected)
  } else if (set_op == "intersect"){
    nodes_combined <- intersect(nodes_prev_selection, nodes_selected)
  } else if (set_op == "difference"){
    nodes_combined <- setdiff(nodes_prev_selection, nodes_selected)
  }

  graph$selection$nodes <- nodes_combined

  return(graph)
}
