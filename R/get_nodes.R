#' Get node ID information from node data frames
#'
#' Provides information on the node IDs from one or several node data frames.
#'
#' @param nodes_df a data frame containing, at minimum, a column that contains node IDs for the graph. Optionally, additional columns (named as Graphviz node attributes) can be included with values for the named node attribute. These data frames can be conveniently generated using the 'create_nodes' function.
#' @export get_nodes

get_nodes <- function(...){
    # Determine which column contains node ID information
    if ("node" %in% colnames(nodes_df)){
      nodes_column <- which("node" %in% colnames(nodes_df))
    } else if ("nodes" %in% colnames(nodes_df)){
      nodes_column <- which("nodes" %in% colnames(nodes_df))
    } else if ("node_id" %in% colnames(nodes_df)){
      nodes_column <- which("node_id" %in% colnames(nodes_df))
    } else {
      stop("There is no column with node ID information.")
    }

  }



}
