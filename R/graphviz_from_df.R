#' Create DOT code from two data frames
#'
#' A function to generate DOT code from two data frames: one with nodes and the
#' other with edge operations.
#'
#' @param nodes_df
#' @param edges_df
#' @export graphviz_node_edge_blocks_df

graphviz_node_edge_blocks_df <- function(nodes_df, edges_df){

  # Perform basic checks of the inputs
  stopifnot(class(nodes_df) == "data.frame")

  stopifnot(class(edges_df) == "data.frame")

  stopifnot(any(c("node", "nodes", "node_id") %in% colnames(nodes_df)))

  stopifnot(any(c("edge_op", "edge_ops", "edges", "edge") %in% colnames(edges_df)))

  node_attributes <- c("color", "colorscheme", "distortion", "fillcolor",
                       "fixedsize", "fontcolor", "fontname", "fontsize",
                       "group", "height", "image", "labelloc", "margin",
                       "orientation", "penwidth", "peripheries", "shape",
                       "sides", "skew", "style", "tooltip", "width")

  edge_attributes <- c("arrowhead", "arrowsize", "arrowtail", "color",
                       "colorscheme", "constraint", "decorate", "dir",
                       "edgeURL", "edgehref", "edgetarget", "edgetooltip",
                       "fontcolor", "fontname", "fontsize", "headclip",
                       "headhref", "headlabel", "headport", "headtarget",
                       "headtooltip", "headURL", "href", "id", "label",
                       "labelangle", "labeldistance", "labelfloat", "labelfontcolor",
                       "labelfontname", "labelfontsize", "labelhref", "labelURL",
                       "labeltarget", "labeltooltip", "layer", "lhead",
                       "ltail", "minlen", "penwidth", "samehead",
                       "sametail", "style", "tailclip", "tailhref",
                       "taillabel", "tailport", "tailtarget", "tailtooltip",
                       "tailURL", "target", "tooltip", "weight")

  # Develop the node block
  column_with_node_id <- which(colnames(nodes_df) == "node_id")

  other_columns_with_node_attributes <-
    which(colnames(nodes_df) %in% node_attributes)

  for (i in 1:nrow(nodes_df)){
    if (i == 1) node_block <- vector(mode = "character", length = 0)

    if (length(other_columns_with_node_attributes) > 0){


      for (j in other_columns_with_node_attributes){

        if (j == other_columns_with_node_attributes[1]){
          attr_string <- vector(mode = "character", length = 0)
        }

        attr <- paste0(colnames(nodes_df)[j], " = ", "'", nodes_df[i, j], "'")
        attr_string <- c(attr_string, attr)

      }

      if (j == other_columns_with_node_attributes[length(other_columns_with_node_attributes)]){
        attr_string <- paste(attr_string, collapse = ", ")
      }
    }

    line <- paste0("  node",
                   ifelse(exists("attr_string"), paste0(" [", attr_string, "] "), paste0("")),
                   "'", nodes_df[i, column_with_node_id], "'")

    node_block <- c(node_block, line)
  }

  node_block <- paste(node_block, collapse = "\n")

  # Remove the 'attr_string' object if it exists
  if (exists("attr_string") == TRUE){
    rm(attr_string)
  }

  # Develop the edges block
  column_with_edge_op <- which(colnames(edges_df) == "edge_op")

  other_columns_with_edge_attributes <-
    which(colnames(edges_df) %in% edge_attributes)

  for (i in 1:nrow(edges_df)){
    if (i == 1) edge_block <- vector(mode = "character", length = 0)

    if (length(other_columns_with_edge_attributes) > 0){


      for (j in other_columns_with_edge_attributes){

        if (j == other_columns_with_edge_attributes[1]){
          attr_string <- vector(mode = "character", length = 0)
        }

        attr <- paste0(colnames(edges_df)[j], " = ", "'", edges_df[i, j], "'")
        attr_string <- c(attr_string, attr)

      }

      if (j == other_columns_with_edge_attributes[length(other_columns_with_edge_attributes)]){
        attr_string <- paste(attr_string, collapse = ", ")
      }
    }

    line <- paste0("  edge",
                   ifelse(exists("attr_string"), paste0(" [", attr_string, "] "), paste0("")),
                   " ", edges_df[i, column_with_edge_op], " ")

    edge_block <- c(edge_block, line)
  }

  edge_block <- paste(edge_block, collapse = "\n")

  combined_block <- paste(node_block, edge_block, sep = "\n")

  # Combine the node and edge blocks
  return(combined_block)

}
