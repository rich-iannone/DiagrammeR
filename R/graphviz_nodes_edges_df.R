#' Create DOT code from two data frames
#'
#' A function to generate DOT code from two data frames: one with nodes and the
#' other with edge operations.
#'
#' @param nodes_df
#' @param edges_df
#' @export graphviz_nodes_edges_df

graphviz_nodes_edges_df <- function(nodes_df, edges_df, directed = TRUE){

  # Perform basic checks of the inputs
  stopifnot(class(nodes_df) == "data.frame")

  stopifnot(class(edges_df) == "data.frame")

  stopifnot(any(c("node", "nodes", "node_id") %in%
                  colnames(nodes_df)))

  stopifnot(any(c("edge_op", "edge_ops", "edge", "edges",
                  "edge_from", "edge_to", "from", "to") %in%
                  colnames(edges_df)))

  # Force all columns to be of the character class
  for (i in 1:ncol(nodes_df)){
    nodes_df[,i] <- as.character(nodes_df[,i])
  }

  # Force all columns to be of the character class
  for (i in 1:ncol(edges_df)){
    edges_df[,i] <- as.character(edges_df[,i])
  }

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

  #
  # Create the node block
  #

  column_with_node_id <-
    which(colnames(nodes_df) %in% c("node_id", "node", "nodes"))[1]

  other_columns_with_node_attributes <-
    which(colnames(nodes_df) %in% node_attributes)

  for (i in 1:nrow(nodes_df)){

    if (i == 1) node_block <- vector(mode = "character", length = 0)

    if (length(other_columns_with_node_attributes) > 0){

      for (j in other_columns_with_node_attributes){

        if (j == other_columns_with_node_attributes[1]){
          attr_string <- vector(mode = "character", length = 0)
        }

        attribute <- paste0(colnames(nodes_df)[j], " = ", "'", nodes_df[i, j], "'")
        attr_string <- c(attr_string, attribute)
      }

      if (j == other_columns_with_node_attributes[length(other_columns_with_node_attributes)]){
        attr_string <- paste(attr_string, collapse = ", ")
      }
    }

    # Generate a line of node objects when an attribute string exists
    if (exists("attr_string")){
      line <- paste0("  node",
                     " [", attr_string, "] ",
                     "'", nodes_df[i, column_with_node_id], "'")
    }

    # Generate a line of node objects when an attribute string doesn't exist
    if (!exists("attr_string")){
      line <- paste0("  '", nodes_df[i, column_with_node_id], "'")
    }

    # Construct the 'node_block' character object
    node_block <- c(node_block, line)
  }

  node_block <- paste(node_block, collapse = "\n")

  # Remove the 'attr_string' object if it exists
  if (exists("attr_string") == TRUE){
    rm(attr_string)
  }

  # Remove the 'attribute' object if it exists
  if (exists("attribute") == TRUE){
    rm(attribute)
  }

  # Develop the edges block for a data frame containing a column with
  # explicitly defined edge operations
  column_with_edge_op <-
    which(colnames(edges_df) %in% c("edge_op", "edge_ops", "edge", "edges"))[1]

  directed_proportion <-
    sum(grepl("->", edges_df[,column_with_edge_op])) / nrow(edges_df)

  directed <- ifelse(directed_proportion > 0.8, TRUE, FALSE)

  other_columns_with_edge_attributes <-
    which(colnames(edges_df) %in% edge_attributes)

  for (i in 1:nrow(edges_df)){
    if (i == 1) edge_block <- vector(mode = "character", length = 0)

    if (length(other_columns_with_edge_attributes) > 0){


      for (j in other_columns_with_edge_attributes){

        if (j == other_columns_with_edge_attributes[1]){
          attr_string <- vector(mode = "character", length = 0)
        }

        attribute <- paste0(colnames(edges_df)[j], " = ", "'", edges_df[i, j], "'")
        attr_string <- c(attr_string, attribute)

      }

      if (j == other_columns_with_edge_attributes[length(other_columns_with_edge_attributes)]){
        attr_string <- paste(attr_string, collapse = ", ")
      }
    }
  }

    # Generate a line of node objects when an attribute string exists
    if (exists("attr_string")){

      line <-
        paste0("  edge",
               " [", attr_string, "] ",
               "'", gsub(" ", "",
                         unlist(strsplit(edges_df[i, column_with_edge_op],
                                         "-[-|>]")))[1], "'",
               ifelse(directed == TRUE, "->", "--"),
               "'", gsub(" ", "",
                         unlist(strsplit(edges_df[i, column_with_edge_op],
                                         "-[-|>]")))[2], "'"
        )
    }

    # Generate a line of node objects when an attribute string doesn't exist
    if (!exists("attr_string")){

      line <-
        paste0("  '", gsub(" ", "",
                           unlist(strsplit(edges_df[i, column_with_edge_op],
                                           "-[-|>]")))[1], "'",
               ifelse(directed == TRUE, "->", "--"),
               "'", gsub(" ", "",
                         unlist(strsplit(edges_df[i, column_with_edge_op],
                                         "-[-|>]")))[2], "'"
        )
    }

    edge_block <- c(edge_block, line)
  }

  # Construct the 'edge_block' character object
  edge_block <- paste(edge_block, collapse = "\n")

  # Combine the 'node_block' and 'edge_block' objects into a 'combined_block'
  combined_block <- paste(node_block, edge_block, sep = "\n")

  return(combined_block)
}
