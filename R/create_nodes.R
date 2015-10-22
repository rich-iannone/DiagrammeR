#' Create a data frame with nodes and their attributes
#' @description Combine several named vectors for nodes and their attributes
#' into a data frame, which can be combined with other similarly-generated data
#' frame, or, added to a graph object.
#' @param nodes the node ID value(s) for the node(s) to be created.
#' @param type an optional 'type' description for each node.
#' @param label an optional label for each node.
#' @param ... one or more named vectors for associated attributes.
#' @return a data frame.
#' @examples
#' \dontrun{
#' # Create a node data frame
#' nodes <-
#'   create_nodes(nodes = c("a", "b", "c", "d"),
#'                type = "letter",
#'                label = TRUE,
#'                style = "filled",
#'                color = "aqua",
#'                shape = c("circle", "circle",
#'                          "rectangle", "rectangle"),
#'                value = c(3.5, 2.6, 9.4, 2.7))
#' }
#' @export create_nodes

create_nodes <- function(nodes,
                         type = NULL,
                         label = nodes,
                         ...){

  if (is.null(type)){
    type <- as.character(rep("", length(nodes)))
  }

  if (!is.null(type)){

    # Expand vectors with single values to fill to number of nodes
    if (length(type) == 1){
      type <- rep(type, length(nodes))
    }

    # Expand vectors with length > 1 and length < 'length(nodes)'
    if (length(type) > 1 & length(type) < length(nodes)){
      type <-
        c(type, rep("", (length(nodes) - length(type))))
    }

    # Trim vectors with number of values exceeding number of nodes
    if (length(type) > length(nodes)){
      type <- type[1:length(nodes)]
    }
  }

  # Collect extra vectors of data as 'extras'
  extras <- list(...)

  if (length(extras) > 0){

    for (i in 1:length(extras)){

      # Expand vectors with single values to fill to number of nodes
      if (length(extras[[i]]) == 1){
        extras[[i]] <- rep(extras[[i]], length(nodes))
      }

      # Expand vectors with length > 1 and length < 'length(nodes)'
      if (length(extras[[i]]) > 1 & length(extras[[i]]) < length(nodes)){
        extras[[i]] <-
          c(extras[[i]], rep("", (length(nodes) - length(extras[[i]]))))
      }

      # Trim vectors with number of values exceeding number of nodes
      if (length(extras[[i]]) > length(nodes)){
        extras[[i]] <- extras[[i]][1:length(nodes)]
      }
    }
  }

  # Change logical for labels to empty labels
  if (class(label) == "logical" & length(label) == 1){

    if (label == TRUE){

      label <- as.character(nodes)

    } else {

      label <- rep("", length(nodes))
    }
  }

  if (length(extras) > 0){

    nodes_df <- data.frame(nodes = nodes, type = type, label = label,
                           extras, stringsAsFactors = FALSE)
  } else {

    nodes_df <- data.frame(nodes = nodes, type = type, label = label,
                           stringsAsFactors = FALSE)
  }

  return(nodes_df)
}
