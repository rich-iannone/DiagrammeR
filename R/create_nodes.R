#' Create a data frame with nodes and their attributes
#' @description Combine several named vectors for nodes
#' and their attributes into a data frame, which can be
#' combined with other similarly-generated data frames,
#' or, added to a graph object.
#' @param nodes the node ID value(s) for the node(s)
#' to be created.
#' @param type an optional \code{type} label for each
#' node.
#' @param label an optional label for each node.
#' @param ... one or more named vectors for associated
#' attributes.
#' @return a node data frame (ndf).
#' @examples
#' # Create a node data frame (ndf)
#' nodes <-
#'   create_nodes(
#'     nodes = 1:4,
#'     type = "a",
#'     label = TRUE,
#'     style = "filled",
#'     color = "aqua",
#'     shape = c("circle", "circle",
#'               "rectangle", "rectangle"),
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Display the `nodes` ndf
#' nodes
#' #>   nodes type label  style color     shape value
#' #> 1     1    a     1 filled  aqua    circle   3.5
#' #> 2     2    a     2 filled  aqua    circle   2.6
#' #> 3     3    a     3 filled  aqua rectangle   9.4
#' #> 4     4    a     4 filled  aqua rectangle   2.7
#' @export create_nodes

create_nodes <- function(nodes,
                         type = NULL,
                         label = nodes,
                         ...) {

  if (is.null(type)) {
    type <- as.character(rep("", length(nodes)))
  }

  if (!is.null(type)) {
    # Expand vectors with single values to fill to
    # the number of nodes
    if (length(type) == 1) {
      type <- rep(type, length(nodes))
    }

    # Expand vectors with `length` > `1` and
    # `length` < `length(nodes)`
    if (length(type) > 1 &
        length(type) < length(nodes)) {
      type <-
        c(type, rep("", (length(nodes) - length(type))))
    }

    # Trim vectors with number of values exceeding the
    # number of nodes
    if (length(type) > length(nodes)) {
      type <- type[1:length(nodes)]
    }
  }

  # Collect extra vectors of data as `extras`
  extras <- list(...)

  if (length(extras) > 0) {
    for (i in 1:length(extras)) {

      # Expand vectors with single values to fill to
      # the number of nodes
      if (length(extras[[i]]) == 1) {
        extras[[i]] <- rep(extras[[i]], length(nodes))
      }

      # Expand vectors with `length` > `1` and
      # `length` < `length(nodes)`
      if (length(extras[[i]]) > 1 &
          length(extras[[i]]) < length(nodes)) {
        extras[[i]] <-
          c(extras[[i]],
            rep("", (length(nodes) -
                       length(extras[[i]]))))
      }

      # Trim vectors with number of values exceeding
      # the number of nodes
      if (length(extras[[i]]) > length(nodes)) {
        extras[[i]] <- extras[[i]][1:length(nodes)]
      }
    }
  }

  # Change logical for labels to empty labels
  if (class(label) == "logical" &
      length(label) == 1) {
    if (label) {
      label <- as.character(nodes)
    } else {
      label <- rep("", length(nodes))
    }
  }

  if (length(extras) > 0) {
    nodes_df <-
      data.frame(nodes = nodes,
                 type = type,
                 label = label,
                 extras,
                 stringsAsFactors = FALSE)
  } else {
    nodes_df <-
      data.frame(nodes = nodes,
                 type = type,
                 label = label,
                 stringsAsFactors = FALSE)
  }

  return(nodes_df)
}
