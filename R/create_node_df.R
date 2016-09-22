#' Create a node data frame
#' @description Combine several named vectors for nodes
#' and their attributes into a data frame, which can be
#' combined with other similarly-generated data frames,
#' or, added to a graph object.
#' @param n the total number of nodes to include in the
#' node data frame.
#' @param type an optional \code{type} for each
#' node.
#' @param label an optional \code{label} for each node.
#' @param ... one or more named vectors for associated
#' attributes.
#' @return a node data frame (ndf).
#' @examples
#' # Create a node data frame (ndf) where the labels
#' # are equivalent to the node ID values (this is not
#' # recommended)
#' nodes <-
#'   create_node_df(
#'     n = 4,
#'     type = c("a", "a", "b", "b"),
#'     label = TRUE)
#'
#' # Create an ndf with distinct labels
#' nodes <-
#'   create_node_df(
#'     n = 4,
#'     type = "a",
#'     label = TRUE),
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
#' @export create_node_df

create_node_df <- function(n,
                           type = NULL,
                           label = NULL,
                           ...) {

  if (is.null(type)) {
    type <- as.character(rep("", n))
  }

  if (!is.null(type)) {
    # Expand vectors with single values to fill to
    # the number of nodes
    if (length(type) == 1) {
      type <- rep(type, n)
    }

    # Expand vectors with `length` > `1` and
    # `length` < `length(nodes)`
    if (length(type) > 1 &
        length(type) < n) {
      type <-
        c(type, rep("", (n - length(type))))
    }

    # Trim vectors with number of values exceeding the
    # number of nodes
    if (length(type) > n) {
      type <- type[1:n]
    }
  }

  # Collect extra vectors of data as `extras`
  extras <- list(...)

  if (length(extras) > 0) {
    for (i in 1:length(extras)) {

      # Expand vectors with single values to fill to
      # the number of nodes
      if (length(extras[[i]]) == 1) {
        extras[[i]] <- rep(extras[[i]], n)
      }

      # Expand vectors with `length` > `1` and
      # `length` < `length(nodes)`
      if (length(extras[[i]]) > 1 &
          length(extras[[i]]) < n) {
        extras[[i]] <-
          c(extras[[i]],
            rep("", (n - length(extras[[i]]))))
      }

      # Trim vectors with number of values exceeding
      # the number of nodes
      if (length(extras[[i]]) > n) {
        extras[[i]] <- extras[[i]][1:n]
      }
    }
  }

  # Change logical for labels to empty labels
  if (inherits(label, "logical") &
      length(label) == 1) {
    if (label == TRUE) {
      label <- as.character(1:n)
    } else {
      label <- rep("", n)
    }
  }

  if (length(extras) > 0) {
    nodes_df <-
      data.frame(
        nodes = 1:n,
        type = type,
        label = label,
        extras,
        stringsAsFactors = FALSE)
  } else {
    nodes_df <-
      data.frame(
        nodes = 1:n,
        type = type,
        label = label,
        stringsAsFactors = FALSE)
  }

  return(nodes_df)
}
