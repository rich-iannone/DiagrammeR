#' Create a node data frame
#'
#' Combine several vectors for nodes and their attributes into a data frame,
#'   which can be combined with other similarly-generated data frames, or, added
#'   to a graph object. A node data frame, or ndf, has at least the following
#'   columns:
#'
#' - \code{id} (of type \code{integer})
#'
#' - \code{type} (of type \code{character})
#'
#' - \code{label} (of type \code{character})
#'
#' An arbitrary number of additional columns containing aesthetic or data
#'   attributes can be part of the ndf, so long as they follow the
#'   aforementioned columns.
#' @param n the total number of nodes to include in the node data frame.
#' @param type an optional \code{type} for each node.
#' @param label an optional \code{label} for each node.
#' @param ... one or more vectors for associated node attributes.
#' @return a node data frame (ndf).
#' @examples
#' # Create a node data frame (ndf) where the labels
#' # are equivalent to the node ID values (this is not
#' # recommended); the `label` and `type` node
#' # attributes will always be a `character` class
#' # whereas `id` will always be an `integer`
#' node_df <-
#'   create_node_df(
#'     n = 4,
#'     type = c("a", "a", "b", "b"),
#'     label = TRUE)
#'
#' # Display the node data frame
#' node_df
#'
#' # Create an ndf with distinct labels and
#' # additional node attributes (where their classes
#' # will be inferred from the input vectors)
#' node_df <-
#'   create_node_df(
#'     n = 4,
#'     type = "a",
#'     label = c(2384, 3942, 8362, 2194),
#'     style = "filled",
#'     color = "aqua",
#'     shape = c("circle", "circle",
#'               "rectangle", "rectangle"),
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Display the node data frame
#' node_df
#' @importFrom dplyr bind_cols
#' @export
create_node_df <- function(n,
                           type = NULL,
                           label = NULL,
                           ...) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  if (!(inherits(n, "numeric") | inherits(n, "integer"))) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The value supplied to `n` must be numeric")
  }

  if (length(n) > 1) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The value supplied to `n` must be a single numeric value")
  }

  if (is.null(type)) {
    type <- rep(as.character(NA), n)
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
        c(type, rep(as.character(NA), (n - length(type))))
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

    # Create a data frame from the `extras` list
    extras <-
        as.data.frame(
          extras, stringsAsFactors = FALSE)
  }

  # Interpret node label values
  if (is.null(label)) {
    label <- rep(as.character(NA), n)
  } else if (inherits(label, "numeric") |
             inherits(label, "character")) {
    label <- as.character(label)
  } else if (inherits(label, "logical") &
             length(label) == 1) {
    if (label == TRUE) {
      label <- as.character(1:n)
    } else {
      label <- rep(as.character(NA), n)
    }
  }

  if (inherits(extras, "data.frame")) {
    nodes_df <-
      dplyr::bind_cols(
        data.frame(
          id = 1:n,
          type = type,
          label = label,
          stringsAsFactors = FALSE),
        extras)

  } else {
    nodes_df <-
      data.frame(
        id = 1:n,
        type = type,
        label = label,
        stringsAsFactors = FALSE)
  }

  nodes_df
}
