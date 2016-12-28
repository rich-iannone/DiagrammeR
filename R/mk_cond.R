#' Helper for making conditions for some `select_` function
#' @description Create one or multiple conditions for
#' certain \code{select_...} functions that have a
#' \code{conditions} argument.
#' @param ... sets of 3 elements for each condition. Each
#' set of three elements is the: (1) node or edge attribute
#' name (character value), (2) the conditional operator
#' (character value), and (3) the non-attribute operand. If
#' there are multiple conditions to be specified, then a
#' \code{&} or \code{|} operator must be used between each
#' condition, specifying an \code{AND} or \code{OR}.
#' @return a string to be used for any \code{conditions}
#' argument.
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = "basic",
#'     label = TRUE,
#'     value = c(3.5, 2.6, 9.4, 2.7))
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = c("z", "z", "a"),
#'     value = c(6.4, 2.9, 5.0))
#'
#' # Create a graph with the ndf and edf
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Select edges where the `rel` label is `z` using
#' # the `mk_cond()` helper function
#' graph_1 <-
#'   graph %>%
#'   select_edges(
#'     conditions =
#'       mk_cond(
#'         "rel",  "==",  "z"))
#'
#' # Verify that an edge selection has been made; the
#' # edges corresponding to this condition are the
#' # `1->4` and 2->3` edges with edge IDs `1` and `2`
#' get_selection(graph_1)
#' #> [1] 1 2
#'
#' # Select edges based on the relationship label
#' # being `z` and the `value` being < 5.0
#' graph_2 <-
#'   graph %>%
#'   select_edges(
#'     mk_cond(
#'       "rel",  "==",  "z",
#'       "&",
#'       "value", "<", 5.0))
#'
#' # Verify that an edge selection has been made; the
#' # edge corresponding to these conditions is the
#' # `2->3` edge with ID `2`
#' get_selection(graph_2)
#' #> [1] 2
#'
#' # Because we are not specifying conditions as
#' # single strings, we can use objects from the
#' # workspace (or function calls) to compose the
#' # condition(s)
#'
#' # Create the `rel_select` character vector
#' rel_select <- "a"
#'
#' # Use a condition that gets the `rel` operand
#' # from an object, and, a `value` operand that
#' # is calculated from the mean of its values
#' # in the graph's edge data frame (~4.77)
#' graph_3 <-
#'   graph %>%
#'   select_edges(
#'     mk_cond(
#'       "rel",  "==", rel_select,
#'       "&",
#'       "value", ">", get_edge_attrs(., "value") %>%
#'                     mean()))
#'
#' # Verify that an edge selection has been made; the
#' # edge corresponding to these conditions is the
#' # `3->1` edge with ID `3`
#' get_selection(graph_3)
#' #> [1] 3
#' @export mk_cond

mk_cond <- function(...) {

  condition_parts <- list(...)

  # Each set of three elements is the:
  # 1. node/edge attribute name (chr)
  # 2. the conditional operator (chr)
  # 3. the non-attribute operand (chr or num)

  # The number of elements must be either 3 or
  # 3n + (n - 1) where n >= 2;

  if (length(condition_parts) != 3 &
      !(length(condition_parts) %in% ((3 * 2:200) + (2:200 - 1)))) {
    stop("The number of elements must be either 3 or 3n + (n - 1) where n >= 2.")
  }

  if (!inherits(condition_parts[[1]], "character")) {
    stop("The first of the three elements must be a character object.")
  }

  if (!inherits(condition_parts[[2]], "character") |
      !(trimws(condition_parts[[2]]) %in% c("==", ">", "<", ">=", "<=", "!="))) {
    stop("The second of the three elements must conditional operator.")
  }

  if (inherits(condition_parts[[3]], "character")) {
    condition_parts[[3]] <- paste0("'", as.character(condition_parts[[3]]), "'")
  }

  if (length(condition_parts) > 3) {

    length_parts <- length(condition_parts)

    index <- which((3 * 2:200) + (2:200 - 1) == length_parts)

    for (i in 1:index) {
      if (!inherits(condition_parts[[((3 * 2:200) + (2:200 - 1) - 3)[i]]], "character") |
          !(condition_parts[[((3 * 2:200) + (2:200 - 1) - 3)[i]]] %in%
            c("&", "|"))) {
        stop("The separator between each set of three elements must either be `&` or `|`.")
      }
    }

    for (i in 1:index) {
      if (!inherits(condition_parts[[((3 * 2:200) + (2:200 - 1) - 2)[i]]], "character")) {
        stop("The first of the three elements must be a character object.")
      }
    }

    for (i in 1:index) {
      if (!inherits(condition_parts[[((3 * 2:200) + (2:200 - 1) - 1)[i]]], "character") |
          !(trimws(condition_parts[[((3 * 2:200) + (2:200 - 1) - 1)[i]]]) %in%
            c("==", ">", "<", ">=", "<=", "!="))) {
        stop("The second of the three elements must conditional operator.")
      }
    }

    for (i in 1:index) {
      if (inherits(condition_parts[[((3 * 2:200) + (2:200 - 1))[i]]], "character")) {
        condition_parts[[((3 * 2:200) + (2:200 - 1))[i]]] <-
          paste0("'", as.character(condition_parts[[((3 * 2:200) + (2:200 - 1))[i]]]), "'")
      }
    }
  }

  # Collapse all parts into a valid expression
  condition <- paste(condition_parts, collapse = " ")

  return(condition)
}

five <- 5

mk_cond("value", "==", five)


