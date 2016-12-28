#' Helper for making conditions for some `select_` function
#' @description Create one or multiple conditions for
#' certain \code{select_...} functions that have a
#' \code{conditions} argument.
#' @param ... sets of 3 elements for each conditions.
#' @return a string to be used for any \code{conditions}
#' argument.
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


