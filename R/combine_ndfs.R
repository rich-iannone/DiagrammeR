#' Combine multiple node data frames
#' @description Combine several node data frames
#' into a single node data frame.
#' @param ... two or more node data frames, which
#' contain node IDs and associated attributes.
#' @return a combined node data frame.
#' @importFrom dplyr bind_rows
#' @examples
#' # Create two node data frames
#' node_df_1 <-
#'   create_node_df(
#'     n = 4,
#'     type = c("a", "a", "b", "b"),
#'     label = c("D", "Z", "E", "G"),
#'     value = c(8.4, 3.4, 2.9, 7.0))
#'
#' node_df_2 <-
#'   create_node_df(
#'     n = 2,
#'     type = c("b", "c"),
#'     label = c("U", "A"),
#'     value = c(0.4, 3.4))
#'
#' # Combined the node data frames using the
#' # `combine_ndfs()` function
#' node_df_combined <-
#'   combine_ndfs(
#'     node_df_1,
#'     node_df_2)
#'
#' # Inspect the combined node data frame
#' node_df_combined
#' @export combine_ndfs

combine_ndfs <- function(...) {

  ndfs <- list(...)

  for (l in 1:length(ndfs)) {
    if (l == 1) {
      df1 <- ndfs[l][[1]]
      df2 <- ndfs[l + 1][[1]]
    }

    if (l > 1 & l < length(ndfs)) {
      df1 <- ndf_new
      df2 <- ndfs[l + 1][[1]]
    }

    # Bind rows from df1 and df2
    ndf_new <- dplyr::bind_rows(df1, df2)

    if (l == length(ndfs)) {
      break
    }
  }

  # Create montonically-increasing integer id
  # values for new table based on input order
  ndf_new[, 1] <- as.integer(1:nrow(ndf_new))

  ndf_new
}
