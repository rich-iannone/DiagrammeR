#' Combine multiple node data frames
#'
#' @description
#'
#' Combine several node data frames into a single node data frame.
#'
#' @param ... Two or more node data frames, which contain node IDs and
#'   associated attributes.
#'
#' @return A combined node data frame.
#'
#' @examples
#' # Create two node data frames
#' node_df_1 <-
#'   create_node_df(
#'     n = 2,
#'     type = c("a", "b"),
#'     label = c("D", "Z"),
#'     value = c(8.4, 3.4))
#'
#' node_df_2 <-
#'   create_node_df(
#'     n = 2,
#'     type = c("b", "c"),
#'     label = c("U", "A"),
#'     value = c(0.4, 3.4))
#'
#' # Combine the ndfs using the
#' # `combine_ndfs()` function
#' node_df_combined <-
#'   combine_ndfs(
#'     node_df_1,
#'     node_df_2)
#'
#' # Inspect the combined ndf
#' node_df_combined
#'
#' @export
combine_ndfs <- function(...) {

  ndfs <- list(...)

  for (l in seq_along(ndfs)) {
    if (l == 1) {
      df1 <- ndfs[l][[1]]
      df2 <- ndfs[l + 1][[1]]
    }

    if (l > 1 && l < length(ndfs)) {
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
  ndf_new[, 1] <- seq_len(nrow(ndf_new))

  ndf_new
}
