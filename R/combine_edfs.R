#' Combine multiple edge data frames into a single edge data frame
#'
#' Combine several edge data frames in the style of \code{rbind}, except, it
#'   works regardless of the number and ordering of the columns.
#' @param ... two or more edge data frames, which contain edge IDs and
#'   associated attributes.
#' @return a combined edge data frame.
#' @examples
#' # Create an edge data frame (edf)
#' edf_1 <-
#'   create_edge_df(
#'     from = c(1, 1, 2, 3),
#'     to = c(2, 4, 4, 1),
#'     rel = "requires",
#'     color = "green",
#'     data = c(2.7, 8.9, 2.6, 0.6))
#'
#' # Create a second edge data frame
#' edf_2 <-
#'   create_edge_df(
#'     from = c(5, 7, 8, 8),
#'     to = c(7, 8, 6, 5),
#'     rel = "receives",
#'     arrowhead = "dot",
#'     color = "red")
#'
#' # Combine the two edge data frames
#' all_edges <-
#'   combine_edfs(edf_1, edf_2)
#'
#' # View the combined edge data frame
#' all_edges
#' @importFrom dplyr bind_rows mutate
#' @export
combine_edfs <- function(...) {

  data_frames <- list(...)

  for (l in 1:length(data_frames)) {
    if (l == 1) {
      df1 <- data_frames[l][[1]]
      df2 <- data_frames[l + 1][[1]]
    }
    if (l > 1 & l < length(data_frames)) {
      df1 <- edf_new
      df2 <- data_frames[l + 1][[1]]
    }

    # Bind rows from df1 and df2
    edf_new <- dplyr::bind_rows(df1, df2)

    if (l == length(data_frames)) {
      break
    }
  }

  edf_new <-
    edf_new %>%
    dplyr::mutate(id = as.integer(1:nrow(edf_new)))

  edf_new
}
