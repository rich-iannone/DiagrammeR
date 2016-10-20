#' Apply colors based on edge attribute values
#' @description Within a graph's internal edge data
#' frame (edf), use a categorical edge attribute to
#' generate a new edge attribute with color values.
#' @param graph a graph object of class
#' @param edge_attr_from the name of the edge attribute
#' column from which color values will be based.
#' @param edge_attr_to the name of the new edge
#' attribute to which the color values will be applied.
#' @param cut_points an optional vector of numerical
#' breaks for bucketizing continuous numerical values
#' available in a edge attribute column.
#' @param alpha an optional alpha transparency value to
#' apply to the generated colors. Should be in
#' the range of \code{0} (completely transparent) to
#' \code{100} (completely opaque).
#' @param default_color a hexadecimal color value to
#' use for instances when the values do not fall into
#' the bucket ranges specified in the \code{cut_points}
#' vector.
#' @return a graph object of class
#' \code{dgr_graph}.
#' @examples
#' # Create a random graph of 50 nodes and 85 edges
#' graph <-
#'   create_random_graph(
#'     50, 85, set_seed = 1) %>%
#'   set_edge_attrs(
#'     "weight", rnorm(edge_count(.), 5, 2))
#'
#' # We can bucketize values in the edge `weight`
#' # attribute using `cut_points` and, by doing so,
#' # assign colors to each of the bucketed ranges
#' # (for values not part of any bucket, a gray color
#' # is assigned by default)
#' graph <-
#'   graph %>%
#'   colorize_edge_attrs(
#'     "weight", "color",
#'     cut_points = c(0, 2, 4, 6, 8, 10))
#'
#' # Now there will be a `color` edge attribute
#' # with distinct colors
#' get_edge_df(graph)
#' #>    from to rel           weight   color
#' #> 1    24 44     4.58523851279607 #21908C
#' #> 2    22 12     4.21438414111603 #21908C
#' #> 3     4  6     4.36001426290299 #21908C
#' #> 4    16 27     4.44177339404688 #21908C
#' #> 5    34 20     5.98837666253565 #21908C
#' #> 6    46 15     4.64533903546079 #21908C
#' #> 7    23 17     3.98808507577148 #3B528B
#' #> 8    33 13     7.68607765034082 #5DC863
#' #> 9    24 39     4.57084118290626 #21908C
#' #> 10    5 44     4.64088693991323 #21908C
#' @import viridis
#' @export colorize_edge_attrs

colorize_edge_attrs <- function(graph,
                                edge_attr_from,
                                edge_attr_to,
                                cut_points = NULL,
                                alpha = NULL,
                                default_color = "#D9D9D9") {

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

  # Extract edf from graph
  edges_df <- graph$edges_df

  # Get the column number in the edf from which to
  # recode values
  col_to_recode_no <-
    which(colnames(edges_df) %in% edge_attr_from)

  # Get the number of recoded values
  if (is.null(cut_points)) {
    num_recodings <-
      nrow(unique(edges_df[col_to_recode_no]))
  } else if (!is.null(cut_points)) {
    num_recodings <- length(cut_points) - 1
  }

  # Create a data frame which initial values
  new_edge_attr_col <-
    data.frame(
      edge_attr_to = rep(default_color, nrow(edges_df)),
      stringsAsFactors = FALSE)

  # Get the column number for the new edge attribute
  to_edge_attr_colnum <- ncol(edges_df) + 1

  # Bind the current edf with the new column
  edges_df <- cbind(edges_df, new_edge_attr_col)

  # Rename the new column with the target edge attr name
  colnames(edges_df)[to_edge_attr_colnum] <-
    edge_attr_to

  # Get a data frame of recodings
  if (is.null(cut_points)) {
    viridis_df <-
      data.frame(
        to_recode = names(table(edges_df[, col_to_recode_no])),
        colors = gsub("..$", "", viridis(num_recodings)),
        stringsAsFactors = FALSE)

    # Recode rows in the new edge attribute
    for (i in seq_along(names(table(edges_df[, col_to_recode_no])))) {

      recode_rows <-
        which(edges_df[, col_to_recode_no] %in%
                viridis_df[i, 1])

      if (is.null(alpha)) {
        edges_df[recode_rows, to_edge_attr_colnum] <-
          gsub("..$", "", viridis(num_recodings)[i])
      } else if (!is.null(alpha)) {
        if (alpha < 100) {
          edges_df[recode_rows, to_edge_attr_colnum] <-
            gsub("..$", alpha, viridis(num_recodings)[i])
        } else if (alpha == 100) {
          edges_df[recode_rows, to_edge_attr_colnum] <-
            gsub("..$", "", viridis(num_recodings)[i])
        }
      }
    }
  }

  # Recode according to provided cut points
  if (!is.null(cut_points)) {
    for (i in 1:(length(cut_points) - 1)) {
      recode_rows <-
        which(
          as.numeric(edges_df[, col_to_recode_no]) >=
            cut_points[i] &
            as.numeric(edges_df[, col_to_recode_no]) <
            cut_points[i + 1])

      edges_df[recode_rows, to_edge_attr_colnum] <-
        gsub("..$", "", viridis(num_recodings)[i])
    }

    if (!is.null(alpha)) {
      if (alpha < 100) {
        edges_df[, to_edge_attr_colnum] <-
          gsub("..$", alpha,edges_df[, to_edge_attr_colnum])
      } else if (alpha == 100) {
        edges_df[, to_edge_attr_colnum] <-
          gsub("..$", "", edges_df[, to_edge_attr_colnum])
      }
    }
  }

  # Modify the graph
  graph$edges_df = edges_df

  return(graph)
}
