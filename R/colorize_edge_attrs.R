#' Apply colors based on edge attribute values
#'
#' Within a graph's internal edge data frame (edf), use a categorical edge
#'   attribute to generate a new edge attribute with color values.
#' @inheritParams render_graph
#' @param edge_attr_from the name of the edge attribute column from which
#'   color values will be based.
#' @param edge_attr_to the name of the new edge attribute to which the color
#'   values will be applied.
#' @param cut_points an optional vector of numerical breaks for bucketizing
#'   continuous numerical values available in a edge attribute column.
#' @param palette can either be: (1) a palette name from the RColorBrewer
#'   package (e.g., \code{Greens}, \code{OrRd}, \code{RdYlGn}), (2)
#'   \code{viridis}, which indicates use of the \code{viridis} color scale from
#'   the package of the same name, or (3) a vector of hexadecimal color names.
#' @param alpha an optional alpha transparency value to apply to the generated
#'   colors. Should be in the range of \code{0} (completely transparent) to
#'   \code{100} (completely opaque).
#' @param reverse_palette an option to reverse the order of colors in the chosen
#'   palette. The default is \code{FALSE}.
#' @param default_color a hexadecimal color value to use for instances when the
#'   values do not fall into the bucket ranges specified in the
#'   \code{cut_points} vector.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with 5
#' # nodes and 4 edges
#' graph <-
#'   create_graph() %>%
#'   add_path(n = 5) %>%
#'   set_edge_attrs(
#'     edge_attr = weight,
#'     values = c(3.7, 6.3, 9.2, 1.6))
#'
#' # We can bucketize values in
#' # the edge `weight` attribute using
#' # `cut_points` and, by doing so,
#' # assign colors to each of the
#' # bucketed ranges (for values not
#' # part of any bucket, a gray color
#' # is assigned by default)
#' graph <-
#'   graph %>%
#'   colorize_edge_attrs(
#'     edge_attr_from = weight,
#'     edge_attr_to = color,
#'     cut_points = c(0, 2, 4, 6, 8, 10),
#'     palette = "RdYlGn")
#'
#' # Now there will be a `color`
#' # edge attribute with distinct
#' # colors (from the RColorBrewer
#' # Red-Yellow-Green palette)
#' graph %>%
#'   get_edge_df()
#' @import RColorBrewer
#' @importFrom viridis viridis
#' @importFrom rlang enquo UQ get_expr
#' @export
colorize_edge_attrs <- function(graph,
                                edge_attr_from,
                                edge_attr_to,
                                cut_points = NULL,
                                palette = "Spectral",
                                alpha = NULL,
                                reverse_palette = FALSE,
                                default_color = "#D9D9D9") {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph object is not valid")
  }

  # Get the requested `edge_attr_from`
  edge_attr_from <-
    rlang::enquo(edge_attr_from) %>% rlang::get_expr() %>% as.character()

  # Get the requested `edge_attr_to`
  edge_attr_to <-
    rlang::enquo(edge_attr_to) %>% rlang::get_expr() %>% as.character()

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

  # If the number of recodings lower than any Color
  # Brewer palette, shift palette to `viridis`
  if ((num_recodings < 3 | num_recodings > 10) & palette %in%
      c(row.names(RColorBrewer::brewer.pal.info))) {
    palette <- "viridis"
  }

  # Stop function if color palette is not `viridis`
  # or any of the RColorBrewer palettes
  if (length(palette) == 1) {
    if (!(palette %in%
          c(row.names(RColorBrewer::brewer.pal.info),
            "viridis"))) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "The color palette is not an `RColorBrewer` or `viridis` palette")
    }
  }

  # Obtain a color palette
  if (length(palette) == 1) {
    if (palette %in%
        row.names(RColorBrewer::brewer.pal.info)) {
      color_palette <- RColorBrewer::brewer.pal(num_recodings, palette)
    } else if (palette == "viridis") {
      color_palette <- viridis::viridis(num_recodings)
      color_palette <- gsub("..$", "", color_palette)
    }
  }

  # Reverse color palette if `reverse_palette = TRUE`
  if (reverse_palette == TRUE) {
    color_palette <- rev(color_palette)
  }

  # Create a data frame with initial values
  new_edge_attr_col <-
    data.frame(
      edge_attr_to = rep(default_color, nrow(edges_df)),
      stringsAsFactors = FALSE)

  # Get the column number for the new edge attribute
  to_edge_attr_colnum <- ncol(edges_df) + 1

  # Bind the current edf with the new column
  edges_df <- cbind(edges_df, new_edge_attr_col)

  # Rename the new column with the target edge attr name
  colnames(edges_df)[to_edge_attr_colnum] <- edge_attr_to

  # Get a data frame of recodings
  if (is.null(cut_points)) {

    recode_df <-
      data.frame(
        to_recode = names(table(edges_df[, col_to_recode_no])),
        colors = color_palette,
        stringsAsFactors = FALSE)

    # Recode rows in the new edge attribute
    for (i in seq_along(names(table(edges_df[, col_to_recode_no])))) {

      recode_rows <-
        which(edges_df[, col_to_recode_no] %in%
                recode_df[i, 1])

      if (is.null(alpha)) {
        edges_df[recode_rows, to_edge_attr_colnum] <-
          color_palette[i]
      } else if (!is.null(alpha)) {
        if (alpha < 100) {
          edges_df[recode_rows, to_edge_attr_colnum] <-
            gsub("$", alpha, color_palette[i])
        } else if (alpha == 100) {
          edges_df[recode_rows, to_edge_attr_colnum] <-
            gsub("$", "", color_palette[i])
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
        color_palette[i]
    }

    if (!is.null(alpha)) {
      if (alpha < 100) {
        edges_df[, to_edge_attr_colnum] <-
          gsub("$", alpha, edges_df[, to_edge_attr_colnum])
      } else if (alpha == 100) {
        edges_df[, to_edge_attr_colnum] <-
          gsub("$", "", edges_df[, to_edge_attr_colnum])
      }
    }
  }

  # Get the finalized column of values
  edges_attr_vector_colorized <- edges_df[, ncol(edges_df)]

  edge_attr_to_2 <- rlang::enquo(edge_attr_to)

  # Set the edge attribute values for nodes specified
  # in selection
  graph <-
    set_edge_attrs(
      graph = graph,
      edge_attr = rlang::UQ(edge_attr_to_2),
      values = edges_attr_vector_colorized)

  # Remove last action from the `graph_log`
  graph$graph_log <- graph$graph_log[1:(nrow(graph$graph_log) - 1), ]

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = fcn_name,
      time_modified = time_function_start,
      duration = graph_function_duration(time_function_start),
      nodes = nrow(graph$nodes_df),
      edges = nrow(graph$edges_df))

  # Write graph backup if the option is set
  if (graph$graph_info$write_backups) {
    save_graph_as_rds(graph = graph)
  }

  graph
}
