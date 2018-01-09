#' Layout nodes using a text-based schematic
#' @description Layout one or several groups of
#' nodes using a text-based schematic. The option
#' is available to apply sorting to each of the
#' groups.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @param layout a layout character string that
#' provides a schematic for the layout. This
#' consists of a rectangular collection of
#' \code{-} characters (for no node placement),
#' and numbers from \code{1} to \code{9}
#' (representing different groupings of nodes,
#' further described in the \code{nodes} argument).
#' @param nodes a named vector of the form:
#' \code{c("1" = "[node_attr]:[value]", ...)}. The
#' LHS corresponds to the numbers used in the
#' \code{layout} schematic. The RHS provides a
#' shorthand for the node attribute and a value
#' for grouping together nodes (separated by a
#' colon). For instance, with \code{"type:a"} in the
#' RHS (and \code{"1"} in the LHS) we would target
#' all nodes with a \code{type} attribute equal
#' to \code{a} for positioning in the graph as
#' described by the \code{1}s in the \code{layout}.
#' @param sort an optional sorting method to apply
#' to the collection of nodes before assigning
#' positional information. Like \code{nodes}, this
#' is a named vector of the form:
#' \code{c("1" = "[node_attr]:asc|desc", ...)}.
#' The \code{node_attr} in this case should be
#' different than that used in \code{nodes}. Ideally,
#' this node attribute should have unique values.
#' Choose either \code{asc} or \code{desc} right of
#' the colon for ascending or descending sorts.
#' @param width the width of the \code{layout}
#' diagram.
#' @param height the height of the \code{layout}
#' diagram.
#' @param ll a vector describing the the lower-left
#' coordinates of the \code{layout}
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create a graph with unique labels and
#' # several node `type` groupings
#' graph <-
#'   create_graph() %>%
#'   add_node(type = "a", label = "a") %>%
#'   add_node(type = "a", label = "b") %>%
#'   add_node(type = "b", label = "c") %>%
#'   add_node(type = "b", label = "d") %>%
#'   add_node(type = "b", label = "e") %>%
#'   add_node(type = "c", label = "f") %>%
#'   add_node(type = "c", label = "g")
#'
#' # Define a 'layout' for groups of nodes
#' # using a text string (dashes are empty
#' # grid cells, numbers--representing
#' # ad-hoc groupings--correspond to
#' # individual nodes); here, define a layout
#' # with 3 groups of nodes
#' layout <-
#' "
#' 1--------
#' 1--------
#' ---222---
#' --------3
#' --------3
#' "
#'
#' # Use the `layout` along with what nodes
#' # the numbers correspond to in the graph
#' # with the `nodes` named vectors; the
#' # optional `sort` vector describes how
#' # we should sort the collection of node
#' # before adding position information
#' graph <-
#'   graph %>%
#'   layout_nodes_w_string(
#'     layout = layout,
#'     nodes = c("1" = "type:a",
#'               "2" = "type:b",
#'               "3" = "type:c"),
#'     sort = c("1" = "label:asc",
#'              "2" = "label:desc",
#'              "3" = "label:desc"))
#'
#' # Show the graph's node data frame
#' # to confirm that `x` and `y` values
#' # were added to each of the nodes
#' get_node_df(graph)
#' #>   id type label x y
#' #> 1  1    a     a 0 8
#' #> 2  2    a     b 0 6
#' #> 3  3    b     c 5 4
#' #> 4  4    b     d 4 4
#' #> 5  5    b     e 3 4
#' #> 6  6    c     f 8 0
#' #> 7  7    c     g 8 2
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows bind_cols filter_ arrange_ left_join
#' @export layout_nodes_w_string

layout_nodes_w_string <- function(graph,
                                  layout,
                                  nodes,
                                  sort = NULL,
                                  width = 8,
                                  height = 8,
                                  ll = c(0, 0)) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    stop(
      "The graph object is not valid.",
      call. = FALSE)
  }

  # Get the graph's internal node data frame
  # (ndf) into a separate object
  ndf <- graph$nodes_df

  # Get the total number of groups described
  # in the layout
  node_group_count <- length(nodes)

  # Parse the `layout` object to get a vector
  # of rows
  layout <-
    gsub(" ", "", layout) %>%
    stringr::str_split("\n") %>%
    unlist()

  layout <-
    layout[which(layout != "")]

  # Determine the row length from the layout text
  layout_row_length <- vector(mode = "numeric")

  for (i in 1:length(layout)) {
    layout_row_length <-
      c(layout_row_length, nchar(layout[i]))
  }

  # Stop function if not all rows are of equal length
  if (mean(layout_row_length) != layout_row_length[1]) {

    stop(
      "Each row must have the same length.",
      call. = FALSE)
  }

  layout_column_number <- layout_row_length <- layout_row_length[1]
  layout_row_number <- layout_column_length <- length(layout)

  # Define the exact `x` and `y` positions in which
  # the nodes could be placed
  x_pts <- seq(0, width, width/(layout_column_number - 1)) + ll[1]
  y_pts <- rev(seq(0, height, height/(layout_row_number - 1))) + ll[2]

  # Create tibble called `ndf_parts`
  ndf_parts <- tibble::tibble()

  for (k in 1:node_group_count) {

    # Create table with position and node ID
    position_table <-
      tibble::tibble(
        x = as.numeric(NA),
        y = as.numeric(NA))

    position_table <- position_table[-1, ]

    node_group <- names(nodes)[k]
    node_select <- nodes[[k]]

    node_attr <- unlist(stringr::str_split(node_select, ":"))[1]
    node_attr_val <- unlist(stringr::str_split(node_select, ":"))[2]

    sort_attr <- unlist(stringr::str_split(sort[k], ":"))[1]
    sort_dir <- unlist(stringr::str_split(sort[k], ":"))[2]

    for (i in 1:layout_row_number) {
      for (j in 1:layout_column_number) {

        if (unlist(stringr::str_split(layout[i], ""))[j] == k) {

          item_table <-
            tibble::tibble(
              x = x_pts[j],
              y = y_pts[i])

          position_table <-
            position_table %>%
            dplyr::bind_rows(item_table)
        }
      }
    }

    # Filter the graph `ndf`
    ndf_part <-
      ndf %>%
      dplyr::filter_(paste0(node_attr, " == '", node_attr_val, "'"))

    # Optionally apply sorting
    if (!is.null(sort)) {
      if (sort_dir == "desc") {
        ndf_part <-
          ndf_part %>%
          dplyr::arrange_(paste0("desc(", sort_attr, ")"))

      } else {
        ndf_part <-
          ndf_part %>%
          dplyr::arrange_(sort_attr)
      }
    }

    # Trim rows from the position table if its
    # row count is greater than that of `ndf_part`
    if (nrow(position_table) > nrow(ndf_part)) {
      position_table <- position_table[1:nrow(ndf_part), ]
    }

    # Bind `position_table` to the split `ndf_part`
    ndf_part <-
      dplyr::bind_cols(position_table, ndf_part)

    # Bind `ndf_part` to `ndf_parts`
    ndf_parts <-
      dplyr::bind_rows(ndf_parts, ndf_part)
  }

  # Join the `ndf_parts` to the main `ndf`
  ndf <-
    ndf %>%
    dplyr::left_join(
      ndf_parts,
      by = c("id", "type", "label"))

  # Replace the graph's ndf with the revised version
  graph$nodes_df <- ndf

  # Update the `graph_log` df with an action
  graph$graph_log <-
    add_action_to_log(
      graph_log = graph$graph_log,
      version_id = nrow(graph$graph_log) + 1,
      function_used = "layout_nodes_w_string",
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
