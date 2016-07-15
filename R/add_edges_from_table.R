#' Add edges and attributes to graph from a table
#' @description Add edges and their attributes to an
#' existing graph object from data in a CSV file or a
#' data frame.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param table either a path to a CSV file, or, a data
#' frame object.
#' @param from_col the name of the table column from
#' which edges originate.
#' @param from_mapping a single character value for
#' the mapping of a column in the external table
#' (supplied as \code{from_col}) to a column in the
#' graph's internal node data frame (ndf).
#' @param to_col the name of the table column to
#' which edges terminate.
#' @param to_mapping a single character value for
#' the mapping of a column in the external table
#' (supplied as \code{to_col}) to a column in the
#' graph's internal node data frame (ndf).
#' @param set_rel an optional string to apply a
#' \code{rel} attribute to all edges created from the
#' table records.
#' @param select_cols an optional character vector for
#' specifying which columns in the table that should be
#' imported as edge attributes.
#' @param drop_cols an optional character vector for
#' dropping columns from the incoming data.
#' @param rename_attrs an optional character vector for
#' renaming edge attributes.
#' @param rel_col an option to apply a column of data
#' in the table as \code{rel} attribute values.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' library(magrittr)
#' library(dplyr)
#'
#' # Create a graph from a CSV file
#' graph <- create_graph() %>%
#' add_edges_from_table(
#'   system.file("examples/projects_and_contributors.csv",
#'               package = "DiagrammeR"),
#'   from_col = "contributor_name",
#'   to_col = "project_name",
#'   rel_col = "contributor_role",
#'   set_rel = "contributes_to")
#'
#' # Get a count of nodes in the graph
#' node_count(graph)
#' #> [1] 13
#'
#' # Get a count of edges in the graph
#' edge_count(graph)
#' #> [1] 13
#' }
#' @export add_edges_from_table

add_edges_from_table <- function(graph,
                                 table,
                                 from_col,
                                 from_mapping = NULL,
                                 to_col,
                                 to_mapping = NULL,
                                 set_rel = NULL,
                                 select_cols = NULL,
                                 drop_cols = NULL,
                                 rename_attrs = NULL,
                                 rel_col = NULL) {

  if (inherits(table, "character")) {
    # Load in CSV file
    csv <- read.csv(table, stringsAsFactors = FALSE)
  } else if (inherits(table, "data.frame")) {
    # Rename `table` object as `csv`
    csv <- table
  }

  # Get numbers of rows and columns in the table
  rows_in_csv <- nrow(csv)
  cols_in_csv <- ncol(csv)

  # Get rownames for existing edges in graph object
  edges_existing_rownames <-
    rownames(get_edge_df(graph))

  # Verify that value for `from_col` is in the table
  if (!(from_col %in% colnames(csv))) {
    stop("The value specified in `from_col` is not in the table.")
  }

  # Verify that value for `to_col` is in the table
  if (!(to_col %in% colnames(csv))) {
    stop("The value specified in `to_col` is not in the table.")
  }

  # Verify that value for `from_mapping` is in the
  # graph's ndf
  if (!is.null(from_mapping)) {
    if (!(from_mapping %in% colnames(get_node_df(graph)))) {
      stop("The value specified in `from_mapping` is not in the graph.")
    }
  }

  # Verify that value for `to_mapping` is in the
  # graph's ndf
  if (!is.null(to_mapping)) {
    if (!(to_mapping %in% colnames(get_node_df(graph)))) {
      stop("The value specified in `to_mapping` is not in the graph.")
    }
  }

  if (is.null(from_mapping) & is.null(to_mapping)) {

    if (node_count(graph) == 0) {
      starting_node <- 1
    } else {
      if (suppressWarnings(
        any(!(is.na(
          as.numeric(graph$nodes_df$nodes)))))) {
        starting_node <-
          suppressWarnings(
            max(
              as.numeric(
                graph$nodes_df[
                  which(!is.na(
                    as.numeric(graph$nodes_df$nodes))),
                  1])) + 1)
      } else {
        starting_node <- 1
      }
    }

    # If values for `select_cols` are provided, filter
    # the CSV columns by those named columns
    if (!is.null(select_cols)) {

      # If none of the specified values in `select_cols`
      # are in the CSV, stop the function
      if (all(select_cols %in% colnames(csv)) == FALSE) {
        stop("None of the values specified for selecting columns are available.")
      }

      columns_retained <-
        which(colnames(csv) %in% select_cols)

      csv <- csv[,columns_retained]
    }

    # If values for `drop_cols` provided, filter the CSV
    # columns by those named columns
    if (is.null(select_cols) & !is.null(drop_cols)) {

      columns_retained <-
        which(!(colnames(csv) %in% drop_cols))

      csv <- csv[,columns_retained]
    }

    # If values for `rename_attrs` provided, rename all
    # of the CSV columns by those replacement values
    # (number of new names should match number of columns
    # even after selecting or dropping columns)
    if (!is.null(rename_attrs)) {
      if (length(rename_attrs) != length(colnames(csv))) {
        stop(paste0("The number of values specified for column name changes ",
                    "does not match the number of columns available"))
      }
      colnames(csv) <- rename_attrs
    }

    # Optionally set the `rel` attribute from a
    # specified column in the CSV (this copies into
    # the `rel` column)
    if (!is.null(rel_col)) {
      if (any(colnames(csv) == rel_col)) {
        csv$rel <- csv[,which(colnames(csv) == rel_col)]
      }
    }

    # Get the unique set of nodes to add to the graph
    nodes <-
      create_nodes(
        nodes = unique(
          c(csv[, which(colnames(csv) %in% from_col)],
            csv[, which(colnames(csv) %in% to_col)])))

    # Add node data frame to the graph
    graph <- add_node_df(graph, nodes)

    # Create an edge data frame
    edges <-
      create_edges(
        from = csv[, which(colnames(csv) %in% from_col)],
        to = csv[, which(colnames(csv) %in% to_col)]
      )

    # Add edge data frame to the graph
    graph <- add_edge_df(graph, edges)

    return(graph)
  }

  # Verify that all values in `from_col` in the table are
  # available in the graph
  if (!(all(
    csv[,which(colnames(csv) == from_col)] %in%
    get_node_df(graph)[,
                       which(colnames(get_node_df(graph)) == from_mapping)]))) {
    stop(paste0("The `from` values in the table don't all match the requested",
                "node attribute value in the graph."))
  }

  # Verify that all values in `to_col` in the table are
  # available in the graph
  if (!(all(csv[,which(colnames(csv) == to_col)] %in%
            get_node_df(graph)[,which(colnames(get_node_df(graph)) == to_mapping)]))) {
    stop(paste0("The `to` values in the table don't all match the requested",
                "node attribute values in the graph."))
  }

  # If values for `select_cols` provided, filter the
  # table columns by those named columns
  if (!is.null(select_cols)) {

    # If none of the specified values in `select_cols`
    # are in the table, stop the function
    if (all(select_cols %in% colnames(csv)) == FALSE) {
      stop("None of the values specified for selecting columns are available.")
    }
    columns_retained <- which(colnames(csv) %in% select_cols)
    csv <- csv[,columns_retained]
  }

  # If values for `drop_cols` provided, filter the
  # table columns by those named columns
  if (is.null(select_cols) & !is.null(drop_cols)) {
    columns_retained <-
      which(!(colnames(csv) %in% drop_cols))
    csv <- csv[,columns_retained]
  }

  # If values for `rename_attrs` provided, rename the
  # table columns by those replacement values
  if (!is.null(rename_attrs)) {
    if (length(rename_attrs) !=
        length(colnames(csv))) {
      stop(paste0("The number of values specified for column name changes ",
                  "does not match the number of columns available"))
    }
    colnames(csv) <- rename_attrs
  }

  # Get relevant column numbers from the table
  from_col_value <- which(colnames(csv) == from_col)
  to_col_value <- which(colnames(csv) == to_col)

  # Get relevant column numbers from the graph's ndf
  from_mapping_value <-
    which(colnames(get_node_df(graph)) == from_mapping)

  to_mapping_value <-
    which(colnames(get_node_df(graph)) == to_mapping)

  # Create edges
  for (i in 1:rows_in_csv) {
    graph <-
      add_edge(
        graph = graph,
        from = get_node_df(graph)[
          which(get_node_df(graph)[
            ,from_mapping_value] ==
              csv[i, from_col_value]), 1],
        to = get_node_df(graph)[
          which(get_node_df(graph)[
            ,to_mapping_value] ==
              csv[i, to_col_value]), 1])
  }

  # Get rownames for edges created
  edges_created_rownames <-
    as.numeric(
      setdiff(
        rownames(
          get_edge_df(graph)),
        edges_existing_rownames))

  # Get column numbers in table that are edge attributes
  if (!is.null(rel_col)) {
    edge_attr_cols_csv <-
      which(colnames(csv) %in%
              setdiff(colnames(csv),
                      c(from_col, to_col, rel_col)))
  } else {
    edge_attr_cols_csv <-
      which(colnames(csv) %in%
              setdiff(colnames(csv),
                      c(from_col, to_col)))
  }

  # Add table columns as attributes
  for (i in edges_created_rownames) {
    for (j in edge_attr_cols_csv) {
      graph <-
        set_edge_attrs(
          x = graph,
          from = get_edge_df(graph)[
            which(
              rownames(get_edge_df(graph)) == i), 1],
          to = get_edge_df(graph)[
            which(
              rownames(get_edge_df(graph)) == i), 2],
          edge_attr = colnames(csv)[j],
          values = csv[i,j])
    }

    # Optionally set the `rel` attribute from a
    # specified column in the table
    if (!is.null(rel_col)) {
      graph <-
        set_edge_attrs(
          x = graph,
          from = get_edge_df(graph)[
            which(
              rownames(get_edge_df(graph)) == i),1],
          to = get_edge_df(graph)[
            which(
              rownames(get_edge_df(graph)) == i),2],
          edge_attr = "rel",
          values = csv[i, which(colnames(csv) %in%
                                  rel_col)])
    }
  }

  # Optionally set the `rel` attribute with a single
  # value repeated down
  if (!is.null(set_rel)) {
    graph <-
      select_edges(
        graph = graph,
        from = get_edge_df(graph)[
          edges_created_rownames, 1],
        to = get_edge_df(graph)[
          edges_created_rownames, 2])

    graph <-
      set_edge_attrs_ws(
        graph = graph,
        edge_attr = "rel",
        value = set_rel)

    graph <- clear_selection(graph = graph)
  }

  return(graph)
}
