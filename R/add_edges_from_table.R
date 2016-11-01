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
#' @param to_col the name of the table column to
#' which edges terminate.
#' @param ndf_mapping a single character value for
#' the mapping of the \code{from} and \code{to} columns
#' in the external table (supplied as \code{from_col}
#' and \code{to_col}, respectively) to a column in the
#' graph's internal node data frame (ndf).
#' @param set_rel an optional string to apply a
#' \code{rel} attribute to all edges created from the
#' table records.
#' @param select_cols an optional character vector for
#' specifying which columns in the table that should be
#' imported as edge attributes.
#' @param drop_cols an optional character vector for
#' dropping columns from the incoming data.
#' @param rel_col an option to apply a column of data
#' in the table as \code{rel} attribute values.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' # Create a graph from a CSV file
#' graph <-
#'   create_graph() %>%
#'   add_edges_from_table(
#'     system.file("examples/projects_and_contributors.csv",
#'                 package = "DiagrammeR"),
#'     from_col = "contributor_name",
#'     to_col = "project_name",
#'     rel_col = "contributor_role",
#'     set_rel = "contributes_to")
#'
#' # Get a summary of the new graph
#' graph_info(graph)
#' #>   name  n  e      dens min_deg max_deg avg_deg time tz
#' #> 1      13 13 0.1666667       1       7       2
#' }
#' @importFrom utils read.csv
#' @export add_edges_from_table

add_edges_from_table <- function(graph,
                                 table,
                                 from_col,
                                 to_col,
                                 ndf_mapping,
                                 set_rel = NULL,
                                 select_cols = NULL,
                                 drop_cols = NULL,
                                 rel_col = NULL) {

  # Get the number of nodes ever created for
  # this graph
  nodes_created <- graph$last_node

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

    csv <- csv[, columns_retained]
  }

  # If values for `drop_cols` provided, filter the CSV
  # columns by those named columns
  if (is.null(select_cols) & !is.null(drop_cols)) {

    columns_retained <-
      which(!(colnames(csv) %in% drop_cols))

    csv <- csv[, columns_retained]
  }

  # Optionally set the `rel` attribute from a
  # specified column in the CSV (this copies into
  # the `rel` column)
  if (!is.null(rel_col)) {
    if (any(colnames(csv) == rel_col)) {

      csv$rel <- csv[, which(colnames(csv) == rel_col)]
    }
  }

  if (is.null(from_mapping) & is.null(to_mapping)) {

    # Get the unique set of nodes to add to the graph
    ndf <-
      create_node_df(
        n = length(unique(
          c(csv[, which(colnames(csv) %in% from_col)],
            csv[, which(colnames(csv) %in% to_col)]))),
        nodes = unique(
          c(csv[, which(colnames(csv) %in% from_col)],
            csv[, which(colnames(csv) %in% to_col)])))

    # Add node data frame to the graph
    graph <- add_node_df(graph, nodes)

    # Create an edge data frame
    edges <-
      create_edge_df(
        from = csv[, which(colnames(csv) %in% from_col)],
        to = csv[, which(colnames(csv) %in% to_col)])

    # Add edge data frame to the graph
    graph <- add_edge_df(graph, edges)

    # Update the `last_node` counter
    graph$last_node <- nodes_created

    return(graph)
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
    csv <- csv[, columns_retained]
  }

  # If values for `drop_cols` provided, filter the
  # table columns by those named columns
  if (is.null(select_cols) & !is.null(drop_cols)) {
    columns_retained <-
      which(!(colnames(csv) %in% drop_cols))
    csv <- csv[, columns_retained]
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

  # Update the `last_node` counter
  graph$last_node <- nodes_created

  return(graph)
}
