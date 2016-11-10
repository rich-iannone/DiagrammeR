###
# Graph validation functions
###

# Function to check whether a graph object is valid
graph_object_valid <- function(graph) {

  # Check the object class for `dgr_graph`
  if (!inherits(graph, "dgr_graph")) {
    return(FALSE)
  }

  # Check for all component names to be present
  if (!all(c("graph_name", "graph_time", "graph_tz",
             "nodes_df", "edges_df", "global_attrs",
             "directed", "last_node") %in%
           names(graph))) {
    return(FALSE)
  }

  # Check for specific graph classes
  if (any(
    inherits(graph$global_attrs, "data.frame") == FALSE,
    inherits(graph$directed, "logical") == FALSE,
    inherits(graph$global_attrs$attr, "character") == FALSE,
    inherits(graph$global_attrs$value, "character") == FALSE,
    inherits(graph$global_attrs$attr_type, "character") == FALSE)) {
    return(FALSE)
  }

  return(TRUE)
}

# Function to check whether a graph contains any nodes
graph_contains_nodes <- function(graph) {

  if (is.null(graph$nodes_df)) {
    return(FALSE)
  }

  if (node_count(graph) == 0) {
    return(FALSE)
  }

  return(TRUE)
}

# Function to check whether a graph contains any edges
graph_contains_edges <- function(graph) {

  if (is.null(graph$edges_df)) {
    return(FALSE)
  }

  if (edge_count(graph) == 0) {
    return(FALSE)
  }

  return(TRUE)
}

# Function to check whether a graph contains a valid edge selection
graph_contains_edge_selection <- function(graph) {

  # Check if graph contains any selection
  if (is.null(graph$selection)) {
    return(FALSE)
  }

  # Check if graph contains edge selection
  if (is.null(graph$selection$edges)) {
    return(FALSE)
  }

  # Check if graph contains `from` and `to` integer vectors
  if (is.null(graph$selection$edges$from) |
      is.null(graph$selection$edges$to)) {
    return(FALSE)
  }

  # Check if the `from` and `to` vectors have length > 0,
  # and are of equal length
  if (length(graph$selection$edges$from) == 0 |
      length(graph$selection$edges$to) == 0 |
      length(graph$selection$edges$from) !=
      length(graph$selection$edges$to)) {
    return(FALSE)
  }

  return(TRUE)
}

# Function to check whether a graph contains a valid edge selection
graph_contains_edge_selection <- function(graph) {

  # Check if graph contains any selection
  if (is.null(graph$selection)) {
    return(FALSE)
  }

  # Check if graph contains an edge selection
  if (is.null(graph$selection$edges)) {
    return(FALSE)
  }

  # Check if graph contains `from` and `to` vectors
  if (is.null(graph$selection$edges$from) |
      is.null(graph$selection$edges$to)) {
    return(FALSE)
  }

  # Check if the `from` and `to` vectors have length > 0,
  # and are of equal length
  if (length(graph$selection$edges$from) == 0 |
      length(graph$selection$edges$to) == 0 |
      length(graph$selection$edges$from) !=
      length(graph$selection$edges$to)) {
    return(FALSE)
  }

  return(TRUE)
}

# Function to check whether a graph contains a valid node selection
graph_contains_node_selection <- function(graph) {

  # Check if graph contains any selection
  if (is.null(graph$selection)) {
    return(FALSE)
  }

  # Check if graph contains a node selection
  if (is.null(graph$selection$nodes)) {
    return(FALSE)
  }

  # Check if the `nodes` vector has length > 0
  if (length(graph$selection$nodes) == 0) {
    return(FALSE)
  }

  return(TRUE)
}

# Function to determine whether a node or edge
# attribute has values that are all non-NA and
# are unique
#' @importFrom dplyr select_ distinct
#' @importFrom magrittr not
is_attr_unique_and_non_na <- function(graph,
                                      which_graph_df,
                                      attr) {

  if (which_graph_df == "ndf") {
    df <- graph$nodes_df
  } else if (which_graph_df == "edf") {
    df <- graph$edges_df
  } else {
    stop("The `which_graph_df` argument must be either `ndf` or `edf`.")
  }

  if (!(attr %in% colnames(df))) {
    stop("The `attr` provided is not available.")
  }

  # Are all values not NA?
  all_is_not_na <-
    df %>% dplyr::select_(attr) %>%
    is.na %>% magrittr::not() %>% all()

  # Are all values distinct?
  all_values_distinct <-
    df %>% dplyr::select_(attr) %>% dplyr::distinct() %>% nrow() ==
    nrow(df)

  if (all_is_not_na & all_values_distinct) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


###
# Graph transformation functions
###


# Function to take a graph object and labels for `from`
# and `to` values, and, translate the `from`/`to` label
# values to node ID values
translate_to_node_id <- function(graph, from, to) {

  # Check that node labels are unique
  if (length(unique(graph$nodes_df$label)) !=
      node_count(graph)) {
    stop("You cannot use labels to form edges because they are not distinct")
  }

  # No node labels can be empty
  if (any(graph$nodes_df$label == "")) {
    stop("You cannot use labels to form edges if there are empty strings for labels")
  }

  # Create the `from_id` and `to_id` vectors
  from_id <- vector("integer")
  to_id <- vector("integer")

  # Get an ordered vector of node ID values
  # as `from` nodes
  for (i in 1:length(from)) {
    from_id <-
      c(from_id,
        graph$nodes_df[
          which(graph$nodes_df$label %in% from[i]), 1])
  }

  # Get an ordered vector of node ID values
  # as `to` nodes
  for (i in 1:length(to)) {
    to_id <-
      c(to_id,
        graph$nodes_df[
          which(graph$nodes_df$label %in% to[i]), 1])
  }

  # Reassign these nodes back to `from` and `to`
  from <- from_id
  to <- to_id

  id_from_to <- list(from = from_id, to = to_id)

  return(id_from_to)
}
