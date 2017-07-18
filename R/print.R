#' Print the graph to the terminal
#' @description This function will provide a summary
#' of the graph.
#' @param x The graph to print.
#' @return The graph object (invisibly).
#' @method print dgr_graph
#' @examples
#' # Create a graph
#' graph <- create_graph()
#'
#' # Get a summary of the graph
#' graph
#' @export
#' @export print.dgr_graph

print.dgr_graph <- function(x) {

  # Get a count of all nodes in the graph
  node_count <- x %>% node_count()

  # Get a count of all edges in the graph
  edge_count <- x %>% edge_count()

  # Get the node `type` status
  if (all(is.na(x$nodes_df$type))) {

    node_type_status <- "<unused>"

  } else if (!all(is.na(x$nodes_df$type)) & any(is.na(x$nodes_df$type))) {

    node_type_status <-
      paste0(
        x$nodes_df$type %>%
          unique() %>%
          setdiff(as.character(NA)) %>%
          length(), " val",
        ifelse(
          x$nodes_df$type %>%
            unique() %>%
            setdiff(as.character(NA)) %>%
            length() > 1, "s", ""))

  } else if (!any(is.na(x$nodes_df$type))) {

    node_type_status <-
      paste0(
        x$nodes_df$type %>%
          unique() %>%
          setdiff(as.character(NA)) %>%
          length(), " vals - complete")
  }

  # Get the node `label` status
  if (all(is.na(x$nodes_df$label))) {

    node_label_status <- "<unused>"

  } else if (!all(is.na(x$nodes_df$label)) & any(is.na(x$nodes_df$label))) {
    node_label_status <-
      paste0(
        x$nodes_df$label %>%
          unique() %>%
          setdiff(as.character(NA)) %>%
          length(), " val",
        ifelse(
          x$nodes_df$label %>%
            unique() %>%
            setdiff(as.character(NA)) %>%
            length() > 1, "s", ""))

  } else if (!any(is.na(x$nodes_df$label))) {

    node_label_status <-
      paste0(
        x$nodes_df$label %>%
          unique() %>%
          setdiff(as.character(NA)) %>%
          length(), " vals - complete")

    if (any(duplicated(x$nodes_df$label)) == FALSE) {

      node_label_status <-
        paste0(
          node_label_status, " & unique")
    }
  }

  # Get a list of extra node attributes
  node_extra_attrs <-
    x$nodes_df %>%
    colnames() %>%
    setdiff(c("id", "type", "label"))

  if (length(node_extra_attrs) > 0) {

    count_extra_attrs <- length(node_extra_attrs)

    if (count_extra_attrs == 1) {

      node_extra_attrs_str <-
        paste0(
          "1 additional node attribute (",
          node_extra_attrs[1], ")")

    } else if (count_extra_attrs > 1 & count_extra_attrs <= 3) {

      node_extra_attrs_str <-
        paste0(
          count_extra_attrs,
          " additional node attributes (",
          paste(node_extra_attrs, collapse = ", "),
          ")")

    } else if (count_extra_attrs > 3) {

      node_extra_attrs_str <-
        paste0(
          count_extra_attrs,
          " additional node attributes (",
          paste(node_extra_attrs[1:3], collapse = ", "),
          " + ", count_extra_attrs - 3, " more)")
    }
  } else {

    node_extra_attrs_str <- "no additional node attributes"
  }

  # Get the edge `rel` status
  if (all(is.na(x$edges_df$rel))) {

    edge_rel_status <- "<unused>"

  } else if (!all(is.na(x$edges_df$rel)) & any(is.na(x$edges_df$rel))) {

    edge_rel_status <-
      paste0(
        x$edges_df$rel %>%
          unique() %>%
          setdiff(as.character(NA)) %>%
          length(), " val",
        ifelse(
          x$edges_df$rel %>%
            unique() %>%
            setdiff(as.character(NA)) %>%
            length() > 1, "s", ""))

  } else if (!any(is.na(x$edges_df$rel))) {
    edge_rel_status <-
      paste0(
        x$edges_df$rel %>%
          unique() %>%
          setdiff(as.character(NA)) %>%
          length(), " vals - complete")
  }

  # Get a list of extra edge attributes
  edge_extra_attrs <-
    x$edges_df %>%
    colnames() %>%
    setdiff(c("id", "from", "to", "rel"))

  if (length(edge_extra_attrs) > 0) {

    count_extra_attrs <- length(edge_extra_attrs)

    if (count_extra_attrs == 1) {

      edge_extra_attrs_str <-
        paste0(
          "1 additional edge attribute (",
          edge_extra_attrs[1], ")")

    } else if (count_extra_attrs > 1 & count_extra_attrs <= 3) {

      edge_extra_attrs_str <-
        paste0(
          count_extra_attrs,
          " additional edge attributes (",
          paste(edge_extra_attrs, collapse = ", "),
          ")")

    } else if (count_extra_attrs > 3) {

      edge_extra_attrs_str <-
        paste0(
          count_extra_attrs,
          " additional edge attributes (",
          paste(edge_extra_attrs[1:3], collapse = ", "),
          " + ", count_extra_attrs - 3, " more)")
    }
  } else {

    edge_extra_attrs_str <- "no additional edge attributes"
  }

  # Determine if the graph is directed
  if (is_graph_directed(x)) {
    directed_undirected <- "directed"
  } else if (is_graph_directed(x) == FALSE) {
    directed_undirected <- "undirected"
  }

  # Determine if the graph is a property graph
  if (is_property_graph(x)) {
    property_graph_status <- TRUE
  } else if (is_property_graph(x) == FALSE) {
    property_graph_status <- FALSE
  }

  # Determine if the graph is a simple graph
  if (is_graph_simple(x)) {
    simple_graph_status <- TRUE
  } else if (is_graph_simple(x) == FALSE) {
    simple_graph_status <- FALSE
  }

  # Determine if the graph is connected
  # or disconnected
  if (is_graph_connected(x)) {
    connected_graph_status <- TRUE
  } else if (is_graph_connected(x) == FALSE) {
    connected_graph_status <- FALSE
  }

  # Generate a string describing the number of
  # nodes in the graph
  if (node_count > 1) {
    node_count_str <- paste0(node_count, " nodes")
  } else if (node_count == 1) {
    node_count_str <- paste0(node_count, " node")
  } else if (node_count == 0) {
    node_count_str <- "no nodes"
  }

  # Generate a string describing the number of
  # edges in the graph
  if (node_count > 0) {
    if (edge_count > 1) {
      edge_count_str <- paste0(edge_count, " edges")
    } else if (node_count == 1) {
      edge_count_str <- paste0(edge_count, " edge")
    } else if (edge_count == 0) {
      edge_count_str <- "no edges"
    }
  } else {
    edge_count_str <- ""
  }

  # Generate the header string for printing
  if (edge_count > 0) {

    # Generate a string describing the density of
    # the graph
    density_str <-
      paste0(
        "density: ", graph_info(x)$dens)

    header_str <-
      paste0(
        "DiagrammeR Graph // ", node_count_str,
        " / ", edge_count_str,
        " / ", density_str)

  } else if (edge_count == 0) {

    header_str <-
      paste0(
        "DiagrammeR Graph // ", node_count_str)
  }

  # Generate the subheader string for printing
  sub_header_str <-
    paste0(
      "  -- ",
      directed_undirected,
      ifelse(connected_graph_status, " / connected", " / disconnected"),
      ifelse(property_graph_status, " / property graph", ""),
      ifelse(simple_graph_status, " / simple graph", ""))

  #
  # Create strings for node-specific information
  #

  node_detail_str_1 <-
    paste0(
      "  NODES / ",
      "type: ", node_type_status,
      " / ",
      "label: ", node_label_status)

  node_detail_str_2 <-
    paste0(
      "    -- ",
      node_extra_attrs_str)

  #
  # Create strings for edge-specific information
  #

  edge_detail_str_1 <-
    paste0(
      "  EDGES / ",
      "rel: ", edge_rel_status)

  edge_detail_str_2 <-
    paste0(
      "    -- ",
      edge_extra_attrs_str)

  #
  # Create string for active selections
  #

  if (all(is.na(get_selection(x))) & length(get_selection(x)) == 1) {

    selection_str <- "<none>"

  } else {

    if (nrow(x$node_selection) > 0) {

      selection_type <- "node"

      selection_count <-
        nrow(x$node_selection)

      selection_all <-
        ifelse(
          node_selection_count == nrow(x$nodes_df),
          TRUE, FALSE)
    }

    if (nrow(x$edge_selection) > 0) {

      selection_type <- "edge"

      selection_count <-
        nrow(x$edge_selection)

      selection_all <-
        ifelse(
          edge_selection_count == nrow(x$edges_df),
          TRUE, FALSE)
    }

    selection_str <-
      paste0(
        selection_count, " ",
        selection_type,
        ifelse(selection_count > 1, "s", ""),
        " selected",
        ifelse(selection_all, " (ALL)", "")
      )
  }

  selection_detail_str <-
    paste0(
      "  SELECTION / ",
      selection_str)

  #
  # Create string for active caches
  #

  if (all(is.na(get_cache(x)))) {

    cache_str <- "<none>"

  } else {

    cache_count <-
      length(x$cache)

    cache_str <-
      paste0(
        cache_count,
        " cache",
        ifelse(cache_count > 1, "s", ""))
  }

  cache_detail_str <-
    paste0(
      "  CACHES / ",
      cache_str)

  #
  # Create string for global attributes
  #

  if (nrow(x$global_attrs) == 0) {

    global_attrs_str <- "<none>"

  } else {

    global_attrs_count <-
      nrow(x$global_attrs)

    global_attrs_str <-
      paste0(
        global_attrs_count,
        ifelse(global_attrs_count > 1, " are set", " is set"))
  }

  global_attrs_detail_str <-
    paste0(
      "  GLOBAL ATTRS / ",
      global_attrs_str)

  #
  # Create string for graph actions
  #

  if (nrow(x$graph_actions) == 0) {

    graph_actions_str <- "<none>"

  } else {

    graph_actions_count <-
      nrow(x$global_attrs)

    graph_actions_str <-
      paste0(
        graph_actions_count,
        ifelse(graph_actions_count > 1, " are set", " is set"))
  }

  graph_actions_detail_str <-
    paste0(
      "  GRAPH ACTIONS / ",
      graph_actions_str)

  #
  # Create string of the recent graph history
  #

  number_of_actions_logged <- nrow(x$graph_log)

  tail_actions_logged <-
    x$graph_log %>%
    utils::tail(3)

  number_of_actions_in_tail <-
    nrow(tail_actions_logged)

  if (number_of_actions_in_tail == number_of_actions_logged) {
    last_actions_performed_str <-
      paste(tail_actions_logged$function_used, collapse = "() -> ") %>%
      paste0(., "()")
  } else if (number_of_actions_in_tail < number_of_actions_logged) {
    last_actions_performed_str <-
      paste0(
        "<", number_of_actions_logged - number_of_actions_in_tail, " actions> -> ",
        paste(
          tail_actions_logged$function_used, collapse = "() -> ") %>%
          paste0(., "()"))
  }

  graph_history_detail_str <-
    paste0(
      "  GRAPH HISTORY / ",
      last_actions_performed_str)

  # Generate the complete statement for printing
  complete_stmt <-
    paste0(
      header_str, "\n",
      sub_header_str, "\n",
      "\n",
      node_detail_str_1, "\n",
      node_detail_str_2, "\n",
      edge_detail_str_1, "\n",
      edge_detail_str_2, "\n",
      selection_detail_str, "\n",
      cache_detail_str, "\n",
      global_attrs_detail_str, "\n",
      graph_actions_detail_str, "\n",
      graph_history_detail_str)

  cat(complete_stmt)
}
