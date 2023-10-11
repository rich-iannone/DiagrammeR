#' Print the graph to the terminal
#'
#' This function will provide a summary of the graph.
#'
#' @param x A graph object of class `dgr_graph`.
#' @examples
#' \dontrun{
#' # Create a random graph using the
#' # `add_gnm_graph()` function
#' graph <-
#'   create_graph() %>%
#'   add_gnm_graph(
#'     n = 10,
#'     m = 15,
#'     set_seed = 23)
#'
#' # Get a summary of the graph
#' graph
#' }
#' @keywords internal
#' @export
print.dgr_graph <- function(x, ...) {

  args <- list(...)
  args <- NULL

  # Get the console width
  console_width <- getOption("width")

  # Create vector of information labels
  info_labels <-
    c("get_node_df" = " info: `get_node_df()`",
      "get_edge_df" = " info: `get_edge_df()`",
      "get_selection" = " info: `get_selection()`",
      "get_cache" = " info: `get_cache()",
      "get_attr_dfs" = " info: `get_attr_dfs()`",
      "get_global_graph_attr_info" = " info: `get_global_graph_attr_info()`")

  # Get a count of all nodes in the graph
  node_count <- x %>% count_nodes()

  # Get a count of all edges in the graph
  edge_count <- x %>% count_edges()

  # Get the node `type` status
  if (all(is.na(x$nodes_df$type))) {

    node_type_status <- "<unused>"

  } else if (!all(is.na(x$nodes_df$type)) & any(is.na(x$nodes_df$type))) {

    node_type_status <-
      paste0(
        x$nodes_df$type %>%
          unique() %>%
          base::setdiff(NA_character_) %>%
          length(), " val",
        ifelse(
          x$nodes_df$type %>%
            unique() %>%
            base::setdiff(NA_character_) %>%
            length() > 1, "s", ""))

  } else if (!any(is.na(x$nodes_df$type))) {

    node_type_status <-
      paste0(
        x$nodes_df$type %>%
          unique() %>%
          base::setdiff(NA_character_) %>%
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
          base::setdiff(NA_character_) %>%
          length(), " val",
        ifelse(
          x$nodes_df$label %>%
            unique() %>%
            base::setdiff(NA_character_) %>%
            length() > 1, "s", ""))

  } else if (!any(is.na(x$nodes_df$label))) {

    node_label_status <-
      paste0(
        x$nodes_df$label %>%
          unique() %>%
          base::setdiff(NA_character_) %>%
          length(), " vals - complete")

    if (!any(duplicated(x$nodes_df$label))) {

      node_label_status <-
        paste0(
          node_label_status, " & unique")
    }
  }

  # Get a list of extra node attributes
  node_extra_attrs <-
    x$nodes_df %>%
    colnames() %>%
    base::setdiff(c("id", "type", "label"))

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
          base::setdiff(NA_character_) %>%
          length(), " val",
        ifelse(
          x$edges_df$rel %>%
            unique() %>%
            base::setdiff(NA_character_) %>%
            length() > 1, "s", ""))

  } else if (!any(is.na(x$edges_df$rel))) {
    edge_rel_status <-
      paste0(
        x$edges_df$rel %>%
          unique() %>%
          base::setdiff(NA_character_) %>%
          length(), " vals - complete")
  }

  # Get a list of extra edge attributes
  edge_extra_attrs <-
    x$edges_df %>%
    colnames() %>%
    base::setdiff(c("id", "from", "to", "rel"))

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

  # Determine if the graph is weighted
  if (is_graph_weighted(x)) {
    weighted_graph_status <- TRUE
  } else if (!is_graph_weighted(x)) {
    weighted_graph_status <- FALSE
  }

  # Determine if the graph is a DAG
  if (is_graph_dag(x)) {
    dag_graph_status <- TRUE
  } else if (is_graph_dag(x) == FALSE) {
    dag_graph_status <- FALSE
  }

  # Determine if the graph is a property graph
  if (is_property_graph(x)) {
    property_graph_status <- TRUE
  } else if (!is_property_graph(x)) {
    property_graph_status <- FALSE
  }

  # Determine if the graph is a simple graph
  if (is_graph_simple(x)) {
    simple_graph_status <- TRUE
  } else if (!is_graph_simple(x)) {
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
    } else if (edge_count == 1) {
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
    # density_str <-
    #   paste0(
    #     "density: ", graph_info(x)$dens)

    header_str <-
      paste0(
        "DiagrammeR Graph // ", node_count_str,
        " / ", edge_count_str)#,
    # " / ", density_str)

  } else if (edge_count == 0) {

    header_str <-
      paste0(
        "DiagrammeR Graph // ", node_count_str)
  }

  # Generate the subheader string for printing
  if (is_graph_empty(x)) {

    sub_header_str <-
      paste0(
        "  -- empty graph",
        " (mode: ", directed_undirected, ")")

  } else {

    sub_header_str <-
      paste0(
        "  -- ",
        directed_undirected,
        ifelse(connected_graph_status, " / connected", " / disconnected"),
        ifelse(weighted_graph_status, " / weighted", ""),
        ifelse(dag_graph_status, " / DAG", ""),
        ifelse(property_graph_status, " / property graph", ""),
        ifelse(simple_graph_status, " / simple", ""))
  }

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

  if (all(is.na(suppressMessages(get_selection(x)))) &
      length(suppressMessages(get_selection(x))) == 1) {

    selection_str <- "<none>"

  } else {

    if (nrow(x$node_selection) > 0) {

      selection_type <- "node"

      selection_count <-
        nrow(x$node_selection)

      selection_all <-
        ifelse(
          selection_count == nrow(x$nodes_df),
          TRUE, FALSE)
    }

    if (nrow(x$edge_selection) > 0) {

      selection_type <- "edge"

      selection_count <-
        nrow(x$edge_selection)

      selection_all <-
        ifelse(
          selection_count == nrow(x$edges_df),
          TRUE, FALSE)
    }

    selection_str <-
      paste0(
        selection_count, " ",
        selection_type,
        ifelse(selection_count > 1, "s", ""),
        " selected",
        ifelse(selection_all, " (ALL)", ""))
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
      "  CACHE / ",
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
      nrow(x$graph_actions)

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
      paste0(
        paste(tail_actions_logged$function_used, collapse = "() -> "), "()")
  } else if (number_of_actions_in_tail < number_of_actions_logged) {
    last_actions_performed_str <-
      paste0(
        "<", number_of_actions_logged - number_of_actions_in_tail, " action",
        ifelse((number_of_actions_logged - number_of_actions_in_tail) > 1, "s", ""),
        "> -> ",
        paste0(
          paste(
            tail_actions_logged$function_used, collapse = "() -> "), "()"))
  }

  graph_history_detail_str <-
    paste0(
      "  GRAPH LOG / ",
      last_actions_performed_str)

  node_detail_str_1_length <- nchar(node_detail_str_1)
  edge_detail_str_1_length <- nchar(edge_detail_str_1)
  selection_detail_str_length <- nchar(selection_detail_str)
  cache_detail_str_length <- nchar(cache_detail_str)
  global_attrs_detail_str_length <- nchar(global_attrs_detail_str)

  info_labels_node_df_length <- nchar(info_labels["get_node_df"])[[1]]
  info_labels_edge_df_length <- nchar(info_labels["get_edge_df"])[[1]]
  info_labels_selection_length <- nchar(info_labels["get_selection"])[[1]]
  info_labels_cache_length <- nchar(info_labels["get_cache"])[[1]]
  info_labels_attr_dfs_length <- nchar(info_labels["get_attr_dfs"])[[1]]
  info_labels_global_graph_attrs <- nchar(info_labels["get_global_graph_attr_info"])[[1]]

  if (console_width - node_detail_str_1_length - info_labels_node_df_length >= 5) {

    node_detail_str_1 <-
      paste0(
        node_detail_str_1,
        paste(
          rep(
            x = " ",
            times = (console_width -
                       node_detail_str_1_length -
                       info_labels_node_df_length)),
          collapse = ""),
        info_labels["get_node_df"])
  }

  if (console_width - edge_detail_str_1_length - info_labels_edge_df_length >= 5) {

    edge_detail_str_1 <-
      paste0(
        edge_detail_str_1,
        paste(
          rep(
            x = " ",
            times = (console_width -
                       edge_detail_str_1_length -
                       nchar(info_labels["get_edge_df"])[[1]])),
          collapse = ""),
        info_labels["get_edge_df"])
  }

  if (console_width -
      selection_detail_str_length -
      info_labels_selection_length >= 5 &
      !is.na(suppressMessages(get_selection(x)))[1]) {

    selection_detail_str <-
      paste0(
        selection_detail_str,
        paste(
          rep(
            x = " ",
            times = (console_width -
                       selection_detail_str_length -
                       info_labels_selection_length)),
          collapse = ""),
        info_labels["get_selection"])
  }

  if (console_width -
      cache_detail_str_length -
      info_labels_cache_length >= 5 &
      !is.na(get_cache(x))[1]) {

    cache_detail_str <-
      paste0(
        cache_detail_str,
        paste(
          rep(
            x = " ",
            times = (console_width -
                       cache_detail_str_length -
                       info_labels_cache_length)),
          collapse = ""),
        info_labels["get_cache"])
  }

  if (console_width -
      global_attrs_detail_str_length -
      info_labels_global_graph_attrs >= 5) {

    if (inherits(get_global_graph_attr_info(x), "data.frame")) {

      global_attrs_detail_str <-
        paste0(
          global_attrs_detail_str,
          paste(
            rep(
              x = " ",
              times = (console_width -
                         global_attrs_detail_str_length -
                         info_labels_global_graph_attrs)),
            collapse = ""),
          info_labels["get_global_graph_attr_info"])
    }
  }

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
      graph_history_detail_str, "\n"
    )

  cat(complete_stmt)
}
