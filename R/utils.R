###
# Graph validation functions
###

#' Check whether a graph object is valid
#'
#' @noRd
graph_object_valid <- function(graph) {

  # Check for all component names to be present
  if (!all(c("graph_info", "nodes_df", "edges_df",
             "global_attrs", "directed",
             "last_node", "last_edge",
             "node_selection", "edge_selection",
             "cache", "graph_log") %in%
           names(graph))) {

    return(FALSE)
  }

  # Check for specific graph classes
  graph_valid <- all(
    inherits(graph$graph_info, "data.frame"),
    inherits(graph$nodes_df, "data.frame"),
    inherits(graph$edges_df, "data.frame"),
    inherits(graph$global_attrs, "data.frame"),
    inherits(graph$global_attrs$attr, "character"),
    inherits(graph$global_attrs$value, "character"),
    inherits(graph$global_attrs$attr_type, "character"),
    inherits(graph$directed, "logical"),
    inherits(graph$node_selection, "data.frame"),
    inherits(graph$edge_selection, "data.frame"),
    inherits(graph$cache, "list"),
    inherits(graph$graph_log, "data.frame")
  )

  graph_valid
}

#' Check whether a graph contains any nodes
#'
#' @noRd
graph_contains_nodes <- function(graph) {

  nrow(graph$nodes_df > 0)
}

#' Check whether a graph contains any edges
#'
#' @noRd
graph_contains_edges <- function(graph) {

  nrow(graph$edges_df) > 0
}

#' Check whether a graph contains a valid node selection
#'
#' @noRd
graph_contains_node_selection <- function(graph) {

  nrow(graph$node_selection) > 0
}

#' Check whether a graph contains a valid edge selection
#'
#' @noRd
graph_contains_edge_selection <- function(graph) {

  nrow(graph$edge_selection) > 0
}

#' Get a list of node/edge selection properties
#'
#' @noRd
node_edge_selection_properties <- function(graph) {

  # Determine if there is an existing
  # selection of nodes
  node_selection_available <-
    graph_contains_node_selection(graph = graph)

  # Determine if there is an existing
  # selection of edges
  edge_selection_available <-
    graph_contains_edge_selection(graph)

  # Get the existing node/edge count
  if (node_selection_available || edge_selection_available) {

    selection_type <-
      ifelse(node_selection_available, "node", "edge")

    if (selection_type == "node") {
      selection_count <- nrow(graph$node_selection)
      selection_count_str <-
        paste0(selection_count, " node", ifelse(selection_count > 1, "s", ""))

    } else {

      selection_count <- nrow(graph$edge_selection)
      selection_count_str <-
        paste0(selection_count, " edge", ifelse(selection_count > 1, "s", ""))
    }

  } else {

    selection_type <- NA_character_
    selection_count <- 0
    selection_count_str <- "no nodes or edges"
  }

  list(
    node_selection_available = node_selection_available,
    edge_selection_available = edge_selection_available,
    selection_type = selection_type,
    selection_count = selection_count,
    selection_count_str = selection_count_str)
}

#' Replace the `node_selection` df with different node ID values
#'
#' @noRd
replace_graph_node_selection <- function(graph,
                                         replacement) {

  # Create a new table for the node selection
  dplyr::bind_rows(
    create_empty_nsdf(),
    dplyr::tibble(node = as.integer(replacement))
  )
}

#' Replace the `edge_selection` df with different edge ID values
#'
#' @noRd
replace_graph_edge_selection <- function(graph,
                                         edge_id,
                                         from_node,
                                         to_node) {

  # Create a new table for the edge selection
  dplyr::bind_rows(
    create_empty_esdf(),
    dplyr::tibble(
      edge = as.integer(edge_id),
      from = as.integer(from_node),
      to = as.integer(to_node)
    )
  )
}

create_empty_nsdf <- function() {

  # Create empty `nsdf`
  dplyr::tibble(node = integer()) %>%
    as.data.frame(stringsAsFactors = FALSE)
}

create_empty_esdf <- function() {

  # Create empty `esdf`
  data.frame(
    edge = integer(),
    from = integer(),
    to = integer()
  )
}

#' Is an attribute unique and fully free of `NA`s?
#'
#' This function determines whether a node or edge attribute has values that
#' are all non-`NA` and are unique
#'
#' @noRd
is_attr_unique_and_non_na <- function(graph,
                                      which_graph_df,
                                      attr) {

  if (which_graph_df == "ndf") {
    df <- graph$nodes_df
  } else if (which_graph_df == "edf") {
    df <- graph$edges_df
  } else {
    stop(
      "The `which_graph_df` argument must be either `ndf` or `edf`.",
      call. = FALSE)
  }

  if (!(attr %in% colnames(df))) {
    stop(
      "The `attr` provided is not available.",
      call. = FALSE)
  }

  # Are all values not NA?
  all_is_not_na <-
    !all(is.na(dplyr::pull(df, !!enquo(attr))))

  # Are all values distinct?
  all_values_distinct <-
    dplyr::pull(df, !!enquo(attr)) %>% dplyr::n_distinct() ==
    nrow(df)

  all_is_not_na && all_values_distinct
}

###
# Graph transformation functions
###

#' Transform `from` and `to` edge attributes
#'
#' This function takes a graph object and labels for `from` and `to` values and
#' then translates the `from`/`to` label values to node ID values
#'
#' @noRd
translate_to_node_id <- function(graph, from, to) {

  # Check that node labels are unique
  if (length(unique(graph$nodes_df$label)) !=
      count_nodes(graph)) {

    stop(
      "You cannot use labels to define edges because they are not distinct.",
      call. = FALSE)
  }

  # No node labels can be empty
  if (any(graph$nodes_df$label == "")) {

    stop(
      "You cannot use labels to define edges since there are empty strings for labels.",
      call. = FALSE)
  }

  # Create the `from_id` and `to_id` vectors
  from_id <- vector("integer")
  to_id <- vector("integer")

  # Get an ordered vector of node ID values
  # as `from` nodes
  for (i in seq_along(from)) {
    from_id <-
      c(from_id,
        graph$nodes_df[
          which(graph$nodes_df$label %in% from[i]), 1])
  }

  # Get an ordered vector of node ID values
  # as `to` nodes
  for (i in seq_along(to)) {
    to_id <-
      c(to_id,
        graph$nodes_df[
          which(graph$nodes_df$label %in% to[i]), 1])
  }

  # Reassign these nodes back to `from` and `to`
  from <- from_id
  to <- to_id

  list(from = from_id, to = to_id)
}


###
# Graph logging functions
###

#' Get the time of the graph function invocation
#'
#' @noRd
graph_function_sys_time <- function() {
  Sys.time()
}

#' Get the time taken for a graph function to execute
#'
#' This calculates the difference from the start of the function call (relies on
#' a call of the `graph_function_sys_time()` function) to the time of invoking
#' this function
#'
#' @noRd
graph_function_duration <- function(start_time) {
  end_time <- Sys.time()
  time_diff_s <- (end_time - start_time)[[1]]

  time_diff_s
}

#' Add a log line for a graph `action`
#'
#' @noRd
add_action_to_log <- function(graph_log,
                              version_id,
                              function_used,
                              time_modified,
                              duration,
                              nodes,
                              edges,
                              d_n = 0L,
                              d_e = 0L) {

  # Ensure that `time_modified` inherits from POSIXct
  if (!inherits(time_modified, "POSIXct")) {

    cli::cli_abort(
      "The `time_modified` value must inherit from POSIXct.",
      call. = FALSE)
  }

  # Create a log line
  graph_log_line <-
    data.frame(
      version_id = version_id,
      function_used = as.character(function_used),
      time_modified = time_modified,
      duration = as.numeric(duration),
      nodes = nodes,
      edges = edges,
      d_n = as.integer(d_n),
      d_e = as.integer(d_e),
      stringsAsFactors = FALSE)

  # Append the log line to `graph_log`
  dplyr::bind_rows(graph_log, graph_log_line)
}

#' Save the graph as an RDS file
#'
#' @noRd
save_graph_as_rds <- function(graph) {

  # Construct a file name for the RDS
  rds_filename <-
    paste0(
      graph$graph_info$graph_id, "_",
      formatC(
        graph$graph_log$version_id[nrow(graph$graph_log)],
        width = 6, format = "d", flag = "0"), "_",
      round(
        as.integer(
          graph$graph_log$time_modified[nrow(graph$graph_log)]), 4),
      ".rds")

  # Construct the subdirectory name for the RDS-based
  # graph backups
  rds_dir_name <-
    paste0("backup_", graph$graph_info$graph_id)

  # If directory doesn't exist, create the directory
  # inside of the working directory
  if (!dir.exists(rds_dir_name)) {
    dir.create(rds_dir_name)
  }

  # Save the graph as an RDS file in the subdirectory
  saveRDS(graph, file = file.path(rds_dir_name, rds_filename, fsep = "/"))
}

###
# Selection helper functions
###

#' Get column names from a column selection statement
#'
#' @noRd
get_col_selection <- function(col_selection_stmt) {

  if (length(col_selection_stmt) == 1 &&
      all(stringr::str_detect(
        string = col_selection_stmt,
        pattern = "^([a-zA-Z_\\.][a-zA-Z0-9_\\.]*?|`.*?`)$")
        )) {

    selection_type <- "single_column_name"

    # Get the column names
    column_selection <- col_selection_stmt

  } else if (length(col_selection_stmt) == 1 &&
             all(stringr::str_detect(
               string = col_selection_stmt,
               pattern = "(.* & ).*")
               )) {

    selection_type <- "column_names"

    # Get the column names
    column_selection <-
      stringr::str_split(
        string = col_selection_stmt,
        pattern = " & ") %>%
      unlist()

  } else if (any(stringr::str_detect(
    string = col_selection_stmt,
    pattern = "^[0-9]*?:[0-9]*?$"))) {

    selection_type <- "column_index_range"

    # Get the column indices
    column_selection <-
      seq(
        from = (stringr::str_split(
          string = col_selection_stmt,
          pattern = ":") %>%
            unlist())[1] %>%
          as.numeric(),
        to = (stringr::str_split(
          string = col_selection_stmt,
          pattern = ":") %>%
            unlist())[2] %>%
          as.numeric())

  } else if (any(
    stringr::str_detect(
      string = col_selection_stmt,
      pattern = "^([a-zA-Z_\\.][a-zA-Z0-9_\\.]*?|`.*?`):([a-zA-Z_\\.][a-zA-Z0-9_\\.]*?|`.*?`)$"))
  ) {

    selection_type <- "column_range"

    # Get the first and last column names
    column_selection <-
      c(
        (stringr::str_split(
          string = col_selection_stmt,
          pattern = ":") %>%
           unlist())[1],
        (stringr::str_split(
          string = col_selection_stmt,
          pattern = ":") %>%
           unlist())[2])
  } else {
    return(list())
  }

  list(
    selection_type = selection_type,
    column_selection = column_selection)
}

###
# Aesthetic attribute functions
###

#' Get the ideal contrasting text color given the fill color of a node
#'
#' @noRd
contrasting_text_color <- function(background_color) {

  rgb_colors <-
    ((grDevices::col2rgb(background_color) %>%
        as.numeric()) / 255)^2.2

  luminance <-
    (0.2126 * rgb_colors[1]) +
    (0.7152 * rgb_colors[2]) +
    (0.0722 * rgb_colors[3])

  saturation <-
    (max(rgb_colors) - min(rgb_colors) + 0.00001) /
    (max(rgb_colors) + 0.00001)

  if (saturation < 0.35) {
    if (luminance > 0.5) {
      contrasting_color <- "#000000"
    }
  } else {
    contrasting_color <- "#FFFFFF"
  }

  contrasting_color
}

###
# Functions that help build info messages
###

#' Construct a consistent message string, passing it to `message()`
#'
#' @noRd
emit_message <- function(fcn_name,
                         message_body) {

  glue::glue("`{fcn_name}()` INFO: {message_body}") %>%
    as.character() %>%
    message()
}

#' Get the calling function as a formatted character string
#'
#' @noRd
get_calling_fcn <- function() {

  calling_fcn <- deparse(sys.call(-1))

  stringr::str_replace_all(
    calling_fcn,
    pattern = "([a-z0-9_]*)(.*)",
    replacement = "\\1")
}

###
# Functions that produce useful vectors
###


#' Produce a vector of Graphviz graph attribute names
#'
#' @noRd
gv_graph_attributes <- function() {

  c("layout", "bgcolor", "rankdir",
    "overlap", "outputorder", "fixedsize",
    "mindist", "nodesep", "ranksep",
    "stylesheet")
}

#' Produce a vector of Graphviz node attribute names
#'
#' @noRd
gv_node_attributes <- function() {

  c("shape", "style", "penwidth", "color", "fillcolor",
    "fontname", "fontsize", "fontcolor", "image", "fa_icon",
    "height", "width", "group",
    "tooltip", "xlabel", "URL",
    "distortion", "sides", "skew", "peripheries",
    "gradientangle", "label", "fixedsize",
    "labelloc", "margin", "orientation", "pos")
}

#' Produce a vector of Graphviz edge attribute names
#'
#' @noRd
gv_edge_attributes <- function() {

  c("style", "penwidth", "color", "arrowsize",
    "arrowhead", "arrowtail",
    "fontname", "fontsize", "fontcolor",
    "len", "tooltip", "URL",
    "label", "labelfontname", "labelfontsize",
    "labelfontcolor", "labeltooltip", "labelURL",
    "edgetooltip", "edgeURL",
    "headtooltip", "headURL",
    "headclip", "headlabel", "headport",
    "tailtooltip", "tailURL",
    "tailclip",  "taillabel", "tailport",
    "dir", "decorate")
}

#' Produce a vector of node creation function names
#'
#' @noRd
node_creation_functions <- function() {

  c("add_node", "add_n_nodes", "add_n_node_clones",
    "add_node_clones_ws", "add_n_nodes_ws",
    "add_node_df", "add_nodes_from_df_cols",
    "add_nodes_from_table", "add_full_graph",
    "add_balanced_tree", "add_cycle",
    "add_path", "add_prism", "add_star",
    "add_grid_2d", "add_grid_3d", "add_gnm_graph",
    "add_gnp_graph", "add_pa_graph",
    "add_smallworld_graph", "add_growing_graph",
    "add_islands_graph")
}

#' Produce a vector of node deletion function names
#'
#' @noRd
node_deletion_functions <- function() {
  # previously, create_subgraph_ws, and create_complement_graph?
  c("transform_to_subgraph_ws", "transform_to_complement_graph",
    "delete_node", "delete_nodes_ws")
}

#' Produce a vector of edge creation function names
#'
#' @noRd
edge_creation_functions <- function() {

  c("add_edge", "add_edge_clone", "add_edges_w_string",
    "add_edge_df", "add_forward_edges_ws",
    "add_reverse_edges_ws",
    "add_edges_from_table", "add_full_graph",
    "add_balanced_tree", "add_cycle",
    "add_path", "add_prism", "add_star",
    "add_grid_2d", "add_grid_3d", "add_gnm_graph",
    "add_gnp_graph", "add_pa_graph",
    "add_smallworld_graph", "add_growing_graph",
    "fully_connect_nodes_ws")
}

#' Produce a vector of edge deletion function names
#'
#' @noRd
edge_deletion_functions <- function() {

  c("delete_edge", "delete_edges_ws",
    "create_subgraph_ws", "create_complement_graph",
    "delete_node", "delete_nodes_ws", "delete_loop_edges_ws",
    "fully_disconnect_nodes_ws")
}

#' Produce vector of graph initializing function names
#'
#' @noRd
graph_init_functions <- function() {
  c("create_graph",
    "from_igraph", "from_adj_matrix",
    "import_graph")
}

#' Produce a list of data frames for functions that get node properties
#'
#' @noRd
value_per_node_functions <- function() {

  list(
    "get_alpha_centrality" =
      data.frame(
        arg = c("alpha", "exo", "weights_attr", "tol"),
        value_type = c("numeric", "numeric", "character", "numeric"),
        stringsAsFactors = FALSE
      ),
    "get_authority_centrality" =
      data.frame(
        arg = "weights_attr",
        value_type = "character",
        stringsAsFactors = FALSE
      ),
    "get_betweenness" =
      data.frame(
        arg = NULL,
        value_type = NULL
      ),
    "get_closeness" =
      data.frame(
        arg = "direction",
        value_type = "character",
        stringsAsFactors = FALSE
      ),
    "get_cmty_edge_btwns" =
      data.frame(
        arg = NULL,
        value_type = NULL
      ),
    "get_cmty_fast_greedy" =
      data.frame(
        arg = NULL,
        value_type = NULL
      ),
    "get_cmty_l_eigenvec" =
      data.frame(
        arg = NULL,
        value_type = NULL
      ),
    "get_cmty_louvain" =
      data.frame(
        arg = NULL,
        value_type = NULL
      ),
    "get_cmty_walktrap" =
      data.frame(
        arg = "steps",
        value_type = "numeric",
        stringsAsFactors = FALSE
      ),
    "get_degree_distribution" =
      data.frame(
        arg = "mode",
        value_type = "character",
        stringsAsFactors = FALSE
      ),
    "get_degree_histogram" =
      data.frame(
        arg = "mode",
        value_type = "character",
        stringsAsFactors = FALSE
      ),
    "get_degree_in" =
      data.frame(
        arg = "normalized",
        value_type = "logical",
        stringsAsFactors = FALSE
      ),
    "get_degree_out" =
      data.frame(
        arg = "normalized",
        value_type = "logical",
        stringsAsFactors = FALSE
      ),
    "get_degree_total" =
      data.frame(
        arg = "normalized",
        value_type = "logical",
        stringsAsFactors = FALSE
      ),
    "get_eccentricity" =
      data.frame(
        arg = NULL,
        value_type = NULL
      ),
    "get_eigen_centrality" =
      data.frame(
        arg = "weights_attr",
        value_type = "character",
        stringsAsFactors = FALSE
      ),
    "get_pagerank" =
      data.frame(
        arg = c("directed", "damping"),
        value_type = c("logical", "numeric"),
        stringsAsFactors = FALSE
      ),
    "get_s_connected_cmpts" =
      data.frame(
        arg = NULL,
        value_type = NULL
      ),
    "get_w_connected_cmpts" =
      data.frame(
        arg = NULL,
        value_type = NULL
      )
  )
}


#' Get the `df_id` values for a graph's internal NDF or EDF
#'
#' @noRd
get_df_ids <- function(graph_df) {

  if (nrow(graph_df) > 0) {

    if ("df_id" %in% colnames(graph_df)) {

      graph_df %>%
        dplyr::select("df_id") %>%
        dplyr::filter(!is.na(df_id)) %>%
        dplyr::pull("df_id")
    } else {
      return(NA_character_)
    }

  } else if (nrow(graph_df) == 0) {
    return(NA_character_)
  }
}


#' Scavenge the `graph$df_storage` list
#'
#' This removes any linked data frames if the associated nodes or edges no
#' longer exist.
#'
#' @noRd
remove_linked_dfs <- function(graph) {

  if (is.null(graph$df_storage)) {
    return(graph)
  }

  ndf_df_ids <-
    graph %>%
    get_node_df() %>%
    get_df_ids()

  edf_df_ids <-
    graph %>%
    get_edge_df() %>%
    get_df_ids()

  # Determine if any of the stored
  # data frames are not available in
  # the graph's internal node data frame
  if (length(graph$df_storage) > 0) {

    ndf_df_id_to_remove <-
      graph$df_storage %>%
      dplyr::bind_rows() %>%
      dplyr::filter(node_edge__ == "node") %>%
      dplyr::select("df_id__") %>%
      dplyr::distinct() %>%
      dplyr::pull("df_id__") %>%
      base::setdiff(ndf_df_ids)

    # If any stored data frames are associated
    # with edges that no longer exist, remove them
    if (length(ndf_df_id_to_remove) > 0) {

      for (i in seq_along(ndf_df_id_to_remove)) {

        graph$df_storage[ndf_df_id_to_remove[i]] <- NULL
      }
    }
  }

  # Determine if any of the stored
  # data frames are not available in
  # the graph's internal edge data frame
  if (length(graph$df_storage) > 0) {
    edf_df_id_to_remove <-
      graph$df_storage %>%
      dplyr::bind_rows() %>%
      dplyr::filter(node_edge__ == "edge") %>%
      dplyr::select("df_id__") %>%
      dplyr::distinct() %>%
      dplyr::pull("df_id__") %>%
      base::setdiff(edf_df_ids)


    # If any stored data frames are associated
    # with edges that no longer exist, remove them
    if (length(edf_df_id_to_remove) > 0) {

      for (i in seq_along(edf_df_id_to_remove)) {

        graph$df_storage[edf_df_id_to_remove[i]] <- NULL
      }
    }
  }

  # Check the type of list that remains
  if (length(graph$df_storage) == 0) {
    graph$df_storage <-
      unname(graph$df_storage)
  }

  # Remove the `df_id` column from the
  # graph's ndf if there are no referenced
  # data frames within (i.e., all NA)
  if (rlang::has_name(graph$nodes_df, "df_id") &&
      all(is.na(graph$nodes_df$df_id))) {

    graph$nodes_df$df_id <- NULL
  }

  # Remove the `df_id` column from the
  # graph's edf if there are no referenced
  # data frames within (i.e., all NA)
  if (rlang::has_name(graph$edges_df, "df_id") &&
      all(is.na(graph$edges_df$df_id))) {

    graph$edges_df$df_id <- NULL
  }

  graph
}

###
# Functions for working with graph SVG output
###

#' Create a table of SVG attributes
#'
#' @noRd
get_svg_tbl <- function(svg_vec) {

  svg_tbl <-
    dplyr::tibble(
      index = integer(),
      type = character(),
      node_id = integer(),
      from = integer(),
      to = integer())

  for (i in seq(svg_vec)) {

    line <- svg_vec[i]

    if (grepl("^<\\?xml.*", line)) {
      rec <- dplyr::tibble(index = i, type = "xml_declaration")
    } else if (grepl("(^<!DOCTYPE.*|.*http://www.w3.org.*)", line)) {
      rec <- dplyr::tibble(index = i, type = "doctype_declaration")
    } else if (grepl("(<!-- Generated by|^ -->$)", line)) {
      rec <- dplyr::tibble(index = i, type = "generator_comment")
    } else if (grepl("<!-- Title:", line)) {
      rec <- dplyr::tibble(index = i, type = "title_block")
    } else if (grepl("^<svg", line)) {
      rec <- dplyr::tibble(index = i, type = "svg") %>% dplyr::bind_cols(get_attr_tbl(line))
    } else if (grepl(" viewBox", line)) {
      rec <- dplyr::tibble(index = i, type = "viewbox_info")
    } else if (grepl("^<g ", line)) {
      rec <- dplyr::tibble(index = i, type = "g") %>% dplyr::bind_cols(get_attr_tbl(line))
    } else if (grepl("^<title>", line)) {
      rec <- dplyr::tibble(index = i, type = "title") %>% dplyr::bind_cols(get_inner_html(line))
    } else if (grepl("^<polygon", line)) {
      rec <- dplyr::tibble(index = i, type = "polygon") %>% dplyr::bind_cols(get_attr_tbl(line))
    } else if (grepl("^<path", line)) {
      rec <- dplyr::tibble(index = i, type = "path") %>% dplyr::bind_cols(get_attr_tbl(line))
    } else if (grepl("^<ellipse", line)) {
      rec <- dplyr::tibble(index = i, type = "ellipse") %>% dplyr::bind_cols(get_attr_tbl(line))
    } else if (grepl("<!-- [0-9]*? -->", line)) {
      node_id <- gsub("(<!-- | -->)", "", line) %>% as.integer()
      rec <- dplyr::tibble(index = i, type = "node_block", node_id = node_id)
    } else if (grepl("<!-- [0-9]*?&#45;&gt;[0-9]*? -->", line)) {
      from_node_id <- gsub("(<!-- |?&#45;&gt;[0-9]*? -->)", "", line) %>% as.integer()
      to_node_id <- gsub("(<!-- [0-9]*?&#45;&gt;| -->)", "", line) %>% as.integer()
      rec <- dplyr::tibble(index = i, type = "edge_block", from = from_node_id, to = to_node_id)
    } else if (grepl("<!-- [0-9]*?&#45;&#45;[0-9]*? -->", line)) {
      from_node_id <- gsub("(<!-- |?&#45;&#45;[0-9]*? -->)", "", line) %>% as.integer()
      to_node_id <- gsub("(<!-- [0-9]*?&#45;&#45;| -->)", "", line) %>% as.integer()
      rec <- dplyr::tibble(index = i, type = "edge_block", from = from_node_id, to = to_node_id)
    } else if (grepl("^<text ", line)) {
      rec <- dplyr::tibble(index = i, type = "text") %>%
        dplyr::bind_cols(get_attr_tbl(line)) %>%
        dplyr::bind_cols(get_inner_html(line))
    } else if (grepl("</g>", line)) {
      rec <- dplyr::tibble(index = i, type = "g_close")
    } else if (grepl("</svg>", line)) {
      rec <- dplyr::tibble(index = i, type = "svg_close")
    } else {
      rec <- dplyr::tibble(index = i, type = "unknown")
    }

    svg_tbl <- dplyr::bind_rows(svg_tbl, rec)
  }

  svg_tbl %>% tidyr::fill(node_id)
}

#' Function to create a one-row table of attr-value pairs
#'
#' @noRd
get_attr_tbl <- function(line) {

  line <- gsub("<[a-z]*? ", "", line) %>% gsub("\"", "'", .) %>% gsub("(/>|>|>.*)", "", .)

  el_attrs <-
    strsplit(line, "' ") %>%
    unlist() %>%
    gsub("'", "", .) %>%
    strsplit("=")

  stats::setNames(
    sapply(el_attrs, `[[`, 2),
    sapply(el_attrs, `[[`, 1)) %>%
    as.list() %>%
    dplyr::as_tibble()
}

#' Function to create a one-row, one-column table of inner HTML
#'
#' @noRd
get_inner_html <- function(line) {

  inner_html <- gsub("(^<.*?>|</.*?>)", "", line)

  dplyr::tibble(inner_html = inner_html)
}
