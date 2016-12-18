#' Create a graph object
#' @description Generates a graph object with the
#' option to use node data frames (ndfs) and/or edge
#' data frames (edfs) to populate the initial graph.
#' @param nodes_df an optional data frame containing,
#' at minimum, a column (called \code{nodes}) which
#' contains node IDs for the graph. Additional
#' columns (named as Graphviz node attributes) can be
#' included with values for the named node attribute.
#' @param edges_df an optional data frame containing,
#' at minimum, two columns (called \code{from} and
#' \code{to}) where node IDs are provided. Additional
#' columns (named as Graphviz edge attributes) can be
#' included with values for the named edge attribute.
#' @param directed with \code{TRUE} (the default) or
#' \code{FALSE}, either directed or undirected edge
#' operations will be generated, respectively.
#' @param graph_name an optional string for labeling
#' the graph object.
#' @param attr_theme the theme (i.e., collection of
#' \code{graph}, \code{node}, and \code{edge} global
#' graph attributes) to use for this graph. The default
#' theme is called \code{default}.
#' @param write_backups an option to write incremental
#' backups of changing graph states to disk. If
#' \code{TRUE}, a subdirectory of the working directory
#' will be used to store \code{RDS} files. The
#' default value is \code{FALSE} so one has to opt in
#' to use this functionality.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # With `create_graph()` we can simply create an
#' # empty graph (and add in nodes and edges later,
#' # with other functions)
#' graph <- create_graph()
#'
#' # A graph can be created with nodes and
#' # without edges; this is usually done in 2 steps:
#' # 1. create a node data frame (ndf) using the
#' #    `create_node_df()` function
#' ndf <- create_node_df(n = 4)
#'
#' # 2. create the graph object with `create_graph()`
#' #    and pass in the ndf to `nodes_df`
#' graph <- create_graph(nodes_df = ndf)
#'
#' # Get information on the graph's nodes
#' node_info(graph)
#' #>   id type label deg indeg outdeg loops
#' #> 1  1 <NA>  <NA>   0     0      0     0
#' #> 2  2 <NA>  <NA>   0     0      0     0
#' #> 3  3 <NA>  <NA>   0     0      0     0
#' #> 4  4 <NA>  <NA>   0     0      0     0
#'
#' # You can create a similar graph with just nodes but
#' # also provide a range of attributes for the nodes
#' # (e.g., types, labels, arbitrary 'values')
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     label = TRUE,
#'     type = c("type_1", "type_1",
#'              "type_5", "type_2"),
#'     shape = c("circle", "circle",
#'               "rectangle", "rectangle"),
#'     values = c(3.5, 2.6, 9.4, 2.7))
#'
#' graph <- create_graph(nodes_df = ndf)
#'
#' # Get information on the graph's internal node
#' # data frame (ndf)
#' get_node_df(graph)
#' #>   id   type label     shape values
#' #> 1  1 type_1     1    circle    3.5
#' #> 2  2 type_1     2    circle    2.6
#' #> 3  3 type_5     3 rectangle    9.4
#' #> 4  4 type_2     4 rectangle    2.7
#'
#' # A graph can also be created by specifying both
#' # the nodes and edges; create an edge data frame
#' # (edf) using the `create_edge_df()` function:
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "leading_to",
#'     values = c(7.3, 2.6, 8.3))
#'
#' # 2. create the graph object with `create_graph()`
#' #    and pass in the ndf and edf
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf)
#'
#' # Get information on the graph's internal edge
#' # data frame (edf)
#' get_edge_df(graph)
#' #>   id from to        rel values
#' #> 1  1    1  4 leading_to    7.3
#' #> 2  2    2  3 leading_to    2.6
#' #> 3  3    3  1 leading_to    8.3
#'
#' # Get information on the graph's internal node
#' # data frame (ndf)
#' get_node_df(graph)
#' #>   id   type label     shape values
#' #> 1  1 type_1     1    circle    3.5
#' #> 2  2 type_1     2    circle    2.6
#' #> 3  3 type_5     3 rectangle    9.4
#' #> 4  4 type_2     4 rectangle    2.7
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @export create_graph

create_graph <- function(nodes_df = NULL,
                         edges_df = NULL,
                         directed = TRUE,
                         graph_name = NULL,
                         attr_theme = "default",
                         write_backups = FALSE) {

  ## DF: `graph_info`

  # Get the time of graph creation
  graph_time <- Sys.time()

  # Get the locale's time zone
  graph_tz <- Sys.timezone()

  # Generate a random 8-character, alphanumeric
  # string to use as a graph ID
  graph_id <-
    replicate(
      8, sample(c(LETTERS, letters, 0:9), 1)) %>%
    paste(collapse = "")

  # Create the `graph_info` data frame
  graph_info <-
    data.frame(
      graph_id = as.character(graph_id),
      graph_name = as.character(paste0("graph_", graph_id)),
      graph_time = graph_time,
      graph_tz = graph_tz,
      write_backups = as.logical(write_backups),
      stringsAsFactors = FALSE)

  # Insert a user-defined `graph_name` if supplied
  if (!is.null(graph_name)) {
    graph_info[1, 2] <- as.character(graph_name)
  }

  ## DF: `global_attrs`

  # Create an empty table for global graph attributes
  global_attrs <-
    data.frame(
      attr = as.character(NA),
      value = as.character(NA),
      attr_type = as.character(NA),
      stringsAsFactors = FALSE)[-1, ]

  # If `attr_theme` is `default` then populate the
  # `global_attrs` data frame with global graph attrs
  if (inherits(attr_theme, "character")) {
    if (attr_theme == "default") {
      global_attrs <-
        data.frame(
          attr = as.character(
            c("layout", "outputorder", "fontname", "fontsize",
              "shape", "fixedsize", "width", "style",
              "fillcolor", "color", "fontcolor",
              "len", "color", "arrowsize")
          ),
          value = as.character(
            c("neato", "edgesfirst", "Helvetica", "10",
              "circle", "true", "0.5", "filled",
              "aliceblue", "gray70", "gray50",
              "1.5", "gray40", "0.5")
          ),
          attr_type = as.character(
            c(rep("graph", 2),
              rep("node", 9),
              rep("edge", 3))),
          stringsAsFactors = FALSE)
    } else {
      stop("The value for `attr_theme` doesn't refer to any available theme.")
    }
  } else if (is.null(attr_theme)) {
    global_attrs <-
      tibble::tibble(
        attr = as.character(NA),
        value = as.character(NA),
        attr_type = as.character(NA)) %>%
      .[-1, ] %>%
      as.data.frame(stringsAsFactors = FALSE)
  }

  ## DF: `nodes_df`

  # Create an empty node data frame (`ndf`)
  ndf <-
    data.frame(
      id = as.integer(NA),
      type = as.character(NA),
      label = as.character(NA),
      stringsAsFactors = FALSE)[-1, ]

  ## DF: `edges_df`

  # Create an empty edge data frame (`edf`)
  edf <-
    data.frame(
      id = as.integer(NA),
      from = as.integer(NA),
      to = as.integer(NA),
      rel = as.character(NA),
      stringsAsFactors = FALSE)[-1, ]

  ## DF: `node_selection`

  # Create an empty node selection data frame (`nsdf`)
  nsdf <-
    tibble::tibble(
      node = as.integer(NA))[-1, ] %>%
    as.data.frame(stringsAsFactors = FALSE)

  ## DF: `edge_selection`

  # Create an empty edge selection data frame (`esdf`)
  esdf <-
    tibble::tibble(
      edge = as.integer(NA),
      from = as.integer(NA),
      to = as.integer(NA))[-1, ] %>%
    as.data.frame(stringsAsFactors = FALSE)

  ## DF: `graph_log`

  # Create an empty `graph_log` data frame
  graph_log <-
    data.frame(
      version_id = as.integer(NA),
      function_used = as.character(NA),
      time_modified = graph_time,
      duration = as.numeric(NA),
      nodes = as.integer(NA),
      edges = as.integer(NA),
      stringsAsFactors = FALSE)[-1, ]

  ## Empty Graph

  # Initialize a graph object
  graph <-
    list(graph_info = graph_info,
         nodes_df = ndf,
         edges_df = edf,
         global_attrs = global_attrs,
         directed = ifelse(directed,
                           TRUE, FALSE),
         last_node = 0,
         last_edge = 0,
         node_selection = nsdf,
         edge_selection = esdf,
         graph_log = graph_log)

  attr(graph, "class") <- "dgr_graph"

  # If neither an ndf nor both ndf & edf provided,
  # create an initialized graph with no nodes or edges
  if (all(c(is.null(nodes_df), is.null(edges_df)))) {

    # Update the `graph_log` df with an action
    graph_log <-
      add_action_to_log(
        graph_log = graph_log,
        version_id = 1,
        function_used = "create_graph",
        time_modified = graph_time,
        duration = graph_function_duration(graph_time),
        nodes = nrow(graph$nodes_df),
        edges = nrow(graph$edges_df))

    # Add the `graph_log` df to the graph object
    graph$graph_log <- graph_log

    # Write graph backup if the option is set
    if (graph$graph_info$write_backups) {
      save_graph_as_rds(graph = graph)
    }

    # If neither an ndf nor both ndf & edf provided,
    # return the initialized graph with no nodes or edges
    return(graph)

  } else if (!is.null(nodes_df) & is.null(edges_df)) {

    # If only an ndf is provided, create a graph
    # just containing nodes

    # Force the `type` and `label` columns
    # to be of the character class
    for (i in 2:3) {
      nodes_df[, i] <- as.character(nodes_df[, i])
    }

    # Bind the nodes to the `nodes_df` df in the graph
    graph$nodes_df <-
      dplyr::bind_rows(graph$nodes_df, nodes_df)

    # Modify the `last_node` vector
    graph$last_node <- nrow(nodes_df)

    # Update the `graph_log` df with an action
    graph_log <-
      add_action_to_log(
        graph_log = graph_log,
        version_id = 1,
        function_used = "create_graph",
        time_modified = graph_time,
        duration = graph_function_duration(graph_time),
        nodes = nrow(graph$nodes_df),
        edges = nrow(graph$edges_df))

    # Add the `graph_log` df to the graph object
    graph$graph_log <- graph_log

    # Write graph backup if the option is set
    if (graph$graph_info$write_backups) {
      save_graph_as_rds(graph = graph)
    }

    return(graph)

  } else if (!is.null(nodes_df) & !is.null(edges_df)) {

    # If an ndf and edf both provided, create a graph
    # initially populated with both nodes and edges

    # Force the `type` and `label` columns
    # to be of the character class
    for (i in 2:3) {
      nodes_df[, i] <- as.character(nodes_df[, i])
    }

    # Bind the nodes to the `nodes_df` df in the graph
    graph$nodes_df <-
      dplyr::bind_rows(graph$nodes_df, nodes_df)

    # Modify the `last_node` vector
    graph$last_node <- nrow(nodes_df)

    # Ensure that the edf has the correct classes
    if (inherits(edges_df, "data.frame")) {
      if (ncol(edges_df) > 2) {

        # Force the rel column to be of the character class
        edges_df$rel <- as.character(edges_df$rel)
      }
    }

    # Bind the edges to the `edges_df` df in the graph
    graph$edges_df <-
      dplyr::bind_rows(graph$edges_df, edges_df)

    # Modify the `last_edge` vector
    graph$last_edge <- nrow(edges_df)

    # Update the `graph_log` df with an action
    graph_log <-
      add_action_to_log(
        graph_log = graph_log,
        version_id = 1,
        function_used = "create_graph",
        time_modified = graph_time,
        duration = graph_function_duration(graph_time),
        nodes = nrow(graph$nodes_df),
        edges = nrow(graph$edges_df))

    # Add the `graph_log` df to the graph object
    graph$graph_log <- graph_log

    # Write graph backup if the option is set
    if (graph$graph_info$write_backups) {
      save_graph_as_rds(graph = graph)
    }

    return(graph)
  }
}
