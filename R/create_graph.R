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
#' @param graph_time a date or date-time string
#' (required for insertion of graph into a graph series
#' of the type \code{temporal}).
#' @param graph_tz an optional value for the time zone
#' (\code{tz}) corresponding to the date or date-time
#' string supplied as a value to \code{graph_time}. If
#' no time zone is provided then it will be set to
#' \code{GMT}.
#' @param attr_theme The theme (i.e., collection of
#' \code{graph}, \code{node}, and \code{edge} global
#' graph attributes) to use for this graph. The default
#' theme is called \code{default}.
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
#' #>   from to        rel values
#' #> 1    1  4 leading_to    7.3
#' #> 2    2  3 leading_to    2.6
#' #> 3    3  1 leading_to    8.3
#'
#' # Get information on the graph's internal node
#' # data frame (ndf)
#' get_node_df(graph)
#' #>   id   type label     shape values
#' #> 1  1 type_1     1    circle    3.5
#' #> 2  2 type_1     2    circle    2.6
#' #> 3  3 type_5     3 rectangle    9.4
#' #> 4  4 type_2     4 rectangle    2.7
#' @importFrom tibble tibble
#' @export create_graph

create_graph <- function(nodes_df = NULL,
                         edges_df = NULL,
                         directed = TRUE,
                         graph_name = NULL,
                         graph_time = NULL,
                         graph_tz = NULL,
                         attr_theme = "default") {

  # Create an empty table for global graph attributes
  global_attrs <-
    tibble::tibble(
      attr = as.character(NA),
      value = as.character(NA),
      attr_type = as.character(NA)) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    .[-1, ]

  # If `attr_theme` is `default` then populate the
  # `global_attrs` data frame with global graph attrs
  if (attr_theme == "default") {

    global_attrs <-
      tibble::tibble(
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
            rep("edge", 3))
        )) %>%
      as.data.frame(stringsAsFactors = FALSE)
  }

  if (all(c(is.null(nodes_df), is.null(edges_df)))) {

    # If neither an ndf nor both ndf & edf provided,
    # create an empty graph

    # Create the `dgr_graph` list object
    dgr_graph <-
      list(graph_name = graph_name,
           graph_time = graph_time,
           graph_tz = graph_tz,
           nodes_df = NULL,
           edges_df = NULL,
           global_attrs = global_attrs,
           directed = ifelse(directed,
                             TRUE, FALSE),
           last_node = 0)

    attr(dgr_graph, "class") <- "dgr_graph"

    return(dgr_graph)

  } else if (!is.null(nodes_df) & is.null(edges_df)) {

    # If only an ndf is provided, create a graph
    # just containing nodes

    # Force the `type` and `label` columns
    # to be of the character class
    for (i in 2:3) {
      nodes_df[, i] <- as.character(nodes_df[, i])
    }

    # Create the `dgr_graph` list object
    dgr_graph <-
      list(graph_name = graph_name,
           graph_time = graph_time,
           graph_tz = graph_tz,
           nodes_df = nodes_df,
           edges_df = NULL,
           global_attrs = global_attrs,
           directed = ifelse(directed,
                             TRUE, FALSE),
           last_node = nrow(nodes_df))

    attr(dgr_graph, "class") <- "dgr_graph"

    return(dgr_graph)

  } else if (!is.null(nodes_df) & !is.null(edges_df)) {

    # If an ndf and edf both provided, create a graph
    # initially populated with both nodes and edges

    # Force the `type` and `label` columns
    # to be of the character class
    for (i in 2:3) {
      nodes_df[, i] <- as.character(nodes_df[, i])
    }

    if (inherits(edges_df, "data.frame")) {
      if (ncol(edges_df) > 2) {

        # Force the rel column to be of the character class
        edges_df[, 3] <- as.character(edges_df[, 3])
      }
    }

    # Create the `dgr_graph` list object
    dgr_graph <-
      list(graph_name = graph_name,
           graph_time = graph_time,
           graph_tz = graph_tz,
           nodes_df = nodes_df,
           edges_df = edges_df,
           global_attrs = global_attrs,
           directed = directed,
           last_node = nrow(nodes_df))

    attr(dgr_graph, "class") <- "dgr_graph"

    return(dgr_graph)
  }
}
