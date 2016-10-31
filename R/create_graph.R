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
#' @param graph_attrs an optional vector of graph
#' attribute statements that can serve as defaults
#' for the graph.
#' @param node_attrs an optional vector of node
#' attribute statements that can serve as defaults for
#' nodes.
#' @param edge_attrs an optional vector of edge
#' attribute statements that can serve as defaults for
#' edges.
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
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' # Create an empty graph
#' graph <- create_graph()
#'
#' # A graph can be created with nodes and
#' # without edges; this is usually done in 2 steps:
#' # 1. create a node data frame (ndf) using the
#' #    `create_node_df()` function
#' nodes <- create_node_df(n = 4)
#'
#' # 2. create the graph object with `create_graph()`
#' #    and pass in the ndf to `nodes_df`
#' graph <- create_graph(nodes_df = nodes)
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
#' nodes <-
#'   create_node_df(
#'     n = 4,
#'     label = TRUE,
#'     type = c("type_1", "type_1",
#'              "type_5", "type_2"),
#'     shape = c("circle", "circle",
#'               "rectangle", "rectangle"),
#'     values = c(3.5, 2.6, 9.4, 2.7))
#'
#' graph <- create_graph(nodes_df = nodes)
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
#' # A graph can also be created by just specifying the
#' # edges between nodes (in this case the unique set
#' # of nodes will be created added along with their
#' # connections but there is no possibility to add
#' # node attributes this way--they can be added later
#' # with different function--although edge attributes
#' # can specified); this is usually done in 2 steps:
#' # 1. create an edge data frame (edf) using the
#' #    `create_edge_df()` function:
#' edges <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = "leading_to",
#'     values = c(7.3, 2.6, 8.3))
#'
#' # 2. create the graph object with `create_graph()`
#' #    and pass in the edf to `edges_df`
#' graph <- create_graph(edges_df = edges)
#'
#' # Get information on the graph's internal edge
#' # data frame (edf)
#' get_edge_df(graph)
#' #>   from to        rel values
#' #> 1    1  4 leading_to    7.3
#' #> 2    2  3 leading_to    2.6
#' #> 3    3  1 leading_to    8.3
#'
#' # You can create a graph with both nodes and edges
#' # defined, and, add in some default attributes
#' # to be applied to all the nodes (`node_attrs`) and
#' # edges (`edge_attrs`)
#' graph <-
#'   create_graph(
#'     nodes_df = nodes,
#'     edges_df = edges,
#'     node_attrs = "fontname = Helvetica",
#'     edge_attrs = c("color = blue",
#'                    "arrowsize = 2"))
#'
#' # For this new graph, get counts of nodes and edges
#' node_count(graph)
#' #> [1] 4
#'
#' edge_count(graph)
#' #> [1] 3
#' @importFrom tibble tibble
#' @export create_graph

create_graph <- function(nodes_df = NULL,
                         edges_df = NULL,
                         graph_attrs = NULL,
                         node_attrs = NULL,
                         edge_attrs = NULL,
                         directed = TRUE,
                         graph_name = NULL,
                         graph_time = NULL,
                         graph_tz = NULL) {

  # Add default values for `graph_attrs`, `node_attrs`,
  # and `edge_attrs`
  if (is.null(graph_attrs)) {
    graph_attrs <-
      c("layout = neato", "outputorder = edgesfirst")
  }

  if (is.null(node_attrs)) {
    node_attrs <-
      c("fontname = Helvetica", "fontsize = 10",
        "shape = circle", "fixedsize = true",
        "width = 0.5", "style = filled",
        "fillcolor = aliceblue", "color = gray70",
        "fontcolor = gray50")
  }

  if (is.null(edge_attrs)) {
    edge_attrs <-
      c("len = 1.5", "color = gray40",
        "arrowsize = 0.5")
  }

  # Create vector of graph attributes
  graph_attributes <-
    c("bgcolor", "layout", "overlap", "fixedsize",
      "mindist", "nodesep", "outputorder", "ranksep",
      "rankdir", "stylesheet")

  # Create vector of node attributes
  node_attributes <-
    c("color", "distortion", "fillcolor",
      "fixedsize", "fontcolor", "fontname", "fontsize",
      "group", "height", "label", "labelloc", "margin",
      "orientation", "penwidth", "peripheries", "pos",
      "shape", "sides", "skew", "style", "tooltip",
      "width", "img", "icon")

  # Create vector of edge attributes
  edge_attributes <-
    c("arrowhead", "arrowsize", "arrowtail", "color",
      "constraint", "decorate", "dir", "edgeURL",
      "edgehref", "edgetarget", "edgetooltip",
      "fontcolor", "fontname", "fontsize", "headclip",
      "headhref", "headlabel", "headport", "headtarget",
      "headtooltip", "headURL", "href", "id", "label",
      "labelangle", "labeldistance", "labelfloat",
      "labelfontcolor", "labelfontname", "labelfontsize",
      "labelhref", "labelURL", "labeltarget",
      "labeltooltip", "layer", "lhead", "ltail", "minlen",
      "penwidth", "samehead", "sametail", "style",
      "tailclip", "tailhref", "taillabel", "tailport",
      "tailtarget", "tailtooltip", "tailURL", "target",
      "tooltip", "weight")

  # Create an empty data frame for global
  # graph attributes
  global_attrs <-
    tibble::tibble(
      attr = as.character(NA),
      value = as.character(NA),
      attr_type = as.character(NA)) %>%
    as.data.frame(stringsAsFactors = FALSE) %>%
    .[-1, ]

  # If nodes, edges, and attributes not provided,
  # create an empty graph
  if (all(c(is.null(nodes_df), is.null(edges_df),
            is.null(graph_attrs), is.null(node_attrs),
            is.null(edge_attrs)))) {

    # Create the `dgr_graph` list object
    dgr_graph <-
      list(graph_name = graph_name,
           graph_time = graph_time,
           graph_tz = graph_tz,
           nodes_df = NULL,
           edges_df = NULL,
           graph_attrs = NULL,
           node_attrs = NULL,
           edge_attrs = NULL,
           global_attrs = global_attrs,
           directed = ifelse(directed,
                             TRUE, FALSE),
           last_node = 0)

    attr(dgr_graph, "class") <- "dgr_graph"

    return(dgr_graph)

  } else if (all(c(is.null(nodes_df),
                   is.null(edges_df)))) {

    # If nodes and edges not provided, but other
    # attributes are, create an empty graph with
    # attributes

    # Create the `dgr_graph` list object
    dgr_graph <-
      list(graph_name = graph_name,
           graph_time = graph_time,
           graph_tz = graph_tz,
           nodes_df = NULL,
           edges_df = NULL,
           graph_attrs = graph_attrs,
           node_attrs = node_attrs,
           edge_attrs = edge_attrs,
           global_attrs = global_attrs,
           directed = ifelse(directed,
                             TRUE, FALSE),
           last_node = 0)

    attr(dgr_graph, "class") <- "dgr_graph"

    return(dgr_graph)

  } else if (!is.null(nodes_df)) {

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
           graph_attrs = graph_attrs,
           node_attrs = node_attrs,
           edge_attrs = edge_attrs,
           global_attrs = global_attrs,
           directed = directed,
           last_node = nrow(nodes_df))

    attr(dgr_graph, "class") <- "dgr_graph"

    return(dgr_graph)

  } else if (is.null(nodes_df) & !is.null(edges_df)) {

    from_to_columns <-
      ifelse(any(c("from", "to") %in%
                   colnames(edges_df)), TRUE, FALSE)

    # Determine which columns in the `edges_df` df
    # contains edge attributes
    other_columns_with_edge_attributes <-
      which(colnames(edges_df) %in% edge_attributes)

    # Determine whether the complementary set of
    # columns is present
    if (from_to_columns) {
      both_from_to_columns <-
        all(c(any(c("from") %in%
                    colnames(edges_df))),
            any(c("to") %in%
                  colnames(edges_df)))
    }

    if (exists("both_from_to_columns")) {
      if (both_from_to_columns) {
        from_column <-
          which(colnames(edges_df) %in% c("from"))[1]
        to_column <-
          which(colnames(edges_df) %in% c("to"))[1]
      }
    }

    nodes_df <-
      create_nodes(
        nodes = unique(c(edges_df$from,
                         edges_df$to)))

    # Create the `dgr_graph` list object
    dgr_graph <-
      list(graph_name = graph_name,
           graph_time = graph_time,
           graph_tz = graph_tz,
           nodes_df = nodes_df,
           edges_df = edges_df,
           graph_attrs = graph_attrs,
           node_attrs = node_attrs,
           edge_attrs = edge_attrs,
           global_attrs = global_attrs,
           directed = directed,
           last_node = nrow(nodes_df))

    attr(dgr_graph, "class") <- "dgr_graph"

    return(dgr_graph)
  }
}
