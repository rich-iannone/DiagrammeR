#' Create DOT code from two data frames and optionally render graph
#'
#' Generates and returns either Graphviz DOT code or a Graphviz graph from two data frames:
#' one with nodes and the other with edges
#'
#' @param nodes_df a data frame containing, at minimum, a column (called 'nodes') which contains node IDs for the graph. Optionally, additional columns (named as Graphviz node attributes) can be included with values for the named node attribute.
#' @param edges_df a data frame containing, at minimum, a column (called 'edge_op') with edge operations as character strings (in the form of '[node_id] -> [node_id]). Alternatively, there may be two columns (called 'edge_from' and 'edge_to') where node IDs are provided. Optionally, additional columns (named as Graphviz edge attributes) can be included with values for the named edge attribute.
#' @param graph_attrs an optional vector of graph attribute statements that can serve as defaults for the graph.
#' @param node_attrs an optional vector of node attribute statements that can serve as defaults for nodes.
#' @param edge_attrs an optional vector of edge attribute statements that can serve as defaults for edges.
#' @param directed with TRUE (the default) or FALSE, either directed or undirected edge operations will be generated, respectively.
#' @param create_graph with TRUE (the default) the function render the graph using the 'grViz' function; with FALSE, the generated Graphviz DOT code is returned as a vector object (useful for substituting in a separate 'grViz' statement).
#' @param return_code if not NULL, the value 'SVG' returns string of SVG XML text whereas the value 'DOT' returns valid Graphviz DOT code.
#' @param width an optional parameter for specifying the width of the resulting graphic in pixels.
#' @param height an optional parameter for specifying the height of the resulting graphic in pixels.
#' @export graphviz_graph

graphviz_graph <- function(nodes_df = NULL, edges_df = NULL,
                           graph_attrs = NULL, node_attrs = NULL,
                           edge_attrs = NULL, directed = TRUE,
                           create_graph = TRUE,
                           return_code = NULL,
                           width = NULL, height = NULL){

  # Perform basic checks of the inputs
  if (!is.null("nodes_df")){

    stopifnot(class(nodes_df) == "data.frame")

    stopifnot(any(c("node", "nodes", "node_id") %in%
                    colnames(nodes_df)))

    # Force all columns to be of the character class
    for (i in 1:ncol(nodes_df)){
      nodes_df[,i] <- as.character(nodes_df[,i])
    }
  }

  if (!is.null("edges_df")){

    stopifnot(class(edges_df) == "data.frame")

    stopifnot(any(c("edge_op", "edge_ops", "edge", "edges",
                    "edge_from", "edge_to", "from", "to") %in%
                    colnames(edges_df)))

    # Force all columns to be of the character class
    for (i in 1:ncol(edges_df)){
      edges_df[,i] <- as.character(edges_df[,i])
    }
  }

  graph_attributes <- c("layout", "overlap", "fixedsize", "outputorder",
                        "ranksep")

  node_attributes <- c("color", "distortion", "fillcolor",
                       "fixedsize", "fontcolor", "fontname", "fontsize",
                       "group", "height", "label", "labelloc", "margin",
                       "orientation", "penwidth", "peripheries", "shape",
                       "sides", "skew", "style", "tooltip", "width")

  edge_attributes <- c("arrowhead", "arrowsize", "arrowtail", "color",
                       "constraint", "decorate", "dir",
                       "edgeURL", "edgehref", "edgetarget", "edgetooltip",
                       "fontcolor", "fontname", "fontsize", "headclip",
                       "headhref", "headlabel", "headport", "headtarget",
                       "headtooltip", "headURL", "href", "id", "label",
                       "labelangle", "labeldistance", "labelfloat", "labelfontcolor",
                       "labelfontname", "labelfontsize", "labelhref", "labelURL",
                       "labeltarget", "labeltooltip", "layer", "lhead",
                       "ltail", "minlen", "penwidth", "samehead",
                       "sametail", "style", "tailclip", "tailhref",
                       "taillabel", "tailport", "tailtarget", "tailtooltip",
                       "tailURL", "target", "tooltip", "weight")

  # Create the default attributes statement for graph attributes
  if (!is.null(graph_attrs)){
    graph_attr_stmt <- paste0("graph [", paste(graph_attrs, collapse = ",\n"), "]")
  }

  # Create the default attributes statement for node attributes
  if (!is.null(node_attrs)){
    node_attr_stmt <- paste0("node [", paste(node_attrs, collapse = ",\n"), "]")
  }

  # Create the default attributes statement for edge attributes
  if (!is.null(edge_attrs)){
    edge_attr_stmt <- paste0("edge [", paste(edge_attrs, collapse = ",\n"), "]")
  }

  # Combine default attributes into a single block
  if (exists("graph_attr_stmt") & exists("node_attr_stmt") & exists("edge_attr_stmt")){

    combined_attr_stmts <- paste(graph_attr_stmt,
                                 node_attr_stmt,
                                 edge_attr_stmt, sep = "\n")
  }

  if (!exists("graph_attr_stmt") & exists("node_attr_stmt") & exists("edge_attr_stmt")){

    combined_attr_stmts <- paste(node_attr_stmt,
                                 edge_attr_stmt, sep = "\n")
  }

  if (exists("graph_attr_stmt") & !exists("node_attr_stmt") & exists("edge_attr_stmt")){

    combined_attr_stmts <- paste(graph_attr_stmt,
                                 edge_attr_stmt, sep = "\n")
  }

  if (exists("graph_attr_stmt") & exists("node_attr_stmt") & !exists("edge_attr_stmt")){

    combined_attr_stmts <- paste(graph_attr_stmt,
                                 node_attr_stmt, sep = "\n")
  }

  if (exists("graph_attr_stmt") & !exists("node_attr_stmt") & !exists("edge_attr_stmt")){

    combined_attr_stmts <- paste0(graph_attr_stmt, "\n")
  }

  if (!exists("graph_attr_stmt") & exists("node_attr_stmt") & !exists("edge_attr_stmt")){

    combined_attr_stmts <- paste0(node_attr_stmt, "\n")
  }

  if (!exists("graph_attr_stmt") & !exists("node_attr_stmt") & exists("edge_attr_stmt")){

    combined_attr_stmts <- paste0(edge_attr_stmt, "\n")
  }

  #
  # Create the node block
  #

  if (!is.null("nodes_df")){

    # Determine the column number with the node ID
    column_with_node_id <-
      which(colnames(nodes_df) %in% c("node_id", "node", "nodes"))[1]

    # Determine which other columns correspond to node attribute values
    other_columns_with_node_attributes <-
      which(colnames(nodes_df) %in% node_attributes)

    # Construct the 'node_block' character object
    for (i in 1:nrow(nodes_df)){

      if (i == 1) node_block <- vector(mode = "character", length = 0)

      if (length(other_columns_with_node_attributes) > 0){

        for (j in other_columns_with_node_attributes){

          if (j == other_columns_with_node_attributes[1]){

            attr_string <- vector(mode = "character", length = 0)
          }

          attribute <- paste0(colnames(nodes_df)[j], " = ", "'", nodes_df[i, j], "'")

          attr_string <- c(attr_string, attribute)
        }

        if (j == other_columns_with_node_attributes[length(other_columns_with_node_attributes)]){

          attr_string <- paste(attr_string, collapse = ", ")
        }
      }

      # Generate a line of node objects when an attribute string exists
      if (exists("attr_string")){

        line <- paste0("  node",
                       " [", attr_string, "] ",
                       "'", nodes_df[i, column_with_node_id], "'")
      }

      # Generate a line of node objects when an attribute string doesn't exist
      if (!exists("attr_string")){

        line <- paste0("  '", nodes_df[i, column_with_node_id], "'")
      }

      node_block <- c(node_block, line)
    }

    # Construct the 'node_block' character object
    node_block <- paste(node_block, collapse = "\n")

    # Remove the 'attr_string' object if it exists
    if (exists("attr_string") == TRUE){
      rm(attr_string)
    }

    # Remove the 'attribute' object if it exists
    if (exists("attribute") == TRUE){
      rm(attribute)
    }
  }

  #
  # Create the edge block
  #

  if (!is.null("edges_df")){

    # Determine whether 'from' or 'to' columns are in 'edges_df'
    from_to_columns <- ifelse(any(c("edge_from", "edge_to", "from", "to") %in%
                                    colnames(edges_df)), "TRUE", "FALSE")

    # Determine which columns in 'edges_df' contains edge attributes
    other_columns_with_edge_attributes <-
      which(colnames(edges_df) %in% edge_attributes)

    # Determine whether the complementary set of columns is present
    if (from_to_columns == TRUE){

      both_from_to_columns <- all(c(any(c("edge_from", "from") %in%
                                          colnames(edges_df))),
                                  any(c("edge_to", "to") %in%
                                        colnames(edges_df)))
    }

    # If the complementary set of columns is present, determine the positions
    if (exists("both_from_to_columns")){

      if (both_from_to_columns == TRUE){

        from_column <- which(colnames(edges_df) %in% c("edge_from", "from"))[1]

        to_column <- which(colnames(edges_df) %in% c("edge_to", "to"))[1]
      }
    }

    # Construct the 'edge_block' character object
    if (exists("from_column") & exists("to_column")){

      if (length(from_column) == 1 & length(from_column) == 1){

        for (i in 1:nrow(edges_df)){

          if (i == 1) edge_block <- vector(mode = "character", length = 0)

          if (length(other_columns_with_edge_attributes) > 0){

            for (j in other_columns_with_edge_attributes){

              if (j == other_columns_with_edge_attributes[1]){
                attr_string <- vector(mode = "character", length = 0)
              }

              attribute <- paste0(colnames(edges_df)[j], " = ", "'", edges_df[i, j], "'")
              attr_string <- c(attr_string, attribute)
            }

            if (j == other_columns_with_edge_attributes[length(other_columns_with_edge_attributes)]){
              attr_string <- paste(attr_string, collapse = ", ")
            }
          }

          # Generate a line of edge objects when an attribute string exists
          if (exists("attr_string")){

            line <- paste0("  edge",
                           paste0(" [", attr_string, "] "),
                           "'", edges_df[i, from_column], "'",
                           ifelse(directed == TRUE, "->", "--"),
                           "'", edges_df[i, to_column], "'",
                           " ")
          }

          # Generate a line of edge objects when an attribute string doesn't exist
          if (!exists("attr_string")){

            line <-
              paste0("  ",
                     "'", edges_df[i, from_column], "'",
                     ifelse(directed == TRUE, "->", "--"),
                     "'", edges_df[i, to_column], "'",
                     " ")
          }

          edge_block <- c(edge_block, line)
        }
      }
    }

    # Develop the edges block for a data frame containing a column with
    # explicitly defined edge operations
    any_columns_with_edge_ops <-
      ifelse(any(c("edge_op", "edge_ops", "edge", "edges") %in%
                   colnames(edges_df)), "TRUE", "FALSE")

    if (any_columns_with_edge_ops == TRUE){

      column_with_edge_op <-
        which(colnames(edges_df) %in% c("edge_op", "edge_ops", "edge", "edges"))[1]

      directed_proportion <-
        sum(grepl("->", edges_df[,column_with_edge_op])) / nrow(edges_df)

      directed <- ifelse(directed_proportion > 0.8, TRUE, FALSE)

      for (i in 1:nrow(edges_df)){
        if (i == 1) edge_block <- vector(mode = "character", length = 0)

        if (length(other_columns_with_edge_attributes) > 0){

          for (j in other_columns_with_edge_attributes){

            if (j == other_columns_with_edge_attributes[1]){
              attr_string <- vector(mode = "character", length = 0)
            }

            attribute <- paste0(colnames(edges_df)[j], " = ", "'", edges_df[i, j], "'")
            attr_string <- c(attr_string, attribute)

          }

          if (j == other_columns_with_edge_attributes[length(other_columns_with_edge_attributes)]){
            attr_string <- paste(attr_string, collapse = ", ")
          }
        }

        # Generate a line of edge objects when an attribute string exists
        if (exists("attr_string")){

          line <-
            paste0("  edge",
                   " [", attr_string, "] ",
                   "'", gsub(" ", "",
                             unlist(strsplit(edges_df[i, column_with_edge_op],
                                             "-[-|>]")))[1], "'",
                   ifelse(directed == TRUE, "->", "--"),
                   "'", gsub(" ", "",
                             unlist(strsplit(edges_df[i, column_with_edge_op],
                                             "-[-|>]")))[2], "'")
        }

        # Generate a line of edge objects when an attribute string doesn't exist
        if (!exists("attr_string")){

          line <-
            paste0("  '", gsub(" ", "",
                               unlist(strsplit(edges_df[i, column_with_edge_op],
                                               "-[-|>]")))[1], "'",
                   ifelse(directed == TRUE, "->", "--"),
                   "'", gsub(" ", "",
                             unlist(strsplit(edges_df[i, column_with_edge_op],
                                             "-[-|>]")))[2], "'")
        }

        edge_block <- c(edge_block, line)
      }
    }

    # Construct the 'edge_block' character object
    edge_block <- paste(edge_block, collapse = "\n")
  }

  # Create the graph code from the chosen attributes, and the
  # nodes and edges blocks
  if (exists("combined_attr_stmts")){
    combined_block <- paste(combined_attr_stmts,
                            node_block, edge_block,
                            sep = "\n")
  }

  if (!exists("combined_attr_stmts")){
    combined_block <- paste(node_block, edge_block,
                            sep = "\n")
  }

  # Create DOT code
  dot_code <- paste0(ifelse(directed == TRUE, "digraph", "graph"),
                     " {\n", "\n", combined_block, "}")

  # Determine whether any code is asked to be returned
  if (!is.null(return_code)){

    # Optionally generate SVG text
    if (return_code == "SVG"){

      svg <- exportSVG(grViz(diagram = dot_code, width = width, height = height))

      return(svg)
    }

    if (return_code == "DOT"){

      return(dot_code)
    }
  }

  if (create_graph == TRUE){

    # Render the graph using the 'grViz' function
    grViz(diagram = dot_code, width = width, height = height)
  }
}
