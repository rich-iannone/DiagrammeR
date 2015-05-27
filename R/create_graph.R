#' Create a graph object using data frames representative of nodes and edges
#' @description Generates a graph object using data frames for nodes and/or edges; the graph object can be manipulated by other functions.
#' @param nodes_df an optional data frame containing, at minimum, a column (called \code{nodes}) which contains node IDs for the graph. Additional columns (named as Graphviz node attributes) can be included with values for the named node attribute.
#' @param edges_df an optional data frame containing, at minimum, a column (called \code{edge_op}) with edge operations as character strings (in the form of \code{[node_id] -> [node_id]}). Alternatively, there may be two columns (called \code{edge_from} and \code{edge_to}) where node IDs are provided. Additional columns (named as Graphviz edge attributes) can be included with values for the named edge attribute.
#' @param graph_attrs an optional vector of graph attribute statements that can serve as defaults for the graph.
#' @param node_attrs an optional vector of node attribute statements that can serve as defaults for nodes.
#' @param edge_attrs an optional vector of edge attribute statements that can serve as defaults for edges.
#' @param directed with \code{TRUE} (the default) or \code{FALSE}, either directed or undirected edge operations will be generated, respectively.
#' @param graph_name an optional string for labeling the graph object.
#' @param graph_time a date or date-time string (required for insertion of graph into a graph series of the type \code{temporal}).
#' @param graph_tz an optional value for the time zone (\code{tz}) corresponding to the date or date-time string supplied as a value to \code{graph_time}. If no time zone is provided then it will be set to \code{GMT}.
#' @return a graph object of class \code{dgr_graph}.
#' @export create_graph

create_graph <- function(nodes_df = NULL,
                         edges_df = NULL,
                         graph_attrs = NULL,
                         node_attrs = NULL,
                         edge_attrs = NULL,
                         directed = TRUE,
                         graph_name = NULL,
                         graph_time = NULL,
                         graph_tz = NULL){

  # Create vector of graph attributes
  graph_attributes <- c("bgcolor", "layout", "overlap", "fixedsize", "mindist",
                        "nodesep", "outputorder", "ranksep", "rankdir", "stylesheet")

  # Create vector of node attributes
  node_attributes <- c("color", "distortion", "fillcolor",
                       "fixedsize", "fontcolor", "fontname", "fontsize",
                       "group", "height", "label", "labelloc", "margin",
                       "orientation", "penwidth", "peripheries", "pos", "shape",
                       "sides", "skew", "style", "tooltip", "width", "img", "icon")

  # Create vector of edge attributes
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

  # If nodes, edges, and attributes not provided, create empty graph
  if (all(c(is.null(nodes_df), is.null(edges_df),
            is.null(graph_attrs), is.null(node_attrs),
            is.null(edge_attrs)))){

    # Create DOT code with nothing in graph
    dot_code <- paste0(ifelse(directed == TRUE, "digraph", "graph"),
                       " {\n", "\n}")

    # Create the 'dgr_graph' list object
    dgr_graph <- list(graph_name = graph_name,
                      graph_time = graph_time,
                      graph_tz = graph_tz,
                      nodes_df = NULL,
                      edges_df = NULL,
                      graph_attrs = NULL,
                      node_attrs = NULL,
                      edge_attrs = NULL,
                      directed = ifelse(directed == TRUE, TRUE, FALSE),
                      dot_code = dot_code)

    attr(dgr_graph, "class") <- "dgr_graph"

    return(dgr_graph)
  }

  # If nodes and edges not provided, but other attributes are,
  # create any empty graph with attributes
  if (all(c(is.null(nodes_df), is.null(edges_df)))){

    # Create DOT code with nothing in graph
    dot_code <- paste0(ifelse(directed == TRUE, "digraph", "graph"),
                       " {\n", "\n}")

    # Create the 'dgr_graph' list object
    dgr_graph <- list(graph_name = graph_name,
                      graph_time = graph_time,
                      graph_tz = graph_tz,
                      nodes_df = NULL,
                      edges_df = NULL,
                      graph_attrs = graph_attrs,
                      node_attrs = node_attrs,
                      edge_attrs = edge_attrs,
                      directed = ifelse(directed == TRUE, TRUE, FALSE),
                      dot_code = dot_code)

    attr(dgr_graph, "class") <- "dgr_graph"

    return(dgr_graph)
  }

  # Perform basic checks of the inputs
  if (!is.null(nodes_df)){

    stopifnot(any(c("node", "nodes", "node_id") %in%
                    colnames(nodes_df)))

    # Force all columns to be of the character class
    for (i in 1:ncol(nodes_df)){
      nodes_df[,i] <- as.character(nodes_df[,i])
    }
  }

  if (class(edges_df) == "data.frame"){
    if (ncol(edges_df) > 2){

      stopifnot(any(c("edge_op", "edge_ops", "edge", "edges",
                      "edge_from", "edge_to", "from", "to") %in%
                      colnames(edges_df)))

      # Force all columns to be of the character class
      for (i in 1:ncol(edges_df)){
        edges_df[,i] <- as.character(edges_df[,i])
      }
    }
  }

  # Create the default attributes statement for graph attributes
  if (!is.null(graph_attrs)){
    graph_attr_stmt <- paste0("graph [", paste(graph_attrs, collapse = ",\n       "), "]\n")
  }

  # Create the default attributes statement for node attributes
  if (!is.null(node_attrs)){
    node_attr_stmt <- paste0("node [", paste(node_attrs, collapse = ",\n     "), "]\n")
  }

  # Create the default attributes statement for edge attributes
  if (!is.null(edge_attrs)){
    edge_attr_stmt <- paste0("edge [", paste(edge_attrs, collapse = ",\n     "), "]\n")
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

  if (!is.null(nodes_df)){

    # Determine the column number with the node ID
    column_with_node_id <-
      which(colnames(nodes_df) %in% c("node_id", "node", "nodes"))[1]

    # Determine whether positional (x,y) data is included
    column_with_x <-
      which(colnames(nodes_df) %in% "x")[1]

    column_with_y <-
      which(colnames(nodes_df) %in% "y")[1]

    if (!is.na(column_with_x) & !is.na(column_with_y)){

      pos <- data.frame("pos" =
                          paste0(nodes_df[,column_with_x], ",",
                                 nodes_df[,column_with_y],
                                 "!"))

      nodes_df <- cbind(nodes_df, pos)
    }

    # Determine whether column 'alpha' exists
    if (any(grepl("$alpha^", colnames(nodes_df)))){
      column_with_alpha_assigned <- grep("$alpha^", colnames(nodes_df))
    } else {
      column_with_alpha_assigned <- NA
    }

    if (!is.na(column_with_alpha_assigned)){

      # Determine number of color attributes in node data frame
      number_of_col_attr <- length(which(colnames(nodes_df) %in%
                                           c("color", "fillcolor", "fontcolor")))

      # If number of color attrs in df is 1, rename referencing alpha column
      if (number_of_col_attr == 1){

        name_of_col_attr <-
          colnames(nodes_df)[which(colnames(nodes_df) %in%
                                     c("color", "fillcolor", "fontcolor"))]

        colnames(nodes_df)[column_with_alpha_assigned] <-
          paste0("alpha_", name_of_col_attr)
      }
    }

    # Determine whether column 'alpha' with color attr exists
    if (any(grepl("alpha_.*", colnames(nodes_df)))){

      alpha_column_no <- grep("alpha_.*", colnames(nodes_df))

      color_attr_column_name <-
        unlist(strsplit(colnames(nodes_df)[
          (which(grepl("alpha_.*", colnames(nodes_df))))
          ], "_"))[-1]

      color_attr_column_no <-
        which(colnames(nodes_df) %in% color_attr_column_name)

      # Append alpha value only if referenced column is for color
      if (any(c("color", "fillcolor", "fontcolor") %in%
              colnames(nodes_df)[color_attr_column_no])){

        # Append alpha for color values that are X11 color names
        if (all(grepl("[a-z]*", as.character(nodes_df[,color_attr_column_no]))) &
            all(as.character(nodes_df[,color_attr_column_no]) %in%
                x11_hex()[,1])){

          for (i in 1:nrow(nodes_df)){

            nodes_df[i,color_attr_column_no] <-
              paste0(x11_hex()[which(x11_hex()[,1] %in%
                                       as.character(nodes_df[i,color_attr_column_no])), 2],
                     formatC(round(as.numeric(nodes_df[i,alpha_column_no]),0),
                             flag = "0", width = 2))
          }
        }

        # Append alpha for color values that are hex color values
        if (all(grepl("#[0-9a-fA-F]{6}$",
                      as.character(nodes_df[,color_attr_column_no])))){

          for (i in 1:nrow(nodes_df)){

            nodes_df[,color_attr_column_no] <-
              as.character(nodes_df[,color_attr_column_no])

            nodes_df[i,color_attr_column_no] <-
              paste0(nodes_df[i,color_attr_column_no],
                     round(as.numeric(nodes_df[i,alpha_column_no]),0))
          }
        }
      }
    }

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

          # Create the node attributes for labels and tooltips when provided
          if (all(colnames(nodes_df)[j] %in% c("label", "tooltip"),
                  nodes_df[i, j] == '')){

            attribute <- NULL

          } else if (all(colnames(nodes_df)[j] %in% c("label", "tooltip"),
                         nodes_df[i, j] != '')){

            attribute <- paste0(colnames(nodes_df)[j], " = ", "'", nodes_df[i, j], "'")

          } else if (all(!(colnames(nodes_df)[j] %in% c("label", "tooltip")),
                         nodes_df[i, j] == '')){

            attribute <- NULL

          } else if (all(!(colnames(nodes_df)[j] %in% c("label", "tooltip")),
                         nodes_df[i, j] != '')){

            attribute <- paste0(colnames(nodes_df)[j], " = ", "'", nodes_df[i, j], "'")
          }

          attr_string <- c(attr_string, attribute)
        }

        if (j == other_columns_with_node_attributes[length(other_columns_with_node_attributes)]){

          attr_string <- paste(attr_string, collapse = ", ")
        }
      }

      # Generate a line of node objects when an attribute string exists
      if (exists("attr_string")){

        line <- paste0("  '", nodes_df[i, column_with_node_id], "'",
                       " [", attr_string, "] ")
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

  if (is.null(nodes_df) & !is.null(edges_df)){

    from_to_columns <-
      ifelse(any(c("edge_from", "edge_to", "from", "to") %in%
                   colnames(edges_df)), TRUE, FALSE)

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

    if (exists("both_from_to_columns")){

      if (both_from_to_columns == TRUE){

        from_column <- which(colnames(edges_df) %in% c("edge_from", "from"))[1]

        to_column <- which(colnames(edges_df) %in% c("edge_to", "to"))[1]
      }
    }

    nodes_df <- as.data.frame(get_nodes(edges_df), stringsAsFactors = FALSE)
    colnames(nodes_df) <- "nodes"

    for (i in 1:length(nodes_df)){

      if (i == 1) node_block <- vector(mode = "character", length = 0)

      node_block <- c(node_block, paste0("  '", nodes_df[i], "'"))
    }

    # Construct the 'node_block' character object
    node_block <- paste(node_block, collapse = "\n")
  }

  #
  # Create the edge block
  #

  if (!is.null(edges_df)){

    # Determine whether 'from' or 'to' columns are in 'edges_df'
    from_to_columns <- ifelse(any(c("edge_from", "edge_to", "from", "to") %in%
                                    colnames(edges_df)), TRUE, FALSE)

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

              # Create the edge attributes for labels and tooltips when provided
              if (all(colnames(edges_df)[j] %in% c("edgetooltip", "headtooltip",
                                                   "label", "labeltooltip",
                                                   "taillabel", "tailtooltip",
                                                   "tooltip"),
                      edges_df[i, j] == '')){

                attribute <- NULL

              } else if (all(colnames(edges_df)[j] %in% c("edgetooltip", "headtooltip",
                                                          "label", "labeltooltip",
                                                          "taillabel", "tailtooltip",
                                                          "tooltip"),
                             edges_df[i, j] != '')){

                attribute <- paste0(colnames(edges_df)[j], " = ", "'", edges_df[i, j], "'")



              } else if (all(!(colnames(edges_df)[j] %in% c("edgetooltip", "headtooltip",
                                                            "label", "labeltooltip",
                                                            "taillabel", "tailtooltip",
                                                            "tooltip")),
                             edges_df[i, j] == '')){

                attribute <- NULL

              } else if (all(!(colnames(edges_df)[j] %in% c("edgetooltip", "headtooltip",
                                                            "label", "labeltooltip",
                                                            "taillabel", "tailtooltip",
                                                            "tooltip")),
                             edges_df[i, j] != '')){

                attribute <- paste0(colnames(edges_df)[j], " = ", "'", edges_df[i, j], "'")
              }

              attr_string <- c(attr_string, attribute)
            }

            if (j == other_columns_with_edge_attributes[length(other_columns_with_edge_attributes)]){
              attr_string <- paste(attr_string, collapse = ", ")
            }
          }

          # Generate a line of edge objects when an attribute string exists
          if (exists("attr_string")){

            line <- paste0("'", edges_df[i, from_column], "'",
                           ifelse(directed == TRUE, "->", "--"),
                           "'", edges_df[i, to_column], "'",
                           paste0(" [", attr_string, "] "))
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
                   colnames(edges_df)), TRUE, FALSE)

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
    if (exists("edge_block")){
      edge_block <- paste(edge_block, collapse = "\n")
    }
  }

  # Create the graph code from the chosen attributes, and the
  # nodes and edges blocks
  if (exists("combined_attr_stmts")){

    if (exists("edge_block") & exists("node_block")){
      combined_block <- paste(combined_attr_stmts,
                              node_block, edge_block,
                              sep = "\n")
    }

    if (!exists("edge_block") & exists("node_block")){
      combined_block <- paste(combined_attr_stmts,
                              node_block,
                              sep = "\n")
    }
  }

  if (!exists("combined_attr_stmts")){

    if (exists("edge_block")){
      combined_block <- paste(node_block, edge_block,
                              sep = "\n")
    }

    if (!exists("edge_block")){
      combined_block <- node_block
    }
  }

  # Create DOT code
  dot_code <- paste0(ifelse(directed == TRUE, "digraph", "graph"),
                     " {\n", "\n", combined_block, "\n}")

  # Remove empty node or edge attribute statements
  dot_code <- gsub(" \\[\\] ", "", dot_code)

  # Create the 'dgr_graph' list object
  dgr_graph <- list(graph_name = graph_name,
                    graph_time = graph_time,
                    graph_tz = graph_tz,
                    nodes_df = nodes_df,
                    edges_df = edges_df,
                    graph_attrs = graph_attrs,
                    node_attrs = node_attrs,
                    edge_attrs = edge_attrs,
                    directed = directed,
                    dot_code = dot_code)

  attr(dgr_graph, "class") <- "dgr_graph"

  return(dgr_graph)
}
