#' Generate DOT code using a graph object
#' @description Generates Graphviz DOT code as an R
#' character object using DiagrammeR graph object.
#' @param graph a graph object of class
#' \code{dgr_graph}.
#' @return a character vector of length 1 containing
#' Graphviz DOT code.
#' @importFrom dplyr filter mutate
#' @importFrom stringr str_replace str_replace_all
#' @export generate_dot

generate_dot <- function(graph) {

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {
    stop("The graph object is not valid.")
  }

  # Create bindings for specific variables
  attr_type <- attr <- value <- NULL

  # Extract objects from the graph objecct
  nodes_df <- graph$nodes_df
  edges_df <- graph$edges_df
  directed <- graph$directed
  global_attrs <- graph$global_attrs

  if ("graph" %in% global_attrs$attr_type) {
    graph_attrs <-
      global_attrs %>%
      dplyr::filter(attr_type == "graph") %>%
      dplyr::mutate(string = paste(attr, "=", value))

    graph_attrs <- graph_attrs[[4]]

  } else {
    graph_attrs <- NA
  }

  if ("node" %in% global_attrs$attr_type) {
    node_attrs <-
      global_attrs %>%
      dplyr::filter(attr_type == "node") %>%
      dplyr::mutate(string = paste(attr, "=", value))

    node_attrs <- node_attrs[[4]]

  } else {
    node_attrs <- NA
  }

  if ("edge" %in% global_attrs$attr_type) {
    edge_attrs <-
      global_attrs %>%
      dplyr::filter(attr_type == "edge") %>%
      dplyr::mutate(string = paste(attr, "=", value))

    edge_attrs <- edge_attrs[[4]]

  } else {
    edge_attrs <- NA
  }

  # Replace NA values with empty strings in `nodes_df`
  if (!is.null(nodes_df)) {
    if (ncol(nodes_df) >= 4) {
      for (i in 4:ncol(nodes_df)) {
        nodes_df[, i] <-
          ifelse(is.na(nodes_df[, i]), "", nodes_df[, i])
        nodes_df[, i] <-
          as.character(nodes_df[, i])
      }
    }
  }

  # Replace NA values with empty strings in `edges_df`
  if (!is.null(edges_df)) {
    if (ncol(edges_df) >= 4) {
      for (i in 4:ncol(edges_df)) {
        edges_df[, i] <-
          ifelse(is.na(edges_df[, i]), "", edges_df[, i])
        edges_df[, i] <-
          as.character(edges_df[, i])
      }
    }
  }

  # If `display` column in `nodes_df`, modify label
  # column for this render
  if ("display" %in% colnames(nodes_df)) {

    display_col <- which(colnames(nodes_df) == "display")
    label_col <- which(colnames(nodes_df) == "label")

    for (i in 1:nrow(nodes_df)) {
      if (nodes_df[i, display_col] != "") {
        nodes_df[i, label_col] <-
          nodes_df[
            i, which(colnames(nodes_df) == nodes_df[i, display_col])]
      } else {
        nodes_df[i, label_col] <- ""
      }
    }
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

  if (nrow(nodes_df) == 0 &
      nrow(edges_df) == 0) {

    # Create DOT code with nothing in graph
    dot_code <-
      paste0(ifelse(directed,
                    "digraph", "graph"),
             " {\n", "\n}")

  } else {

    #
    # Create the DOT attributes block
    #

    # Create the default attributes statement
    # for graph attributes
    if (length(graph_attrs) != 1) {
      if (!(any(is.na(graph_attrs)))) {
        graph_attr_stmt <-
          paste0("graph [",
                 paste(graph_attrs,
                       collapse = ",\n       "),
                 "]\n")
      }
    }

    # Create the default attributes statement
    # for node attributes
    if (length(node_attrs) != 1) {
      if (!(any(is.na(node_attrs)))) {
        node_attr_stmt <-
          paste0("node [", paste(node_attrs,
                                 collapse = ",\n     "),
                 "]\n")
      }
    }

    # Create the default attributes statement
    # for edge attributes
    if (length(edge_attrs) != 1) {
      if (!(any(is.na(edge_attrs)))) {
        edge_attr_stmt <-
          paste0("edge [", paste(edge_attrs,
                                 collapse = ",\n     "),
                 "]\n")
      }
    }

    # Combine default attributes into a single block
    if (exists("graph_attr_stmt") &
        exists("node_attr_stmt") &
        exists("edge_attr_stmt")) {
      combined_attr_stmts <-
        paste(graph_attr_stmt,
              node_attr_stmt,
              edge_attr_stmt, sep = "\n")
    }

    if (!exists("graph_attr_stmt") &
        exists("node_attr_stmt") &
        exists("edge_attr_stmt")) {
      combined_attr_stmts <-
        paste(node_attr_stmt,
              edge_attr_stmt, sep = "\n")
    }

    if (exists("graph_attr_stmt") &
        !exists("node_attr_stmt") &
        exists("edge_attr_stmt")) {
      combined_attr_stmts <-
        paste(graph_attr_stmt,
              edge_attr_stmt, sep = "\n")
    }

    if (exists("graph_attr_stmt") &
        exists("node_attr_stmt") &
        !exists("edge_attr_stmt")) {
      combined_attr_stmts <-
        paste(graph_attr_stmt,
              node_attr_stmt, sep = "\n")
    }

    if (exists("graph_attr_stmt") &
        !exists("node_attr_stmt") &
        !exists("edge_attr_stmt")) {
      combined_attr_stmts <-
        paste0(graph_attr_stmt, "\n")
    }

    if (!exists("graph_attr_stmt") &
        exists("node_attr_stmt") &
        !exists("edge_attr_stmt")) {
      combined_attr_stmts <-
        paste0(node_attr_stmt, "\n")
    }

    if (!exists("graph_attr_stmt") &
        !exists("node_attr_stmt") &
        exists("edge_attr_stmt")) {
      combined_attr_stmts <-
        paste0(edge_attr_stmt, "\n")
    }

    #
    # Create the DOT node block
    #

    if (nrow(nodes_df) > 0) {

      # Determine whether positional (x,y)
      # data is included
      column_with_x <-
        which(colnames(nodes_df) %in% "x")[1]

      column_with_y <-
        which(colnames(nodes_df) %in% "y")[1]

      if (!is.na(column_with_x) & !is.na(column_with_y)) {

        pos <-
          data.frame(
            "pos" =
              paste0(
                nodes_df[, column_with_x],
                ",",
                nodes_df[, column_with_y],
                "!"))

        nodes_df$pos <- pos$pos
      }

      # Determine whether column 'alpha' exists
      if (any(grepl("$alpha^", colnames(nodes_df)))) {
        column_with_alpha_assigned <-
          grep("$alpha^", colnames(nodes_df))
      } else {
        column_with_alpha_assigned <- NA
      }

      if (!is.na(column_with_alpha_assigned)) {

        # Determine the number of color attributes in
        # the node data frame
        number_of_col_attr <-
          length(which(colnames(nodes_df) %in%
                         c("color", "fillcolor",
                           "fontcolor")))

        # If the number of color attrs in df is 1,
        # rename referencing alpha column
        if (number_of_col_attr == 1) {

          name_of_col_attr <-
            colnames(nodes_df)[
              which(colnames(nodes_df) %in%
                      c("color", "fillcolor",
                        "fontcolor"))]

          colnames(nodes_df)[column_with_alpha_assigned] <-
            paste0("alpha:", name_of_col_attr)
        }
      }

      # Determine whether column 'alpha' with
      # color attr exists
      if (any(grepl("alpha:.*", colnames(nodes_df)))) {

        alpha_column_no <- grep("alpha:.*", colnames(nodes_df))

        color_attr_column_name <-
          unlist(strsplit(colnames(nodes_df)[
            (which(grepl("alpha:.*", colnames(nodes_df))))
            ], ":"))[-1]

        color_attr_column_no <-
          which(colnames(nodes_df) %in% color_attr_column_name)

        # Append alpha value only if referenced
        # column is for color
        if (any(c("color", "fillcolor", "fontcolor") %in%
                colnames(nodes_df)[color_attr_column_no])) {

          # Append alpha for color values that are
          # X11 color names
          if (all(grepl("[a-z]*",
                        as.character(nodes_df[, color_attr_column_no]))) &
              all(as.character(nodes_df[, color_attr_column_no]) %in%
                  x11_hex()[, 1])) {

            for (i in 1:nrow(nodes_df)) {
              nodes_df[i, color_attr_column_no] <-
                paste0(x11_hex()[
                  which(x11_hex()[, 1] %in%
                          as.character(nodes_df[i, color_attr_column_no])), 2],
                  formatC(round(as.numeric(nodes_df[i, alpha_column_no]), 0),
                          flag = "0", width = 2))
            }
          }

          # Append alpha for color values that
          # are hex color values
          if (all(grepl("#[0-9a-fA-F]{6}$",
                        as.character(nodes_df[, color_attr_column_no])))) {

            for (i in 1:nrow(nodes_df)) {
              nodes_df[, color_attr_column_no] <-
                as.character(nodes_df[, color_attr_column_no])

              nodes_df[i, color_attr_column_no] <-
                paste0(nodes_df[i, color_attr_column_no],
                       round(as.numeric(nodes_df[i, alpha_column_no]), 0))
            }
          }
        }
      }

      # Determine which other columns correspond
      # to node attribute values
      other_columns_with_node_attributes <-
        which(colnames(nodes_df) %in% node_attributes)

      # Construct the 'node_block' character object
      for (i in 1:nrow(nodes_df)) {
        if (i == 1) {
          node_block <- vector(mode = "character", length = 0)
        }

        if (length(other_columns_with_node_attributes) > 0) {

          for (j in other_columns_with_node_attributes) {

            if (j == other_columns_with_node_attributes[1]) {
              attr_string <- vector(mode = "character", length = 0)
            }

            # Create the node attributes for labels
            # and tooltips when provided
            if (all(colnames(nodes_df)[j] %in%
                    c("label", "tooltip"),
                    is.na(nodes_df[i, j]))) {
              attribute <- NULL
            } else if (all(colnames(nodes_df)[j] %in%
                           c("label", "tooltip"),
                           !is.na(nodes_df[i, j]))) {
              attribute <-
                paste0(colnames(nodes_df)[j],
                       " = ", "'", nodes_df[i, j], "'")
            } else if (all(!(colnames(nodes_df)[j] %in%
                             c("label", "tooltip")),
                           is.na(nodes_df[i, j]))) {
              attribute <- NULL
            } else if (all(!(colnames(nodes_df)[j] %in%
                             c("label", "tooltip")),
                           !is.na(nodes_df[i, j]))) {
              attribute <-
                paste0(colnames(nodes_df)[j],
                       " = ", "'", nodes_df[i, j], "'")
            }
            attr_string <- c(attr_string, attribute)
          }

          if (j == other_columns_with_node_attributes[
            length(other_columns_with_node_attributes)]) {
            attr_string <- paste(attr_string, collapse = ", ")
          }
        }

        # Generate a line of node objects when an
        # attribute string exists
        if (exists("attr_string")) {
          line <- paste0("  '", nodes_df[i, 1], "'",
                         " [", attr_string, "] ")
        }

        # Generate a line of node objects when an
        # attribute string doesn't exist
        if (!exists("attr_string")) {
          line <-
            paste0("  '",
                   nodes_df[i, 1],
                   "'")
        }
        node_block <- c(node_block, line)
      }

      if ("rank" %in% colnames(nodes_df)) {
        node_block <-
          c(node_block,
            tapply(node_block,
                   nodes_df$rank, FUN = function(x) {
                     if(length(x) > 1) {
                       x <- paste0('subgraph{rank = same\n',
                                   paste0(x, collapse = '\n'),
                                   '}\n')
                     }
                     return(x)
                   }))
      }

      # Construct the `node_block` character object
      node_block <- paste(node_block, collapse = "\n")

      # Remove the `attr_string` object if it exists
      if (exists("attr_string")) {
        rm(attr_string)
      }

      # Remove the `attribute` object if it exists
      if (exists("attribute")) {
        rm(attribute)
      }

      if ('cluster' %in% colnames(nodes_df)) {

        # Get column number for column with node
        # attribute `cluster`
        cluster_colnum <-
          which(colnames(nodes_df) %in% "cluster")

        # Get list of clusters defined for the nodes
        cluster_ids <-
          unique(nodes_df$cluster)[
            which(
              unique(nodes_df$cluster) != "")]

        for (i in seq_along(cluster_ids)) {

          regex <-
            paste0("'",
                   paste(nodes_df[which(nodes_df[, cluster_colnum] == i ), 1],
                         collapse = "'.*?\n |'"), "'.*?\n")

          node_block <-
            stringr::str_replace_all(node_block, regex, "")

          replacement <-
            stringr::str_replace(
              paste0("  cluster_", i, " [label = 'xN\n",
                     cluster_ids[i],
                     "'; shape = 'circle';",
                     " fixedsize = 'true';",
                     " fontsize = '8pt';",
                     " peripheries = '2']  \n"), "x",
              length(
                nodes_df[which(nodes_df[, cluster_colnum] == i ), 1]))

          node_block <-
            stringr::str_replace(node_block, "^", replacement)
        }
      }
    }

    #
    # Create the DOT edge block
    #

    if (nrow(edges_df) > 0) {

      # Determine whether `from` or `to` columns are
      # in `edges_df`
      from_to_columns <-
        ifelse(any(c("from", "to") %in%
                     colnames(edges_df)), TRUE, FALSE)

      # Determine which columns in `edges_df`
      # contain edge attributes
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

      # If the complementary set of columns is present,
      # determine the positions
      if (exists("both_from_to_columns")) {
        if (both_from_to_columns) {
          from_column <-
            which(colnames(edges_df) %in% c("from"))[1]
          to_column <-
            which(colnames(edges_df) %in% c("to"))[1]
        }
      }

      # Construct the `edge_block` character object
      if (exists("from_column") &
          exists("to_column")) {

        if (length(from_column) == 1 &
            length(from_column) == 1) {

          for (i in 1:nrow(edges_df)) {

            if (i == 1) {
              edge_block <-
                vector(mode = "character", length = 0)
            }

            if (length(other_columns_with_edge_attributes) > 0) {

              for (j in other_columns_with_edge_attributes) {

                if (j == other_columns_with_edge_attributes[1]) {
                  attr_string <- vector(mode = "character", length = 0)
                }

                # Create the edge attributes for labels
                # and tooltips when provided
                if (all(colnames(edges_df)[j] %in%
                        c("edgetooltip", "headtooltip",
                          "label", "labeltooltip",
                          "taillabel", "tailtooltip",
                          "tooltip"),
                        is.na(edges_df[i, j]))) {
                  attribute <- NULL
                } else if (all(colnames(edges_df)[j] %in%
                               c("edgetooltip", "headtooltip",
                                 "label", "labeltooltip",
                                 "taillabel", "tailtooltip",
                                 "tooltip"),
                               edges_df[i, j] != '')) {
                  attribute <-
                    paste0(colnames(edges_df)[j],
                           " = ", "'", edges_df[i, j],
                           "'")
                } else if (all(!(colnames(edges_df)[j] %in%
                                 c("edgetooltip", "headtooltip",
                                   "label", "labeltooltip",
                                   "taillabel", "tailtooltip",
                                   "tooltip")),
                               is.na(edges_df[i, j]))) {

                  attribute <- NULL
                } else if (all(!(colnames(edges_df)[j] %in%
                                 c("edgetooltip", "headtooltip",
                                   "label", "labeltooltip",
                                   "taillabel", "tailtooltip",
                                   "tooltip")),
                               edges_df[i, j] != '')) {
                  attribute <-
                    paste0(colnames(edges_df)[j],
                           " = ", "'", edges_df[i, j], "'")
                }
                attr_string <- c(attr_string, attribute)
              }

              if (j == other_columns_with_edge_attributes[
                length(other_columns_with_edge_attributes)]) {
                attr_string <- paste(attr_string, collapse = ", ")
              }
            }

            # Generate a line of edge objects when an
            # attribute string exists
            if (exists("attr_string")) {
              line <-
                paste0("'", edges_df[i, from_column], "'",
                       ifelse(directed, "->", "--"),
                       "'", edges_df[i, to_column], "'",
                       paste0(" [", attr_string, "] "))
            }

            # Generate a line of edge objects when an
            # attribute string doesn't exist
            if (!exists("attr_string")) {
              line <-
                paste0("  ",
                       "'", edges_df[i, from_column], "'",
                       ifelse(directed, "->", "--"),
                       "'", edges_df[i, to_column], "'",
                       " ")
            }
            edge_block <- c(edge_block, line)
          }
        }
      }

      # Construct the `edge_block` character object
      if (exists("edge_block")) {
        edge_block <- paste(edge_block, collapse = "\n")
      }

      if ("cluster" %in% colnames(nodes_df)) {

        # Get column number for column with node
        # attribute `cluster`
        cluster_colnum <-
          which(colnames(nodes_df) %in% "cluster")

        # Get list of clusters defined for the nodes
        cluster_ids <-
          which(
            unique(nodes_df$cluster) != "")

        for (i in seq_along(cluster_ids)) {

          regex <-
            stringr::str_replace(
              "'x'", "x",
              paste(nodes_df[which(nodes_df[, cluster_colnum] == i ), 1],
                    collapse = "'|'"))

          edge_block <-
            stringr::str_replace_all(edge_block, regex, paste0("'cluster_", i, "'"))

          regex <-
            paste0("('cluster_", i, "'->'cluster_", i, "' \n |",
                   "'cluster_", i, "'->'cluster_", i, "')")

          edge_block <-
            stringr::str_replace_all(edge_block, regex, "")
        }
      }
    }

    # Create the graph code from the chosen attributes,
    # and the nodes and edges blocks
    if (exists("combined_attr_stmts")) {
      if (exists("edge_block") & exists("node_block")) {
        combined_block <-
          paste(combined_attr_stmts,
                node_block, edge_block,
                sep = "\n")
      }
      if (!exists("edge_block") & exists("node_block")) {
        combined_block <-
          paste(combined_attr_stmts,
                node_block,
                sep = "\n")
      }
    }
    if (!exists("combined_attr_stmts")) {
      if (exists("edge_block")) {
        combined_block <- paste(node_block, edge_block,
                                sep = "\n")
      }
      if (!exists("edge_block")) {
        combined_block <- node_block
      }
    }

    # Create DOT code
    dot_code <-
      paste0(ifelse(directed, "digraph", "graph"),
             " {\n", "\n", combined_block, "\n}")

    # Remove empty node or edge attribute statements
    dot_code <- gsub(" \\[\\] ", "", dot_code)
  }

  return(dot_code)
}
