#' Create DOT code from a data frame
#' A function to generate DOT code from a single data frame
#' @param df the data frame object from which node and edge statements in DOT notation are to be generated.
#' @param edge_between a vector object containing statements that provide information on the relationships between nodes in different columns. The basic syntax takes the form of: "df_column_name_1 [->|--] df_column_name_2".
#' @param node_attr a vector object containing statements for node attributes.
#' @param edge_attr a vector object containing statements for edge attributes.
#' @param add_labels whether to automatically generate a set of node and edge labels based on the node ID and the edge operation, respectively.
#' @export graphviz_single_df

graphviz_single_df <- function(df,
                               edge_between,
                               node_attr = NULL,
                               edge_attr = NULL,
                               add_labels = FALSE){

  # Clean up 'node_attr' statement, if it is provided
  if (exists("node_attr")){
    node_attr <- gsub(",([a-z])", ", \\1", gsub("\\n ", "", gsub("[ ]+", " ", node_attr)))
  }

  # Clean up 'edge_attr' statement, if it is provided
  if (exists("edge_attr")){
    edge_attr <- gsub(",([a-z])", ", \\1", gsub("\\n ", "", gsub("[ ]+", " ", edge_attr)))
  }

  # Extract the column names that serve as nodes
  edge_between_elements <- gsub(" ", "",
                                unlist(strsplit(edge_between, "-[-|>]")))

  # Add function 'strcount' to perform a count of pattern occurances in a string
  strcount <- function(x, pattern, split){
    unlist(lapply(
      strsplit(x, split),
      function(z) na.omit(length(grep(pattern, z)))))
  }

  # Add function 'combine_vector_contents' to combine vector contents
  combine_vector_contents <- function(vector_1, vector_2){
    if (length(vector_1) == length(vector_2)){
      for (i in 1:length(vector_1)){
        if (i == 1) new_vector <- vector(mode = "character",
                                         length = length(vector_1))
        if (vector_1[i] != "") new_vector[i] <- vector_1[i]
        if (vector_2[i] != "") new_vector[i] <- vector_2[i]
      }
      return(new_vector)
    }
  }

  # Create list of node attributes, parsed from 'node_attr' input
  if (!is.null(node_attr)){
    for (i in 1:length(node_attr)){
      if (i == 1) {
        node_attr_values <- vector("list", length(node_attr))
        scaled_node_attr_df <- data.frame(mat.or.vec(nr = nrow(df), nc = 0))
      }

      node_attr_values[[i]] <- gsub("^(([\\w|\\+])*).*", "\\1", node_attr[i], perl = TRUE)

      node_col_num_in_df <- which(colnames(df) == node_attr_values[[i]][1])

      for (j in 1:(strcount(node_attr[i], ",", "") + 1)){

        if (grepl('=',
                  unlist(strsplit(gsub(paste0("^",
                                              gsub("\\+", "\\\\+",
                                                   node_attr_values[[i]][1]),
                                              ":"),
                                       "", node_attr[i]), ","))[j]) == FALSE){

          # Create 'statement' object for parsing
          statement <-
            gsub("^ ", "",
                 unlist(strsplit(gsub(paste0("^",
                                             gsub("\\+", "\\\\+",
                                                  node_attr_values[[i]][1]),
                                             ":"),
                                      "", node_attr[i]), ","))[j])

      }
    }
  }

  # Create list of edge attributes, parsed from 'edge_attr' input
  if (!is.null(edge_attr)){
    for (i in 1:length(edge_attr)){
      if (i == 1){
        edge_attr_values <- vector("list", length(edge_attr))
        scaled_edge_attr_df <- data.frame(mat.or.vec(nr = nrow(df), nc = 0))
      }

      edge_attr_values[[i]] <- gsub("^(([\\w|\\+])*).*", "\\1",
                                    edge_attr[i], perl = TRUE)

      for (j in 1:(strcount(edge_attr[i], ",", "") + 1)){

        if (grepl('=',
                  unlist(strsplit(gsub(paste0("^",
                                              gsub("\\+", "\\\\+",
                                                   edge_attr_values[[i]][1]),
                                              ":"),
                                       "", edge_attr[i]), ","))[j]) == FALSE){

          # Create 'statement' object for parsing
          statement <-
            gsub("^ ", "",
                 unlist(strsplit(gsub(paste0("^",
                                             gsub("\\+", "\\\\+",
                                                  edge_attr_values[[i]][1]),
                                             ":"),
                                      "", edge_attr[i]), ","))[j])

          # Obtain the edge attribute from the statement
          edge_attribute <- gsub("^([a-z]*).*", "\\1", statement, perl = TRUE)

          # Create vector of edge attributes that take numeric values
          numeric_edge_attributes <-
            c("arrowhead", "arrowsize", "arrowtail", "fontsize",
              "labelangle", "labeldistance", "labelfontsize",
              "minlen", "penwidth", "weight")

          # Create vector of edge attributes that take color values
          color_edge_attributes <-
            c("color", "fontcolor", "labelfontcolor")

          # Determine whether there is a valid numeric edge attribute
          # in the statement
          is_num_edge_attribute <- edge_attribute %in% numeric_edge_attributes

          # Determine whether there is a valid color edge attribute
          # in the statement
          is_col_edge_attribute <- edge_attribute %in% color_edge_attributes

          if (is_num_edge_attribute){

            # Get numeric vector for the given normalization limits
            num_range_given <-
              as.numeric(unlist(strsplit(gsub(paste0(edge_attribute,
                                                     " ([0-9\\.]*) to ([0-9\\.]*).*"),
                                              "\\1 \\2",
                                              statement, perl = TRUE), " ")))

            # Validate the length of the numeric vector
            num_range_given_correct_length <- length(num_range_given) == 2

            # Obtain the column name in df that the attr should be scaled against
            comparison_col <- gsub(paste0(edge_attribute,
                                          " [0-9\\.]* to [0-9\\.]* with (\\w)"),
                                   "\\1",
                                   statement, perl = TRUE)

            # Validate that the comparison column exists in the data frame
            comparison_col_in_df <- comparison_col %in% colnames(df)

            # Obtain the column number in the data frame
            comparison_col_num <- which(colnames(df) == comparison_col)

            # Obtain the min and max values for the data to normalize
            num_range_df_col <- c(min(df[, which(colnames(df) == comparison_col)],
                                      na.rm = TRUE),
                                  max(df[, which(colnames(df) == comparison_col)],
                                      na.rm = TRUE))

            # Get normalized values for attribute
            normalized <- num_range_given[1] +
              ((df[,comparison_col_num] - num_range_df_col[1]) *
                 (num_range_given[2] - num_range_given[1])) /
              (num_range_df_col[2] - num_range_df_col[1])

            # Create a data frame of 1 column with normalized data
            scaled_edge_attr_df_col <- data.frame(normalized)
            colnames(scaled_edge_attr_df_col) <- edge_attribute

            # Add data frame column to 'scaled_edge_attr_df'
            scaled_edge_attr_df <- cbind(scaled_edge_attr_df, scaled_edge_attr_df_col)

          }

          if (is_col_edge_attribute){

            # Get vector of colors for the given normalization limits
            col_range_given <-
              unlist(strsplit(gsub(paste0(edge_attribute,
                                          " ([a-zA-Z]*) to ([a-zA-Z]*).*"),
                                   "\\1 \\2",
                                   statement, perl = TRUE), " "))

            # If the colors are named colors, then transform to hex
            if (all(col_range_given %in% x11_hex()[,1])){

              for (k in 1:length(col_range_given)){
                if (k == 1) hex_color_values <- vector(mode = 'character', length = 0)

                a_hex_color <- x11_hex()[which(x11_hex()[,1] %in% col_range_given[k]),2]

                hex_color_values <- c(hex_color_values, a_hex_color)
              }
            }

            # Obtain the column name in df that the attr should be scaled against
            comparison_col <- gsub(paste0(edge_attribute,
                                          " [a-zA-Z]* to [a-zA-Z]* with (\\w)"),
                                   "\\1",
                                   statement, perl = TRUE)

            # Validate that the comparison column exists in the data frame
            comparison_col_in_df <- comparison_col %in% colnames(df)

            # Obtain the column number in the data frame
            comparison_col_num <- which(colnames(df) == comparison_col)

            # Obtain the min and max values for the data to normalize
            num_range_df_col <- c(min(df[, which(colnames(df) == comparison_col)],
                                      na.rm = TRUE),
                                  max(df[, which(colnames(df) == comparison_col)],
                                      na.rm = TRUE))

            # Obtain 100 colors within the color range provided
            number_of_stops <- 100

            for (i in 1:number_of_stops){

              if (i == 1) hex_colors <- vector(mode = "character", length = 0)

              js_call <- paste0("chromato.interpolate('", hex_color_values[1], "', '",
                                hex_color_values[2], "', ",
                                i/number_of_stops, ", 'hsl');")

              ct <- new_context("window")
              invisible(ct$source(system.file("htmlwidgets/lib/chromatography/chromatography.js",
                                              package = "DiagrammeR")))

              hex_colors <- c(hex_colors, unlist(strsplit(ct$eval(js_call), ",")))

              if (i == number_of_stops){
                fractional_hex_colors <-
                  rbind(data.frame(fraction = 0.0, hex_colors = hex_colors[1],
                                   stringsAsFactors = FALSE),
                        data.frame(fraction = seq(from = 1, to = 100, by = 1)/100,
                                   hex_colors = hex_colors, stringsAsFactors = FALSE))
              }
            }

            # Get normalized values for attribute
            for (z in 1:nrow(df)){
              if (z == 1) normalized <- vector(mode = 'character', length = 0)

              a_hex_color <-
                fractional_hex_colors[which(fractional_hex_colors[,1] ==
                        round(df[,comparison_col_num]/
                                (num_range_df_col[2] - num_range_df_col[1]),
                              digits = 2)[z]),2]

              normalized <- c(normalized, a_hex_color)
            }

            # Create a data frame of 1 column with normalized data
            scaled_edge_attr_df_col <- data.frame(normalized)
            colnames(scaled_edge_attr_df_col) <- edge_attribute

            # Add data frame column to 'scaled_edge_attr_df'
            scaled_edge_attr_df <- cbind(scaled_edge_attr_df, scaled_edge_attr_df_col)
          }

        } else {
          edge_attr_values[[i]][j + 1] <-
            gsub("=", " = ",
                 gsub(" ", "",
                      unlist(strsplit(gsub(paste0("^",
                                                  gsub("\\+", "\\\\+",
                                                       edge_attr_values[[i]][1]),
                                                  ":"),
                                           "", edge_attr[i]), ","))))[j]
        }
      }
    }
  }

  # Determine whether column contents should be concatenated to generate
  # possibly more unique strings
  if (any(grepl("\\+", edge_between_elements, perl = TRUE)) == TRUE){

    # Determine which columns are to be concatenated to make one or
    # more synthetic IDs
    left_side_columns <-
      gsub(" ", "", unlist(strsplit(edge_between_elements[1], "\\+")))

    right_side_columns <-
      gsub(" ", "", unlist(strsplit(edge_between_elements[2], "\\+")))

    stopifnot(any(left_side_columns %in% colnames(df)))
    stopifnot(any(right_side_columns %in% colnames(df)))

    ls_cols <- which(colnames(df) %in% left_side_columns)
    rs_cols <- which(colnames(df) %in% right_side_columns)

    for (i in 1:nrow(df)){
      if (i == 1) {
        ls_synthetic <- vector(mode = "character", length = 0)
        rs_synthetic <- vector(mode = "character", length = 0)
      }

      if (length(ls_cols) > 1){
        ls_synthetic <-
          c(ls_synthetic,
            paste(df[i,ls_cols], collapse = "__"))
        ls_origin <- paste(colnames(df[i,ls_cols]), collapse = "+")
        rs_origin <- right_side_columns
      } else {
        if (exists("ls_synthetic")){
          rm(ls_synthetic)
        }
      }

      if (length(rs_cols) > 1){
        rs_synthetic <-
          c(rs_synthetic,
            paste(df[i,rs_cols], collapse = "__"))
        rs_origin <- paste(colnames(df[i,rs_cols]), collapse = "+")
        ls_origin <- left_side_columns
      } else {
        if (exists("rs_synthetic")){
          rm(rs_synthetic)
        }
      }

      if (i == nrow(df)){
        if (exists("ls_synthetic") & !exists("rs_synthetic")){
          node_id <- gsub("'", "_",
                          c(unique(ls_synthetic), unique(df[,rs_cols])))
          origin_id <- c(rep(ls_origin, length(unique(ls_synthetic))),
                         rep(rs_origin, length(unique(df[,rs_cols]))))

        }

        if (exists("rs_synthetic") & !exists("ls_synthetic")){
          node_id <- gsub("'", "_",
                          c(unique(rs_synthetic), unique(df[,ls_cols])))
          origin_id <- c(rep(ls_origin, length(unique(df[,ls_cols]))),
                         rep(rs_origin, length(unique(rs_synthetic))))
        }

        if (exists("ls_synthetic") & exists("rs_synthetic")){
          node_id <- gsub("'", "_",
                          c(unique(ls_synthetic), unique(rs_synthetic)))
          origin_id <- c(rep(ls_origin, length(unique(ls_synthetic))),
                         rep(rs_origin, length(unique(rs_synthetic))))
        }
      }
    }

    # Create the 'nodes_df' data frame, optionally adding a 'label' column
    if (add_labels == TRUE){
      label <- gsub("'", "&#39;", node_id)
      nodes_df <- data.frame(node_id = node_id,
                             origin_id = origin_id,
                             label = label,
                             stringsAsFactors = FALSE)
    } else {
      nodes_df <- data.frame(node_id = node_id,
                             origin_id = origin_id,
                             stringsAsFactors = FALSE)
    }

    # Create the necessary attributes columns in 'nodes_df'
    if (class(node_attr_values) == "list" & length(node_attr_values) > 0){

      for (i in 1:length(node_attr_values)){
        for (j in 2:length(node_attr_values[[i]])){

          column_name <- gsub("^([a-z]*) =.*", "\\1", node_attr_values[[i]][j])
          attr_value <- gsub("^[a-z]* = (.*)", "\\1", node_attr_values[[i]][j])

          for (k in 1:nrow(nodes_df)){
            if (k == 1) col_vector <- vector(mode = "character", length = nrow(nodes_df))

            col_vector[k] <-
              ifelse(nodes_df[k, colnames(nodes_df) == "origin_id"] == node_attr_values[[i]][1],
                     attr_value, "")
          }

          if (!(column_name %in% colnames(nodes_df))){
            nodes_df <- as.data.frame(cbind(nodes_df, as.character(col_vector)),
                                      stringsAsFactors = FALSE)
            colnames(nodes_df)[length(nodes_df)] <- column_name

          }

          if (column_name %in% colnames(nodes_df)){
            nodes_df[, which(colnames(nodes_df) == column_name)] <-
              combine_vector_contents(nodes_df[, which(colnames(nodes_df) == column_name)],
                                      col_vector)
          }
        }
      }
    }

    # Create the 'edges_df' data frame
    for (i in 1:nrow(df)){
      if (i == 1){
        edge_from <- vector(mode = "character", length = 0)
        edge_to <- vector(mode = "character", length = 0)
      }

      # If the left side uses a synthetic ID
      if (exists("ls_synthetic") & !exists("rs_synthetic")){

        edge_from_row <- gsub("'", "_", ls_synthetic[i])
        edge_from <- c(edge_from, edge_from_row)

        edge_to_row <- gsub("'", "_", df[i,edge_between_elements[2]])
        edge_to <- c(edge_to, edge_to_row)

      }

      # If the right side uses a synthetic ID
      if (exists("rs_synthetic") & !exists("ls_synthetic")){

        edge_from_row <- gsub("'", "_", df[i,edge_between_elements[1]])
        edge_from <- c(edge_from, edge_from_row)

        edge_to_row <- gsub("'", "_", rs_synthetic[i])
        edge_to <- c(edge_to, edge_to_row)

      }

      # If both the left and right sides use synthetic IDs
      if (exists("ls_synthetic") & exists("rs_synthetic")){

        edge_from_row <- gsub("'", "_", ls_synthetic[i])
        edge_from <- c(edge_from, edge_from_row)

        edge_to_row <- gsub("'", "_", rs_synthetic[i])
        edge_to <- c(edge_to, edge_to_row)

      }

      if (i == nrow(df)){
        edges_df <- data.frame(edge_from, edge_to)
      }
    }

    # Create the necessary attributes columns in 'edges_df'
    if (class(edge_attr_values) == "list" & length(edge_attr_values) > 0){

      for (i in 1:length(edge_attr_values)){
        for (j in 2:length(edge_attr_values[[i]])){

          column_name <- gsub("^([a-z]*) =.*", "\\1", edge_attr_values[[i]][j])
          attr_value <- gsub("^[a-z]* = (.*)", "\\1", edge_attr_values[[i]][j])

          col_vector <- rep(attr_value, nrow(edges_df))

          if (!(column_name %in% colnames(edges_df))){
            edges_df <- as.data.frame(cbind(edges_df, as.character(col_vector)),
                                      stringsAsFactors = FALSE)
            colnames(edges_df)[length(edges_df)] <- column_name

          }

          if (column_name %in% colnames(edges_df)){
            edges_df[, which(colnames(edges_df) == column_name)] <-
              combine_vector_contents(edges_df[, which(colnames(edges_df) == column_name)],
                                      col_vector)
          }
        }
      }
    }

  } else {

    # Determine column indices for the node columns
    node_cols <- which(colnames(df) %in% edge_between_elements)

    # Create vector of node IDs
    for (i in 1:length(node_cols)){
      if (i == 1) node_id <- vector(mode = "character", length = 0)
      node_id_part <- unique(as.character(unlist(df[,node_cols[i]],
                                                 use.names = FALSE)))

      node_id <- c(node_id, node_id_part)
    }

    # Replace apostrophes with underscore characters
    node_id <- gsub("'", "_", node_id)

    # Get unique values for each of the columns and use as labels
    #node_id <- gsub("'", "_", unique(as.character(unlist(df[,node_cols],
    #                                                     use.names = FALSE))))

    left_side_column <- edge_between_elements[1]
    right_side_column <- edge_between_elements[2]

    stopifnot(any(left_side_column %in% colnames(df)))
    stopifnot(any(right_side_column %in% colnames(df)))

    ls_col <- which(colnames(df) %in% left_side_column)
    rs_col <- which(colnames(df) %in% right_side_column)

    ls_origin <- left_side_column
    rs_origin <- right_side_column

    ls_times <- length(unique(df[,ls_col]))
    rs_times <- length(unique(df[,rs_col]))

    # Create the 'origin_id' vector
    origin_id <- c(rep(ls_origin, ls_times),
                   rep(rs_origin, rs_times))

    # Create the 'nodes_df' data frame, optionally adding a 'label' column
    if (add_labels == TRUE){
      label <- gsub("'", "&#39;", unique(as.character(unlist(df[,node_cols],
                                                             use.names = FALSE))))
      nodes_df <- data.frame(node_id = node_id,
                             origin_id = origin_id,
                             label = label)
    } else {
      nodes_df <- data.frame(node_id = node_id,
                             origin_id = origin_id)
    }

    # Create the necessary attributes columns in 'nodes_df'
    if (class(node_attr_values) == "list" & length(node_attr_values) > 0){

      for (i in 1:length(node_attr_values)){
        for (j in 2:length(node_attr_values[[i]])){

          column_name <- gsub("^([a-z]*) =.*", "\\1", node_attr_values[[i]][j])
          attr_value <- gsub("^[a-z]* = (.*)", "\\1", node_attr_values[[i]][j])

          for (k in 1:nrow(nodes_df)){
            if (k == 1) col_vector <- vector(mode = "character", length = nrow(nodes_df))

            col_vector[k] <-
              ifelse(nodes_df[k, colnames(nodes_df) == "origin_id"] == node_attr_values[[i]][1],
                     attr_value, "")
          }

          if (!(column_name %in% colnames(nodes_df))){
            nodes_df <- as.data.frame(cbind(nodes_df, as.character(col_vector)),
                                      stringsAsFactors = FALSE)
            colnames(nodes_df)[length(nodes_df)] <- column_name

          }

          if (column_name %in% colnames(nodes_df)){
            nodes_df[, which(colnames(nodes_df) == column_name)] <-
              combine_vector_contents(nodes_df[, which(colnames(nodes_df) == column_name)],
                                      col_vector)
          }
        }
      }
    }

    # Create the 'edges_df' data frame
    for (i in 1:nrow(df)){
      if (i == 1){
        edge_from <- vector(mode = "character", length = 0)
        edge_to <- vector(mode = "character", length = 0)
      }

      edge_from_row <- gsub("'", "_", df[i,edge_between_elements[1]])
      edge_from <- c(edge_from, edge_from_row)

      edge_to_row <- gsub("'", "_", df[i,edge_between_elements[2]])
      edge_to <- c(edge_to, edge_to_row)

      if (i == nrow(df)){
        edges_df <- data.frame(edge_from, edge_to)
      }
    }

    # Create the necessary attributes columns in 'edges_df'
    if (class(edge_attr_values) == "list" & length(edge_attr_values) > 0){

      for (i in 1:length(edge_attr_values)){
        for (j in 2:length(edge_attr_values[[i]])){

          column_name <- gsub("^([a-z]*) =.*", "\\1", edge_attr_values[[i]][j])
          attr_value <- gsub("^[a-z]* = (.*)", "\\1", edge_attr_values[[i]][j])

          col_vector <- rep(attr_value, nrow(edges_df))

          if (!(column_name %in% colnames(edges_df))){
            edges_df <- as.data.frame(cbind(edges_df, as.character(col_vector)),
                                      stringsAsFactors = FALSE)
            colnames(edges_df)[length(edges_df)] <- column_name

          }

          if (column_name %in% colnames(edges_df)){
            edges_df[, which(colnames(edges_df) == column_name)] <-
              combine_vector_contents(edges_df[, which(colnames(edges_df) == column_name)],
                                      col_vector)
          }
        }
      }
    }

    # Add in additional attr columns to 'edges_df'
    edges_df <- cbind(edges_df, scaled_edge_attr_df)
  }

  # Extract information on the relationship between nodes
  if (grepl("->", edge_between)){
    directed <- TRUE
  } else if (grepl("--", edge_between)){
    directed <- FALSE
  } else {
    directed <- FALSE
  }

  # Generate the combined node and edge block for insertion into the
  # Graphviz DOT statement
  combined_block <-
    graphviz_nodes_edges_df(nodes_df = nodes_df,
                            edges_df = edges_df,
                            directed = directed)

  return(combined_block)
}
