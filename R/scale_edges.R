#' Create numerical and color scales for edge attributes
#' Generates either numeric or color scales for specified edge attributes and applies those scales to edge data frames.
#' @param edges_df a data frame containing, at minimum, a column (called \code{edge_op}) with edge operations as character strings (in the form of \code{[node_id] -> [node_id]}). Alternatively, there may be two columns where node IDs specifying edges are provided.
#' @param to_scale a vector of numerical values to be scaled; these currently need to be of the same length and order as the edge operations in the supplied edge data frame, so, it's recommended to reference a column of values available in \code{edges_df}.
#' @param edge_attr the name of the edge attribute for which scaled values are to be created.
#' @param range a vector of 2 elements providing either lower and upper numerical or X11 color values.
#' @param scale_type the type of scaling to perform. Currently, \code{linear} is the only option available.
#' @export scale_edges

scale_edges <- function(edges_df,
                        to_scale,
                        edge_attr,
                        range,
                        scale_type = "linear"){

  # Create vector of edge attributes that take numeric values
  numeric_edge_attributes <-
    c("arrowsize", "fontsize", "labelangle", "labeldistance", "labelfontsize",
      "minlen", "penwidth", "weight")

  # Create vector of edge attributes that take color values
  color_edge_attributes <- c("color", "fontcolor", "labelfontcolor")

  # Determine whether there is a valid numeric edge attribute
  # in the statement
  is_num_edge_attribute <- edge_attr %in% numeric_edge_attributes

  # Determine whether there is a valid color edge attribute
  # in the statement
  is_col_edge_attribute <- edge_attr %in% color_edge_attributes

  # Determine whether the attribute is an alpha attribute
  is_alpha_edge_attribute <- ifelse(grepl("alpha.*", edge_attr), TRUE, FALSE)

  if (is_num_edge_attribute){

    # Stop function if the length of the numeric vector is not 2
    stopifnot(length(range) == 2)

    # Obtain the min and max values for the data to normalize
    num_range_min_max <- c(min(to_scale), max(to_scale))

    # Get normalized values for attribute if "linear" option chosen
    if (scale_type == "linear"){

      normalized <- range[1] +
        ((to_scale - num_range_min_max[1]) *
           (range[2] - range[1])) /
        (num_range_min_max[2] - num_range_min_max[1])
    }

    # Create data frame for merging
    normalized_df <- data.frame(normalized)

    # Column bind both data frames together
    edges_df <- cbind(edges_df, normalized_df)

    # Change temporary name of column to that of attribute chosen
    colnames(edges_df)[which(names(edges_df) %in% "normalized")] <- edge_attr

    return(edges_df)
  }

  if (is_col_edge_attribute){

    # Stop function if the length of the character vector is not 2
    stopifnot(length(range) == 2)

    # If the colors are named colors, then transform to hex
    if (all(range %in% x11_hex()[,1])){

      for (i in 1:length(range)){

        if (i == 1) hex_color_values <- vector(mode = 'character', length = 0)

        a_hex_color <- x11_hex()[which(x11_hex()[,1] %in% range[i]),2]

        hex_color_values <- c(hex_color_values, a_hex_color)
      }
    }

    # Obtain the min and max values for the data to normalize
    num_range_min_max <- c(min(to_scale), max(to_scale))

    # Obtain 100 colors within the color range provided
    number_of_stops <- 100

    for (j in 1:number_of_stops){

      if (j == 1) hex_colors <- vector(mode = "character", length = 0)

      js_call <- paste0("chromato.interpolate('", hex_color_values[1], "', '",
                        hex_color_values[2], "', ",
                        j/number_of_stops, ", 'hsl');")

      ct <- V8::new_context("window")
      invisible(ct$source(system.file("htmlwidgets/lib/chromatography/chromatography.js",
                                      package = "DiagrammeR")))

      hex_colors <- c(hex_colors, unlist(strsplit(ct$eval(js_call), ",")))

      if (j == number_of_stops){
        fractional_hex_colors <-
          rbind(data.frame(fraction = 0.0, hex_colors = hex_colors[1],
                           stringsAsFactors = FALSE),
                data.frame(fraction = seq(from = 1, to = 100, by = 1)/100,
                           hex_colors = hex_colors, stringsAsFactors = FALSE))
      }
    }

    # Get normalized values for attribute
    for (k in 1:length(to_scale)){
      if (k == 1) normalized <- vector(mode = 'character', length = 0)

      a_hex_color <-
        fractional_hex_colors[which(fractional_hex_colors[,1] ==
                                      round((to_scale - min(to_scale))/
                                              (max(to_scale) - min(to_scale)),
                                            digits = 2)[k]), 2]

      normalized <- c(normalized, a_hex_color)
    }

    # Create data frame for merging
    normalized_df <- data.frame(normalized)

    # Column bind both data frames together
    edges_df <- cbind(edges_df, normalized_df)

    # Change temporary name of column to that of attribute chosen
    colnames(edges_df)[which(names(edges_df) %in% "normalized")] <- edge_attr

    return(edges_df)
  }

  if (is_alpha_edge_attribute){

    # Parse statement and determine if the alpha values should be applied to
    # a color attribute, or, whether a new color attribute should be created
    # before applying that alpha

    if (grepl(":", edge_attr)){
      additional_stmt <- unlist(strsplit(edge_attr, ":"))[-1]
    }

    if (exists("additional_stmt") & grepl("=", additional_stmt)){
      attr_assign <- gsub(" ", "", unlist(strsplit(additional_stmt, "=")))
    }

    if (exists("additional_stmt") & grepl("=", additional_stmt) == FALSE){
      attr_assign <- gsub(" ", "", additional_stmt)
    }

    # Add column for color-type attr in edges_df
    if (exists("attr_assign")){

      if (length(attr_assign) == 2){

        if (attr_assign[1] %in% color_edge_attributes){

          attr_in_df <- attr_assign[1] %in% colnames(edges_df)

          if (attr_in_df == FALSE){

            color_attr <- rep(attr_assign[2], nrow(edges_df))

            edges_df <- cbind(edges_df, color_attr)

            colnames(edges_df)[length(edges_df)] <- attr_assign[1]

            apply_to_column_no <- which(colnames(edges_df) %in% attr_assign[1])

          }
        }
      }

      if (length(attr_assign) == 1){

        if (attr_assign[1] %in% color_edge_attributes){

          attr_in_df <- attr_assign[1] %in% colnames(edges_df)

          if (attr_in_df){

            apply_to_column_no <- which(colnames(edges_df) %in% attr_assign[1])

          }
        }
      }
    }

    # Obtain the min and max values for the data to normalize
    num_range_min_max <- c(min(to_scale), max(to_scale))

    # Get normalized values for attribute if "linear" option chosen
    if (scale_type == "linear"){

      normalized <- range[1] +
        ((to_scale - num_range_min_max[1]) *
           (range[2] - range[1])) /
        (num_range_min_max[2] - num_range_min_max[1])
    }

    # Create data frame for merging
    normalized_df <- data.frame(normalized)

    # Column bind both data frames together
    edges_df <- cbind(edges_df, normalized_df)

    # Change temporary name of column to that of attribute chosen
    colnames(edges_df)[which(names(edges_df) %in% "normalized")] <-
      paste0("alpha_", attr_assign[1])

    return(edges_df)
  }

}
