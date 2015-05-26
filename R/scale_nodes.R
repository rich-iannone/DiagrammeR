#' Create numerical and color scales for node attributes
#' @description Generates either numeric or color scales for specified node attributes and applies those scales to node data frames.
#' @param nodes_df a data frame containing, at minimum, a column that contains node IDs for the graph. Optionally, additional columns (named as Graphviz node attributes) can be included with values for the named node attribute. These data frames can be conveniently generated using the \code{create_nodes} function.
#' @param to_scale a vector of numerical values serving as a basis for scaling; these currently need to be of the same length and order as the node IDs in the supplied node data frame, so, it's recommended that the value be a reference to a column of values residing in \code{nodes_df}.
#' @param node_attr the name of the node attribute for which scaled values are to be created.
#' @param range a vector of 2 elements providing either lower and upper numerical or X11 color values.
#' @param scale_type the type of scaling to perform. Currently, \code{linear} is the only option available.
#' @export scale_nodes

scale_nodes <- function(nodes_df,
                        to_scale,
                        node_attr,
                        range,
                        scale_type = "linear"){

  # Create vector of node attributes that take numeric values
  numeric_node_attributes <-
    c("fontsize", "height", "labelfontsize", "penwidth", "weight", "x", "y")

  # Create vector of node attributes that take color values
  color_node_attributes <- c("fillcolor", "fontcolor", "color")

  # Determine whether there is a valid numeric node attribute
  # in the statement
  is_num_node_attribute <- node_attr %in% numeric_node_attributes

  # Determine whether there is a valid color node attribute
  # in the statement
  is_col_node_attribute <- node_attr %in% color_node_attributes

  # Determine whether the attribute is an alpha attribute
  is_alpha_node_attribute <- ifelse(grepl("alpha.*", node_attr), TRUE, FALSE)

  if (is_num_node_attribute){

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
    nodes_df <- cbind(nodes_df, normalized_df)

    # Change temporary name of column to that of attribute chosen
    colnames(nodes_df)[which(names(nodes_df) %in% "normalized")] <- node_attr

    return(nodes_df)
  }

  if (is_col_node_attribute){

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
    nodes_df <- cbind(nodes_df, normalized_df)

    # Change temporary name of column to that of attribute chosen
    colnames(nodes_df)[which(names(nodes_df) %in% "normalized")] <- node_attr

    return(nodes_df)
  }

  if (is_alpha_node_attribute){

    # Parse statement and determine if the alpha values should be applied to
    # a color attribute, or, whether a new color attribute should be created
    # before applying that alpha

    if (grepl(":", node_attr)){
      additional_stmt <- unlist(strsplit(node_attr, ":"))[-1]
    }

    if (exists("additional_stmt") & grepl("=", additional_stmt)){
      attr_assign <- gsub(" ", "", unlist(strsplit(additional_stmt, "=")))
    }

    if (exists("additional_stmt") & grepl("=", additional_stmt) == FALSE){
      attr_assign <- gsub(" ", "", additional_stmt)
    }

    # Add column for color-type attr in nodes_df
    if (exists("attr_assign")){

      if (length(attr_assign) == 2){

        if (attr_assign[1] %in% color_node_attributes){

          attr_in_df <- attr_assign[1] %in% colnames(nodes_df)

          if (attr_in_df == FALSE){

            color_attr <- rep(attr_assign[2], nrow(nodes_df))

            nodes_df <- cbind(nodes_df, color_attr)

            colnames(nodes_df)[length(nodes_df)] <- attr_assign[1]

            apply_to_column_no <- which(colnames(nodes_df) %in% attr_assign[1])
          }
        }
      }

      if (length(attr_assign) == 1){

        if (attr_assign[1] %in% color_node_attributes){

          attr_in_df <- attr_assign[1] %in% colnames(nodes_df)

          if (attr_in_df){

            apply_to_column_no <- which(colnames(nodes_df) %in% attr_assign[1])
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
    nodes_df <- cbind(nodes_df, normalized_df)

    # Change temporary name of column to that of attribute chosen
    colnames(nodes_df)[which(names(nodes_df) %in% "normalized")] <-
      paste0("alpha_", attr_assign[1])

    return(nodes_df)
  }
}
