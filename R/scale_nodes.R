scale_nodes <- function(nodes_df,
                        to_scale,
                        node_attr,
                        range,
                        scale_type = "linear"){

  # Create vector of node attributes that take numeric values
  numeric_node_attributes <-
    c("fontsize", "height", "labelfontsize", "penwidth", "weight")

  # Create vector of node attributes that take color values
  color_node_attributes <- c("fillcolor", "fontcolor")

  # Determine whether there is a valid numeric node attribute
  # in the statement
  is_num_node_attribute <- node_attr %in% numeric_node_attributes

  # Determine whether there is a valid color node attribute
  # in the statement
  is_col_node_attribute <- node_attr %in% color_node_attributes

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

      ct <- new_context("window")
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
}
