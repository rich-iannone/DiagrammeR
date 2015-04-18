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

}
