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


}
