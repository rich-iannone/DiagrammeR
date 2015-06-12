#' Import a graph from various graph formats
#' @description Import a variety of graphs from different graph formats and create a graph object.
#' @param graph_file a connection to a graph file.
#' @param graph_name an optional string for labeling the graph object.
#' @param graph_time a date or date-time string (required for insertion of graph into a graph series of the type \code{temporal}).
#' @param graph_tz an optional value for the time zone (\code{tz}) corresponding to the date or date-time string supplied as a value to \code{graph_time}. If no time zone is provided then it will be set to \code{GMT}.
#' @return a graph object of class \code{dgr_graph}.
#' @import stringr


import_graph <- function(graph_file,
                         graph_name = NULL,
                         graph_time = NULL,
                         graph_tz = NULL){

  # Determine file existence
  file_exists <- file.exists(graph_file)

  # Obtain file extension
  file_extension <- gsub(".*\\.([a-zA-Z]*?)", "\\1", graph_file)

  # Determine file type from file extension
  if (file_extension == "graphml"){

    file_type <- "graphml"

  } else if (file_extension == "gml"){

    file_type <- "gml"

  } else if (file_extension == "gexf"){

    file_type <- "gexf"

  } else {

    file_type <- NA
  }

  if (file_type == "graphml"){

    # Read in the .graphml document as a vector object
    graphml_document <- readLines(graph_file)

    # Determine the starting and ending indices of the <node> tags
    xml_nodes <-
      list(node_start = grep("<node ", graphml_document),
           node_end = grep("</node>", graphml_document))

    # Determine the starting and ending indices of the <edge> tags
    xml_edges <-
      list(edge_start = grep("<edge ", graphml_document),
           edge_end = grep("</edge>", graphml_document))

    # Determine all node ID values for the graph
    for (i in 1:length(xml_nodes[[1]])){

      if (i == 1) nodes_ids <- vector(mode = "character")

      nodes_ids <-
        c(nodes_ids,
          str_replace_all(str_extract(graphml_document[xml_nodes[[1]][i]],
                                      "\".*?\""), "\"", ""))
    }

    # Determine indices that contain first node attributes
    node_key_indices <-
      xml_nodes[[1]][1] - 1 +
      grep("key", graphml_document[xml_nodes[[1]][1]:xml_nodes[[2]][1]])

    # Obtain names of keys
    node_key_names <-
      gsub(".*?\"(.*?)\".*", "\\1",
           graphml_document[node_key_indices])

    # Obtain list of vectors for all node attributes
    node_attributes <- list()

    for (i in 1:length(node_key_names)){

      for (j in 1:length(xml_nodes[[1]])){

        if (j == 1) attribute <- vector(mode = "character")

        attribute <-
          c(attribute,
            gsub(".*?>(.*?)<.*", "\\1", graphml_document[xml_nodes[[1]][j] + i]))

        if (j == length(xml_nodes[[1]])){
          node_attributes[[i]] <-  attribute
          names(node_attributes) <- node_key_names
        }
      }
    }

    # Create all nodes for graph
    all_nodes <- create_nodes(nodes = nodes_ids,
                              label = node_attributes[[which(node_key_names == "label")]],
                              x = node_attributes[[which(node_key_names == "x")]],
                              y = node_attributes[[which(node_key_names == "y")]])

    # Determine all edge values for the graph
    for (i in 1:length(xml_edges[[1]])){

      if (i == 1) {
        edges_from <- vector(mode = "character")
        edges_to <- vector(mode = "character")
      }

      edges_from <-
        c(edges_from,
          str_replace_all(unlist(str_extract_all(graphml_document[xml_edges[[1]][i]],
                                                 "\".*?\""))[1], "\"", ""))

      edges_to <-
        c(edges_to,
          str_replace_all(unlist(str_extract_all(graphml_document[xml_edges[[1]][i]],
                                                 "\".*?\""))[2], "\"", ""))
    }

    # Create all edges for graph
    all_edges <- create_edges(from = edges_from,
                              to = edges_to)

    # Create the graph
    the_graph <- create_graph(nodes_df = all_nodes,
                              edges_df = all_edges,
                              graph_name = graph_name,
                              graph_time = graph_time,
                              graph_tz = graph_tz)

    # Return the graph
    return(the_graph)
  }

  if (file_type == "gml"){

  }


}
