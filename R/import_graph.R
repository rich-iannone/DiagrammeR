#' Import a graph from various graph formats
#' @description Import a variety of graphs from different graph formats and
#' create a graph object.
#' @param graph_file a connection to a graph file.
#' @param file_type the type of file to be imported. Options are:
#' \code{graphml} (GraphML), \code{gml} (GML), and \code{sif} (SIF). If not
#' supplied, the function will infer the type by its file extension.
#' @param graph_name an optional string for labeling the graph object.
#' @param graph_time a date or date-time string (required for insertion of
#' graph into a graph series of the type \code{temporal}).
#' @param graph_tz an optional value for the time zone (\code{tz})
#' corresponding to the date or date-time string supplied as a value to
#' \code{graph_time}. If no time zone is provided then it will be set to
#' \code{GMT}.
#' @return a graph object of class \code{dgr_graph}.
#' @importFrom stringr str_extract str_detect str_split str_count
#' str_replace_all str_extract_all
#' @export import_graph

import_graph <- function(graph_file,
                         file_type = NULL,
                         graph_name = NULL,
                         graph_time = NULL,
                         graph_tz = NULL){

  # Stop function if file doesn't exist
  if (file.exists(graph_file) == FALSE){
    stop("The file as specified doesn't exist.")
  }

  # Stop function if 'file_type' specified is not part of the group
  # that can be imported
  if (!is.null(file_type)){
    if (!(tolower(file_type) %in% c("graphml", "gml", "sif"))){
      stop("The file type as specified cannot be imported.")
    }
  }

  # Obtain file extension if no value supplied for 'file_type'
  if (is.null(file_type)){
    file_extension <- gsub(".*\\.([a-zA-Z]*?)", "\\1", graph_file)

    # Determine file type from file extension
    if (file_extension == "graphml"){
      file_type <- "graphml"
    } else if (file_extension == "gml"){
      file_type <- "gml"
    } else if (file_extension == "sif"){
      file_type <- "sif"
    } else {
      stop("The file type is not known so it can't be imported.")
    }
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
        }
      }

      if (i == length(node_key_names)) names(node_attributes) <- node_key_names
    }

    # Create all nodes for graph
    all_nodes <-
      cbind(create_nodes(nodes = nodes_ids),
            data.frame(node_attributes))

    # Determine all edge values for the graph
    for (i in 1:length(xml_edges[[1]])){

      if (i == 1){
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
    all_edges <-
      create_edges(from = edges_from,
                   to = edges_to)

    # Create the graph
    the_graph <-
      create_graph(nodes_df = all_nodes,
                   edges_df = all_edges,
                   graph_name = graph_name,
                   graph_time = graph_time,
                   graph_tz = graph_tz,
                   node_attrs = c("shape = circle", "width = 10",
                                  "style = filled", "color = black"),
                   graph_attrs = "layout = neato",
                   generate_dot = FALSE)

    # Return the graph
    return(the_graph)
  }

  if (file_type == "gml"){

    # Read in the .gml document as a vector object
    gml_document <- paste(readLines(graph_file), collapse = "")

    # Extract information on whether graph is directed
    graph_directed <-
      unlist(
        str_replace_all(
          str_extract_all(gml_document, "directed [0-1]"),
          "directed ", ""))

    # Extract all node definitions
    node_defs <- unlist(str_extract_all(gml_document, "node[ ]*?\\[.*?\\]"))

    # Get all node ID values
    node_id <-
      str_replace_all(
        str_extract_all(
          node_defs,
          "id [a-z0-9_]*"),
        "id ", "")

    # Get all node label values, if they exist
    if (any(str_detect(node_defs, "label"))){
      node_label <-
        str_replace_all(
          str_replace_all(
            str_extract_all(node_defs,
                            "label \\\".*?\\\""),
            "label \"", ""),
          "\"", "")
    }

    # Extract all edge definitions
    edge_defs <- unlist(str_extract_all(gml_document, "edge[ ]*?\\[.*?\\]"))

    edges_from <-
      str_replace_all(
        str_extract_all(
          edge_defs,
          "source [a-z0-9_]*"),
        "source ", "")

    edges_to <-
      str_replace_all(
        str_extract_all(
          edge_defs,
          "target [a-z0-9_]*"),
        "target ", "")


    if (any(str_detect(edge_defs, "label"))){
      edge_label <-
        str_replace_all(
          str_replace_all(
            str_extract_all(edge_defs,
                            "label \\\".*?\\\""),
            "label \"", ""),
          "\"", "")
    }

    if (any(str_detect(edge_defs, "value"))){
      edge_value <-
        str_replace_all(
          str_extract_all(edge_defs,
                          "value [a-z0-9\\.]*"),
          "value ", "")
    }

    # Create all nodes for graph
    all_nodes <-
      create_nodes(nodes = node_id,
                   label = FALSE)

    if (exists("node_label")){
      all_nodes$label <- node_label
    }

    # Create all edges for graph
    all_edges <-
      create_edges(from = edges_from,
                   to = edges_to)

    if (exists("edge_value")){
      all_edges$data_value <- edge_value
    }

    # Create the graph
    the_graph <-
      create_graph(nodes_df = all_nodes,
                   edges_df = all_edges,
                   directed = ifelse(graph_directed == "1",
                                     TRUE, FALSE),
                   generate_dot = FALSE)

    # Return the graph
    return(the_graph)
  }

  if (file_type == "sif"){

    # Read in the SIF document as a vector object
    sif_document <- readLines(graph_file)

    # Initialize the vector for a node data frame
    nodes <- vector(mode = "character")

    # Determine which nodes are present in the graph
    for (i in 1:length(sif_document)){

      nodes <- c(nodes,
                 ifelse(length(unlist(str_split(sif_document[i], "\t"))) == 1,
                        unlist(str_split(sif_document[i], "\t"))[1],
                        unlist(str_split(sif_document[i], "\t"))[-2]))
    }

    # Obtain a unique vector of nodes in the graph
    nodes <- unique(nodes)

    # Create a node data frame
    nodes_df <- create_nodes(nodes = nodes)

    # Determine which lines have single nodes
    if (any(!str_detect(sif_document, "\\t"))){

      single_nodes <- which(!str_detect(sif_document, "\\t"))
    }

    # Initialize vectors for an edge data frame
    from <- to <- rel <- vector(mode = "character")

    # Obtain complete vectors for the edge data frame
    for (i in which(str_count(sif_document, "\\t") > 1)){

      length_stmt <- length(str_split(sif_document[i], "\t")[[1]])

      from <- c(from, str_split(sif_document[i], "\t")[[1]][1])

      rel <- c(rel, str_split(sif_document[i], "\t")[[1]][2])

      to <- c(to, str_split(sif_document[i], "\t")[[1]][3:length_stmt])
    }

    # Create an edge data frame
    edges_df <- create_edges(from = from,
                             to = to,
                             rel = rel)

    # Create a graph object
    the_graph <- create_graph(nodes_df = nodes_df,
                              edges_df = edges_df,
                              generate_dot = FALSE)

    # Return the graph
    return(the_graph)
  }
}
