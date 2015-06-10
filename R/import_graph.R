#' Import a graph from various graph formats
#' @description Import a variety of graphs from different graph formats and create a graph object.
#' @param graph_file a connection to a graph file.
#' @param graph_name an optional string for labeling the graph object.
#' @param graph_time a date or date-time string (required for insertion of graph into a graph series of the type \code{temporal}).
#' @param graph_tz an optional value for the time zone (\code{tz}) corresponding to the date or date-time string supplied as a value to \code{graph_time}. If no time zone is provided then it will be set to \code{GMT}.
#' @return a graph object of class \code{dgr_graph}.
#' @import stringr
#' @export import_graph

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

  }


}
