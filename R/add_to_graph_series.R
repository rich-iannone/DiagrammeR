#' Add graph object to a graph series object
#' Add a graph object to an extant graph series object for storage of multiple graphs across a sequential or temporal one-dimensional array.
#' @param graph a graph object to add to the graph series object.
#' @param graph_series a graph series object to which the graph object will be added.
#' @return a graph series object of type \code{dgr_graph_1D}.
#' @export add_to_graph_series

add_to_graph_series <- function(graph,
                                graph_series){

  # Get the series type
  series_type <- graph_series$series_type

  # Stop function if graph is not valid
  if (class(graph) != "dgr_graph"){

    return(graph_series)
  }

  # Stop function if graph series type is not valid
  if (!(series_type %in% c("sequential", "temporal"))){

    return(graph_series)
  }

  # If graph series type is 'sequential', add graph to series
  if (series_type == "sequential"){

    graph_series$graphs[[length(graph_series$graphs) + 1]] <- graph

    return(graph_series)
  }

  # For a graph series with a temporal type, determine if 'graph_time' and,
  # optionally, a 'graph_tz' value is provided
  if (series_type == "temporal"){

    is_time_provided <- ifelse(!is.null(graph$graph_time), TRUE, FALSE)
    is_tz_provided <- ifelse(!is.null(graph$graph_tz), TRUE, FALSE)

    if (is_time_provided == FALSE){

      return(graph_series)
    } else {

      if (is_tz_provided == FALSE){

        graph$graph_tz <- "GMT"
      }

      is_time_in_correct_format <-
        ifelse(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
                     graph$graph_time) |
                 grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}$",
                       graph$graph_time) |
                 grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$",
                       graph$graph_time), TRUE, FALSE)

      is_tz_in_correct_format <-
        ifelse(graph$graph_tz %in% OlsonNames(), TRUE, FALSE)

      if (is_time_in_correct_format == FALSE){

        return(graph_series)
      }

      if (is_tz_in_correct_format == FALSE){

        return(graph_series)
      }

      if (is_time_in_correct_format & is_tz_in_correct_format){

        graph_series$graphs[[length(graph_series$graphs) + 1]] <- graph

        return(graph_series)
      }
    }
  }
}
