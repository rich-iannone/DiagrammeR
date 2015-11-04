#' Add graph object to a graph series object
#' @description Add a graph object to an extant graph series object for
#' storage of multiple graphs across a sequential or temporal one-dimensional
#' array.
#' @param graph a graph object to add to the graph series object.
#' @param graph_series a graph series object to which the graph object will be
#' added.
#' @return a graph series object of type \code{dgr_graph_1D}.
#' @examples
#' \dontrun{
#' # Create three graphs (using \code{pipeR} for speed)
#' # and create a graph series using those graphs
#' library(magrittr)
#'
#' graph_1 <- create_graph() %>%
#'   add_node("a") %>% add_node("b") %>% add_node("c") %>%
#'   add_edge("a", "c") %>% add_edge("a", "b") %>% add_edge("b", "c")
#'
#' graph_2 <- graph_1 %>%
#'   add_node("d") %>% add_edge("d", "c")
#'
#' graph_3 <- graph_2 %>%
#'   add_node("e") %>% add_edge("e", "b")
#'
#' # Create an empty graph series
#' series <- create_series(series_type = "sequential")
#'
#' # Add graphs to the graph series
#' series <- graph_1 %>% add_to_series(series)
#' series <- graph_2 %>% add_to_series(series)
#' series <- graph_3 %>% add_to_series(series)
#' }
#' @export add_to_series

add_to_series <- function(graph,
                          graph_series){

  # Get the series type
  series_type <- graph_series$series_type

  # Stop function if graph is not valid
  if (class(graph) != "dgr_graph"){

    stop("The supplied graph object is not valid.")
  }

  # Stop function if graph series type is not valid
  if (!(series_type %in% c("sequential", "temporal"))){

    stop("The graph series type is neither 'sequential' nor 'temporal'")
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

    # Stop function if no time information available in a graph to be
    # added to a graph series of the 'temporal' type
    if (is_time_provided == FALSE){

      stop("No time information is provided in this graph object.")
    } else {

      # If time zone not provided, automatically provide the "GMT" time zone
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

        stop("The time provided in this graph object is not in the correct format.")
      }

      if (is_tz_in_correct_format == FALSE){

        stop("The time zone provided in this graph object is not in the correct format.")
      }

      if (is_time_in_correct_format & is_tz_in_correct_format){

        graph_series$graphs[[length(graph_series$graphs) + 1]] <- graph

        return(graph_series)
      }
    }
  }
}
