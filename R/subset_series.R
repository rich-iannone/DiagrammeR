#' Subset a graph series object
#' @description Subsetting a graph series by the graphs' index positions in
#' the graph series or through selection via graphs' date-time attributes.
#' @param graph_series a graph series object of type \code{dgr_graph_1D}.
#' @param by either \code{number}, which allows for subsetting of the graph
#' series by graph indices, or \code{time} which for graph series objects of
#' type \code{temporal} allows for a subsetting of graphs by a date-time or
#' time range.
#' @param values where the subsetting of the graph series by to occur via graph
#' indices (where \code{by = number}), provide a vector of those indices; when
#' subsetting by time (where \code{by = time}), a range of times can be
#' provided as a vector.
#' @param tz the time zone (\code{tz}) corresponding to dates or date-time
#' string provided in \code{values} (if \code{by = "date"}).
#' @return a graph series object of type \code{dgr_graph_1D}.
#' @examples
#' \dontrun{
#' # Create three graphs (using \code{pipeR} for speed)
#' # and create a graph series using those graphs
#' library(magrittr)
#'
#' graph_time_1 <-
#'   create_graph(graph_name = "graph_with_time_1",
#'                graph_time = "2015-03-25 03:00",
#'                graph_tz = "GMT") %>%
#'   add_node("a") %>% add_node("b") %>% add_node("c") %>%
#'   add_edge("a", "c") %>% add_edge("a", "b") %>% add_edge("b", "c")
#' 
#' graph_time_2 <-
#'   create_graph(graph_name = "graph_with_time_2",
#'                graph_time = "2015-03-26 03:00",
#'                graph_tz = "GMT") %>%
#'   add_node("d") %>% add_node("e") %>% add_node("f") %>%
#'   add_edge("d", "f") %>% add_edge("d", "e") %>% add_edge("e", "f")
#' 
#' graph_time_3 <-
#'   create_graph(graph_name = "graph_with_time_3",
#'                graph_time = "2015-03-27 15:00",
#'                graph_tz = "GMT") %>%
#'   add_node("x") %>% add_node("y") %>% add_node("z") %>%
#'   add_edge("x", "z") %>% add_edge("x", "y") %>% add_edge("y", "z")
#'
#' # Create an empty graph series
#' series_temporal <- create_series(series_type = "temporal")
#'
#' # Add graphs to the graph series
#' series_temporal <- graph_time_1 %>% add_to_series(series_temporal)
#' series_temporal <- graph_time_2 %>% add_to_series(series_temporal)
#' series_temporal <- graph_time_3 %>% add_to_series(series_temporal)
#'
#' # Subset graph series by sequence
#' series_sequence_subset <-
#'   subset_series(graph_series = series_temporal,
#'                 by = "number",
#'                 values = 2)
#'
#' graph_count(series_sequence_subset)
#' #> [1] 1
#'
#' # Subset graph series by date-time
#' series_time_subset <-
#'   subset_series(graph_series = series_temporal,
#'                 by = "time",
#'                 values = c("2015-03-25 12:00",
#'                            "2015-03-26 12:00"),
#'                 tz = "GMT")
#'
#' graph_count(series_time_subset)
#' #> [1] 1
#' }
#' @export subset_series

subset_series <- function(graph_series,
                          by = "number",
                          values,
                          tz = NULL){

  if (graph_count(graph_series = graph_series) == 0){

    return(graph_series)
  }

  if (by == "number"){

    # validate the value provided for 'values'
    if (class(values) != "numeric"){

      return(graph_series)
    }

    indices_in_graph_series <-
      1:graph_count(graph_series = graph_series)

    indices_in_subset_value <- values

    graphs_to_remove <-
      which(!(indices_in_graph_series %in%
                indices_in_subset_value))

    graphs_to_remove <-
      sort(graphs_to_remove, decreasing = TRUE)

    for (i in graphs_to_remove){

      graph_series <-
        remove_from_series(graph_series = graph_series,
                           index = i)
    }

    return(graph_series)
  }

  if (by == "time"){

    # validate the value provided for 'values'
    if (class(values) == "numeric"){

      return(graph_series)
    }

    is_tz_in_correct_format <-
      ifelse(tz %in% OlsonNames(), TRUE, FALSE)

    if (is_tz_in_correct_format == FALSE){

      return(graph_series)
    }

    for (i in 1:length(values)){

      is_time_in_correct_format <-
        ifelse(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
                     values[i]) |
                 grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}$",
                       values[i]) |
                 grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$",
                       values[i]), TRUE, FALSE)

      if (is_time_in_correct_format == FALSE){

        return(graph_series)
      }
    }

    # Create subset based on range
    if (length(values) == 2){

      for (i in 1:graph_count(graph_series = graph_series)){

        if (i == 1){
          dates_times_in_series <- vector(mode = "numeric", length = 0)
          tz_in_series <- vector(mode = "character", length = 0)
        }

        dates_times_in_series <-
          c(dates_times_in_series, graph_series$graphs[[i]]$graph_time)

        tz_in_series <-
          c(tz_in_series, graph_series$graphs[[i]]$graph_tz)

      }

      for (i in 1:length(dates_times_in_series)){

        if (i == 1){
          dates_times_in_series_with_tz <-
            as.POSIXct(rep("1970-01-01", length(dates_times_in_series)),
                       tz = "GMT")
        }

        dates_times_in_series_with_tz[i] <-
          as.POSIXct(dates_times_in_series[i], tz = tz_in_series[i])
      }


      graphs_to_remove <-
        which(!(1:length(dates_times_in_series_with_tz) %in%
                  which(dates_times_in_series_with_tz >=
                          as.POSIXct(values[1], tz = tz) &
                          dates_times_in_series_with_tz <=
                          as.POSIXct(values[2], tz = tz))))

      graphs_to_remove <-
        sort(graphs_to_remove, decreasing = TRUE)

      # Remove selected graphs from the series
      for (i in graphs_to_remove){

        graph_series <-
          remove_from_series(graph_series = graph_series,
                                   index = i)
      }

      return(graph_series)
    }

    # Create subset based on exact time
    if (length(values) == 1){


    }
  }
}
