#' Subset a graph series object
#' @description Subsetting a graph series by the graphs' index positions in the graph series or through selection via graphs' date-time attributes.
#' @param graph_series a graph series object of type \code{dgr_graph_1D}.
#' @param by either \code{number}, which allows for subsetting of the graph series by graph indices, or \code{time} which for graph series objects of type \code{temporal} allows for a subsetting of graphs by a date-time or time range.
#' @param values where the subsetting of the graph series by to occur via graph indices (where \code{by = number}), provide a vector of those indices; when subsetting by time (where \code{by = time}), a range of times can be provided as a vector.
#' @param tz the time zone (\code{tz}) corresponding to dates or date-time string provided in \code{values} (if \code{by = "date"}).
#' @return a graph series object of type \code{dgr_graph_1D}.
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
