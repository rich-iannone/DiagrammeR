#' Subset a graph series object
#'
#' Subsetting a graph series by the graphs' index positions in the graph series
#' or through selection via graphs' date-time attributes.
#'
#' @param graph_series A graph series object of type `dgr_graph_1D`.
#' @param by Either `number`, which allows for subsetting of the graph series by
#'   graph indices, or `time` which for graph series objects of type `temporal`
#'   allows for a subsetting of graphs by a date-time or time range.
#' @param values Where the subsetting of the graph series by to occur via graph
#'   indices (where `by = number`), provide a vector of those indices; when
#'   subsetting by time (where `by = time`), a range of times can be provided as
#'   a vector.
#' @param tz The time zone (`tz`) corresponding to dates or date-time string
#'   provided in `values` (if `by = "date"`).
#'
#' @return A graph series object of type `dgr_graph_1D`.
#'
#' @examples
#' # Create three graphs
#' graph_time_1 <-
#'   create_graph(
#'     graph_name = "graph_with_time_1") %>%
#'   set_graph_time(
#'     time = "2015-03-25 03:00",
#'     tz = "GMT")
#'
#' graph_time_2 <-
#'   create_graph(
#'     graph_name = "graph_with_time_2") %>%
#'   set_graph_time(
#'     time = "2015-03-26 03:00",
#'     tz = "GMT")
#'
#' graph_time_3 <-
#'   create_graph(
#'     graph_name = "graph_with_time_3") %>%
#'   set_graph_time(
#'     time = "2015-03-27 15:00",
#'     tz = "GMT")
#'
#' # Create an empty graph series and add
#' # the graphs
#' series_temporal <-
#'   create_graph_series(
#'     series_type = "temporal") %>%
#'   add_graph_to_graph_series(
#'     graph = graph_time_1) %>%
#'   add_graph_to_graph_series(
#'     graph = graph_time_2) %>%
#'   add_graph_to_graph_series(
#'     graph = graph_time_3)
#'
#' # Subset graph series by sequence
#' series_sequence_subset <-
#'   filter_graph_series(
#'     graph_series = series_temporal,
#'     by = "number",
#'     values = 2)
#'
#' # Get a count of graphs in
#' # the series
#' series_sequence_subset %>%
#'   count_graphs_in_graph_series()
#'
#' # Subset graph series by date-time
#' series_time_subset <-
#'   filter_graph_series(
#'     graph_series = series_temporal,
#'     by = "time",
#'     values = c("2015-03-25 12:00",
#'                "2015-03-26 12:00"),
#'     tz = "GMT")
#'
#' # Get a count of graphs in
#' # the series
#' series_time_subset %>%
#'   count_graphs_in_graph_series()
#'
#' @export
filter_graph_series <- function(graph_series,
                                by = "number",
                                values,
                                tz = NULL) {

  if (count_graphs_in_graph_series(
    graph_series = graph_series) == 0) {

    return(graph_series)
  }

  if (by == "number") {

    # validate the value provided for 'values'
    if (!inherits(values, "numeric")) {

      return(graph_series)
    }

    indices_in_graph_series <-
      1:count_graphs_in_graph_series(graph_series = graph_series)

    indices_in_subset_value <- values

    graphs_to_remove <-
      which(!(indices_in_graph_series %in%
                indices_in_subset_value))

    graphs_to_remove <-
      sort(graphs_to_remove, decreasing = TRUE)

    for (i in graphs_to_remove) {

      graph_series <-
        remove_graph_from_graph_series(
          graph_series = graph_series,
          index = i)
    }

    return(graph_series)
  }

  if (by == "time") {

    # validate the value provided for 'values'
    if (inherits(values, "numeric")) {

      return(graph_series)
    }

    is_tz_in_correct_format <-
      ifelse(tz %in% OlsonNames(), TRUE, FALSE)

    if (is_tz_in_correct_format == FALSE) {

      return(graph_series)
    }

    for (i in 1:length(values)) {

      is_time_in_correct_format <-
        ifelse(grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$",
                     values[i]) |
                 grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}$",
                       values[i]) |
                 grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$",
                       values[i]), TRUE, FALSE)

      if (is_time_in_correct_format == FALSE) {

        return(graph_series)
      }
    }

    # Create subset based on range
    if (length(values) == 2) {

      for (i in 1:count_graphs_in_graph_series(graph_series = graph_series)) {

        if (i == 1) {
          dates_times_in_series <- vector(mode = "numeric", length = 0)
          tz_in_series <- vector(mode = "character", length = 0)
        }

        dates_times_in_series <-
          c(dates_times_in_series, graph_series$graphs[[i]]$graph_time)

        tz_in_series <-
          c(tz_in_series, graph_series$graphs[[i]]$graph_tz)

      }

      for (i in 1:length(dates_times_in_series)) {

        if (i == 1) {
          dates_times_in_series_with_tz <-
            as.POSIXct(rep("1970-01-01", length(dates_times_in_series)),
                       tz = "GMT")
        }

        dates_times_in_series_with_tz[i] <-
          as.POSIXct(dates_times_in_series[i], tz = tz_in_series[i],
                     origin = "1970-01-01")
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
      for (i in graphs_to_remove) {

        graph_series <-
          remove_graph_from_graph_series(
            graph_series = graph_series,
            index = i)
      }

      return(graph_series)
    }
  }
}
