#' Subset a graph series object
#' Subsetting a graph series by the graphs' index positions in the graph series or through selection via graphs' date-time attributes.
#' @param graph_series a graph series object of type \code{dgr_graph_1D}.
#' @param by either \code{number}, which allows for subsetting of the graph series by graph indices, or \code{time} which for graph series objects of type \code{temporal} allows for a subsetting of graphs by a date-time or time range.
#' @param values where the subsetting of the graph series by to occur via graph indices (where \code{by = number}), provide a vector of those indices; when subsetting by time (where \code{by = time}), a range of times can be provided as a vector.
#' @return a graph series object of type \code{dgr_graph_1D}.
#' @export subset_graph_series

subset_graph_series <- function(graph_series,
                                by = "number",
                                values){

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

    for (i in which(!(indices_in_graph_series %in%
                                    indices_in_subset_value))){

      graph_series <-
        remove_from_graph_series(graph_series = graph_series,
                                 index = i)
    }

    return(graph_series)
  }
}
