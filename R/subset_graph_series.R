#' Subset a graph series object
#' Subsetting a graph series by the graphs' index positions in the graph series or through selection via graphs' date-time attributes.
#' @param graph_series a graph series object of type \code{dgr_graph_1D}.
#' @param by either \code{number}, which allows for subsetting of the graph series by graph indices, or \code{time} which for graph series objects of type \code{temporal} allows for a subsetting of graphs by a date-time or time range.
#' @param values for subsetting the graph series by graph indices (where \code{by = number}), provide a vector of those indices; when subsetting by time (where \code{by = time}), a range of times can be provided as a vector.
#' @return a graph series object of type \code{dgr_graph_1D}.
#' @export subset_graph_series

subset_graph_series <- function(graph_series,
                                by = "number",
                                values){


}
