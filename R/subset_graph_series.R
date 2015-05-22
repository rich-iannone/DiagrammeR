#' Subset a graph series object
#' Subsetting a graph series by the graphs' index positions in the graph series or through selection via graphs' date-time attributes.
#' @param graph_series a graph series object.
#' @param by either "number", which allows for subsetting of the graph series by graph indices, or "time" which for graph series objects of type "temporal" allows for a subsetting of graphs by a date-time or time range.
#' @param subset for subsetting the graph series by graph indices (where by = number), provide a vector of those indices; when subsetting by time (where by = time), a range of times can be provided as a vector.
#' @return a numeric vector representing a count of graphs in a graph series object.
#' @export subset_graph_series

subset_graph_series <- function(graph_series,
                                by = "number",
                                subset){


}
