#' Trigger a script embedded in a graph series object
#' @description Run an R script located inside or referenced from the graph series object in order to migrate the state of one or more contained graphs.
#' @param graph_series a graph series object of type \code{dgr_graph_1D}.
#' @param script the index of the script character string or path reference held in in the graph series.
#' @return a graph series object of type \code{dgr_graph_1D}.
#' @export trigger_script

trigger_script <- function(graph_series,
                           script = 1){

  to_substitute <- deparse(substitute(graph_series))

  substituted_script <- gsub("_SELF_", to_substitute,
                             graph_series$series_scripts[script])

  evaluated_series <- eval(parse(text = substituted_script))

  return(evaluated_series)
}
