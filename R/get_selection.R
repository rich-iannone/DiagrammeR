#' Get the current selection available in a graph object
#' @description Get the current selection of nodes or edges from a graph
#' object of class \code{dgr_graph}.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @examples
#' \dontrun{
#' }
#' @return a list object with the current selection of nodes or edges.
#' @export get_selection

get_selection <- function(graph){

  # If there is no selection available return NA value
  if (is.null(graph$selection)){
    return(NA)
  }

  selection <- graph$selection

  return(selection)
}
