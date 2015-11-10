#' Withdraw a stored vector from a graph object
#' @description Get the vector of node or edge attribute values stored
#' in a graph object of class \code{dgr_graph}.
#' @param graph a graph object of class \code{dgr_graph} that is created
#' using \code{create_graph}.
#' @return a vector of node or edge attribute values.
#' @export withdraw_values

withdraw_values <- function(graph){

  # If there is no deposited vector available, return NA value
  if (is.null(graph$deposit)){
    return(NA)
  }

  withdrawal <- graph$deposit

  return(withdrawal)
}
