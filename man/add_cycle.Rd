% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_cycle.R
\name{add_cycle}
\alias{add_cycle}
\title{Add a cycle of nodes to the graph}
\usage{
add_cycle(
  graph,
  n,
  type = NULL,
  label = TRUE,
  rel = NULL,
  node_aes = NULL,
  edge_aes = NULL,
  node_data = NULL,
  edge_data = NULL
)
}
\arguments{
\item{graph}{A graph object of class \code{dgr_graph}.}

\item{n}{The number of nodes comprising the cycle.}

\item{type}{An optional string that describes the entity type for the nodes
to be added.}

\item{label}{Either a vector object of length \code{n} that provides optional
labels for the new nodes, or, a boolean value where setting to \code{TRUE}
ascribes node IDs to the label and \code{FALSE} yields a blank label.}

\item{rel}{An optional string for providing a relationship label to all new
edges created in the node cycle.}

\item{node_aes}{An optional list of named vectors comprising node aesthetic
attributes. The helper function \code{\link[=node_aes]{node_aes()}} is strongly recommended for
use here as it contains arguments for each of the accepted node aesthetic
attributes (e.g., \code{shape}, \code{style}, \code{color}, \code{fillcolor}).}

\item{edge_aes}{An optional list of named vectors comprising edge aesthetic
attributes. The helper function \code{\link[=edge_aes]{edge_aes()}} is strongly recommended for
use here as it contains arguments for each of the accepted edge aesthetic
attributes (e.g., \code{shape}, \code{style}, \code{penwidth}, \code{color}).}

\item{node_data}{An optional list of named vectors comprising node data
attributes. The helper function \code{\link[=node_data]{node_data()}} is strongly recommended for
use here as it helps bind data specifically to the created nodes.}

\item{edge_data}{An optional list of named vectors comprising edge data
attributes. The helper function \code{\link[=edge_data]{edge_data()}} is strongly recommended for
use here as it helps bind data specifically to the created edges.}
}
\value{
A graph object of class \code{dgr_graph}.
}
\description{
With a graph object of class \code{dgr_graph}, add a node cycle to the graph.
}
\examples{
# Create a new graph and
# add a cycle of nodes to it
graph <-
  create_graph() \%>\%
  add_cycle(n = 6)

# Get node information
# from this graph
graph \%>\%
  get_node_info()

# Node and edge aesthetic and data
# attributes can be specified in
# the `node_aes`, `edge_aes`,
# `node_data`, and `edge_data`
# arguments

suppressWarnings(RNGversion("3.5.0"))
set.seed(23)

graph_w_attrs <-
  create_graph() \%>\%
  add_cycle(
    n = 3,
    label = c(
      "one", "two", "three"),
    type = c(
      "a", "a", "b"),
    rel = "A",
    node_aes = node_aes(
      fillcolor = "steelblue"),
    edge_aes = edge_aes(
      color = "red",
      penwidth = 1.2),
    node_data = node_data(
      value = c(
        1.6, 2.8, 3.4)),
    edge_data = edge_data(
      value =
        rnorm(
          n = 3,
          mean = 5.0,
          sd = 1.0)))

# Get the graph's node data frame
graph_w_attrs \%>\% get_node_df()

# Get the graph's edge data frame
graph_w_attrs \%>\% get_edge_df()

}
