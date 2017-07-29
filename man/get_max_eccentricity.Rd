% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_max_eccentricity.R
\name{get_max_eccentricity}
\alias{get_max_eccentricity}
\title{Get the maximum graph eccentricity}
\usage{
get_max_eccentricity(graph)
}
\arguments{
\item{graph}{a graph object of class
\code{dgr_graph}.}
}
\value{
a single numeric value representing the
maximum eccentricity of the graph.
}
\description{
Get the diameter of a graph, which is
the largest eccentricity in the graph. The graph
eccentricity of a node is its shortest path from
the farthest other node in the graph.
}
\examples{
# Create a cycle graph
graph <-
  create_graph() \%>\%
  add_cycle(n = 5)

# Determine the graph's maximum
# eccentricity
get_max_eccentricity(graph)
#> [1] 4

# Create a full graph and then
# get the maximum eccentricity
# value for that
create_graph() \%>\%
  add_full_graph(n = 10) \%>\%
  get_max_eccentricity()
#> [1] 1
}