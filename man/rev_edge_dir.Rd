% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rev_edge_dir.R
\name{rev_edge_dir}
\alias{rev_edge_dir}
\title{Reverse the direction of all edges in a graph}
\usage{
rev_edge_dir(graph)
}
\arguments{
\item{graph}{A graph object of class \code{dgr_graph}.}
}
\value{
A graph object of class \code{dgr_graph}.
}
\description{
Using a directed graph as input, reverse the direction of all edges in that
graph.
}
\examples{
# Create a graph with a
# directed tree
graph <-
  create_graph() \%>\%
  add_balanced_tree(
    k = 2, h = 2)

# Inspect the graph's edges
graph \%>\% get_edges()

# Reverse the edge directions
# such that edges are directed
# toward the root of the tree
graph <-
  graph \%>\%
  rev_edge_dir()

# Inspect the graph's edges
# after their reversal
graph \%>\% get_edges()

}
\seealso{
Other edge creation and removal: 
\code{\link{add_edge}()},
\code{\link{add_edge_clone}()},
\code{\link{add_edge_df}()},
\code{\link{add_edges_from_table}()},
\code{\link{add_edges_w_string}()},
\code{\link{add_forward_edges_ws}()},
\code{\link{add_reverse_edges_ws}()},
\code{\link{copy_edge_attrs}()},
\code{\link{create_edge_df}()},
\code{\link{delete_edge}()},
\code{\link{delete_edges_ws}()},
\code{\link{delete_loop_edges_ws}()},
\code{\link{drop_edge_attrs}()},
\code{\link{edge_data}()},
\code{\link{join_edge_attrs}()},
\code{\link{mutate_edge_attrs}()},
\code{\link{mutate_edge_attrs_ws}()},
\code{\link{recode_edge_attrs}()},
\code{\link{rename_edge_attrs}()},
\code{\link{rescale_edge_attrs}()},
\code{\link{rev_edge_dir_ws}()},
\code{\link{set_edge_attr_to_display}()},
\code{\link{set_edge_attrs}()},
\code{\link{set_edge_attrs_ws}()}
}
\concept{edge creation and removal}
