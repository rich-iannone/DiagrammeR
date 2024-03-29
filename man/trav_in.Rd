% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trav_in.R
\name{trav_in}
\alias{trav_in}
\title{Traverse from one or more selected nodes onto adjacent, inward nodes}
\usage{
trav_in(
  graph,
  conditions = NULL,
  copy_attrs_from = NULL,
  copy_attrs_as = NULL,
  agg = "sum",
  add_to_selection = FALSE
)
}
\arguments{
\item{graph}{A graph object of class \code{dgr_graph}.}

\item{conditions}{An option to use filtering conditions for the traversal.}

\item{copy_attrs_from}{providing a node attribute name will copy those node
attribute values to the traversed nodes. Any values extant on the nodes
traversed to will be replaced.}

\item{copy_attrs_as}{If a node attribute name is provided in
\code{copy_attrs_from}, this option will allow the copied attribute values to be
written under a different attribute name. If the attribute name provided in
\code{copy_attrs_as} does not exist in the graph's ndf, the new node attribute
will be created with the chosen name.}

\item{agg}{If a node attribute is provided to \code{copy_attrs_from}, then an
aggregation function is required since there may be cases where multiple
edge attribute values will be passed onto the traversed node(s). To pass
only a single value, the following aggregation functions can be used:
\code{sum}, \code{min}, \code{max}, \code{mean}, or \code{median}.}

\item{add_to_selection}{An option to either add the traversed to nodes to the
active selection of nodes (\code{TRUE}) or switch the active selection entirely
to those traversed to nodes (\code{FALSE}, the default case).}
}
\value{
A graph object of class \code{dgr_graph}.
}
\description{
From a graph object of class \code{dgr_graph} move along inward edges from one or
more nodes present in a selection to other connected nodes, replacing the
current nodes in the selection with those nodes traversed to. An optional
filter by node attribute can limit the set of nodes traversed to.

This traversal function makes use of an active selection of nodes. After the
traversal, depending on the traversal conditions, there will either be a
selection of nodes or no selection at all.

Selections of nodes can be performed using the following node selection
(\verb{select_*()}) functions: \code{\link[=select_nodes]{select_nodes()}}, \code{\link[=select_last_nodes_created]{select_last_nodes_created()}},
\code{\link[=select_nodes_by_degree]{select_nodes_by_degree()}}, \code{\link[=select_nodes_by_id]{select_nodes_by_id()}}, or
\code{\link[=select_nodes_in_neighborhood]{select_nodes_in_neighborhood()}}.

Selections of nodes can also be performed using the following traversal
(\verb{trav_*()}) functions: \code{\link[=trav_out]{trav_out()}}, \code{\link[=trav_in]{trav_in()}}, \code{\link[=trav_both]{trav_both()}},
\code{\link[=trav_out_node]{trav_out_node()}}, \code{\link[=trav_in_node]{trav_in_node()}}, \code{\link[=trav_out_until]{trav_out_until()}}, or
\code{\link[=trav_in_until]{trav_in_until()}}.
}
\examples{
# Set a seed
suppressWarnings(RNGversion("3.5.0"))
set.seed(23)

# Create a simple graph
graph <-
  create_graph() \%>\%
  add_n_nodes(
    n = 2,
    type = "a",
    label = c("asd", "iekd")) \%>\%
  add_n_nodes(
    n = 3,
    type = "b",
    label = c("idj", "edl", "ohd")) \%>\%
  add_edges_w_string(
    edges = "1->2 1->3 2->4 2->5 3->5",
    rel = c(NA, "A", "B", "C", "D"))

# Create a data frame with node ID values
# representing the graph edges (with `from`
# and `to` columns), and, a set of numeric values
df_edges <-
  data.frame(
    from = c(1, 1, 2, 2, 3),
    to = c(2, 3, 4, 5, 5),
    values = round(rnorm(5, 5), 2))

# Create a data frame with node ID values
# representing the graph nodes (with the `id`
# columns), and, a set of numeric values
df_nodes <-
  data.frame(
    id = 1:5,
    values = round(rnorm(5, 7), 2))

# Join the data frame to the graph's internal
# edge data frame (edf)
graph <-
  graph \%>\%
  join_edge_attrs(df = df_edges) \%>\%
  join_node_attrs(df = df_nodes)

# Show the graph's internal node data frame
graph \%>\% get_node_df()

# Show the graph's internal edge data frame
graph \%>\% get_edge_df()

# Perform a simple traversal from node `4` to
# inward adjacent edges with no conditions
# on the nodes traversed to
graph \%>\%
  select_nodes_by_id(nodes = 4) \%>\%
  trav_in() \%>\%
  get_selection()

# Traverse from node `5` to inbound-facing
# nodes, filtering to those nodes that have
# numeric values greater than `5.0` for
# the `values` node attribute
graph \%>\%
  select_nodes_by_id(nodes = 4) \%>\%
  trav_in(
    conditions = values > 5.0) \%>\%
  get_selection()

# Traverse from node `5` to any inbound
# nodes, filtering to those nodes that
# have a `type` attribute of `b`
graph \%>\%
  select_nodes_by_id(nodes = 5) \%>\%
  trav_in(
    conditions = type == "b") \%>\%
  get_selection()

# Traverse from node `5` to any inbound
# nodes, filtering to those nodes that
# have a degree of `2`
graph \%>\%
  {
  node_degrees <-
    get_node_info(.) \%>\%
    dplyr::select(id, deg)
  join_node_attrs(., node_degrees)
  } \%>\%
  select_nodes_by_id(nodes = 5) \%>\%
  trav_in(
    conditions = deg == 2) \%>\%
  get_selection()

# Traverse from node `5` to any inbound
# nodes, and use multiple conditions for the
# traversal
graph \%>\%
  select_nodes_by_id(nodes = 5) \%>\%
  trav_in(
    conditions =
      type == "a" &
      values > 6.0) \%>\%
  get_selection()

# Traverse from node `5` to any inbound
# nodes, and use multiple conditions with
# a single-length vector
graph \%>\%
  select_nodes_by_id(nodes = 5) \%>\%
  trav_in(
    conditions =
      type == "b" | values > 6.0) \%>\%
  get_selection()

# Traverse from node `5` to any inbound
# nodes, and use a regular expression as
# a filtering condition
graph \%>\%
  select_nodes_by_id(nodes = 2) \%>\%
  trav_in(
    conditions = grepl("^i.*", label)) \%>\%
  get_selection()

# Create another simple graph to demonstrate
# copying of node attribute values to traversed
# nodes
graph <-
  create_graph() \%>\%
  add_node() \%>\%
  select_nodes() \%>\%
  add_n_nodes_ws(
    n = 2,
    direction = "from") \%>\%
  clear_selection() \%>\%
  select_nodes_by_id(nodes = 2:3) \%>\%
  set_node_attrs_ws(
    node_attr = value,
    value = 5)

# Show the graph's internal node data frame
graph \%>\% get_node_df()

# Show the graph's internal edge data frame
graph \%>\% get_edge_df()

# Perform a traversal from the outer nodes
# (`2` and `3`) to the central node (`1`) while
# also applying the node attribute `value` to
# node `1` (summing the `value` of 5 from
# both nodes before applying the value to the
# target node)
graph <-
  graph \%>\%
  trav_in(
    copy_attrs_from = value,
    agg = "sum")

# Show the graph's internal node data frame
# after this change
graph \%>\% get_node_df()

}
