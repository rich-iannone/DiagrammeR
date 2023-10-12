#' Export a graph to CSV files
#'
#' @description
#'
#' Export a graph to separate CSV files for nodes and edges.
#'
#' @inheritParams render_graph
#' @param ndf_name The name to provide to the CSV file containing node
#'   information. By default this CSV will be called `nodes.csv`.
#' @param edf_name The name to provide to the CSV file containing edge
#'   information. By default this CSV will be called `edges.csv`.
#' @param output_path The path to which the CSV files will be placed. By
#'   default, this is the current working directory.
#' @param colnames_type Provides options to modify CSV column names to allow for
#'   easier import into other graph systems. The `neo4j` option modifies column
#'   names to allow for direct import of CSVs into Neo4J with the `LOAD CSV`
#'   clause. The `graphframes` option modifies column names to match those
#'   required by the Spark GraphFrames package.
#'
#' @examples
#' # Create a node data frame (ndf)
#' ndf <-
#'   create_node_df(
#'     n = 4,
#'     type = c("a", "a", "z", "z"),
#'     label = TRUE,
#'     value = c(3.5, 2.6, 9.4, 2.7)
#'   )
#'
#' # Create an edge data frame (edf)
#' edf <-
#'   create_edge_df(
#'     from = c(1, 2, 3),
#'     to = c(4, 3, 1),
#'     rel = c("rel_a", "rel_z", "rel_a")
#'   )
#'
#' # Create a graph with the ndf and edf
#' graph <-
#'   create_graph(
#'     nodes_df = ndf,
#'     edges_df = edf
#'   )
#'
#' # Create separate `nodes.csv` and
#' # `edges.csv` files
#' # graph %>% export_csv()
#'
#' @export
export_csv <- function(
    graph,
    ndf_name = "nodes.csv",
    edf_name = "edges.csv",
    output_path = getwd(),
    colnames_type = NULL
) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Validation: Graph object is valid
  if (graph_object_valid(graph) == FALSE) {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The graph is not valid.")
  }

  nodes_df <- get_node_df(graph)
  edges_df <- get_edge_df(graph)

  if (!is.null(colnames_type)) {

    # Modify column names for easier import into Neo4J
    # via `LOAD CSV` or the `neo4j-import` tool
    if (colnames_type == "neo4j") {

      # Modify column names in the ndf
      colnames(nodes_df)[1:3] <-
        c("nodes:ID", ":LABEL", "label")

      # Modify column names in the edf
      colnames(edges_df)[1:3] <-
        c(":START_ID", ":END_ID", ":TYPE")
    }

    # Modify column names for easier import into a
    # Spark GraphFrame using the `spark-csv` pkg with
    # `sqlContext.read.format('com.databricks.spark.csv')`
    if (colnames_type == "graphframes") {

      # Modify column names in the ndf
      colnames(nodes_df)[1] <- "id"

      # Modify column names in the edf
      colnames(edges_df)[1:2] <-
        c("src", "dst")
    }
  }

  # Write the CSV files to the output directory
  utils::write.csv(
    nodes_df,
    file = paste0(output_path,
                  "/", ndf_name),
    row.names = FALSE, quote = FALSE)

  utils::write.csv(
    edges_df,
    file = paste0(output_path,
                  "/", edf_name),
    row.names = FALSE, quote = FALSE)
}
