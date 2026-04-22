# Export a graph to CSV files

Export a graph to separate CSV files for nodes and edges.

## Usage

``` r
export_csv(
  graph,
  ndf_name = "nodes.csv",
  edf_name = "edges.csv",
  output_path = getwd(),
  colnames_type = NULL
)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

- ndf_name:

  The name to provide to the CSV file containing node information. By
  default this CSV will be called `nodes.csv`.

- edf_name:

  The name to provide to the CSV file containing edge information. By
  default this CSV will be called `edges.csv`.

- output_path:

  The path to which the CSV files will be placed. By default, this is
  the current working directory.

- colnames_type:

  Provides options to modify CSV column names to allow for easier import
  into other graph systems. The `neo4j` option modifies column names to
  allow for direct import of CSVs into Neo4J with the `LOAD CSV` clause.
  The `graphframes` option modifies column names to match those required
  by the Spark GraphFrames package.

## Examples

``` r
# Create a node data frame (ndf)
ndf <-
  create_node_df(
    n = 4,
    type = c("a", "a", "z", "z"),
    label = TRUE,
    value = c(3.5, 2.6, 9.4, 2.7)
  )

# Create an edge data frame (edf)
edf <-
  create_edge_df(
    from = c(1, 2, 3),
    to = c(4, 3, 1),
    rel = c("rel_a", "rel_z", "rel_a")
  )

# Create a graph with the ndf and edf
graph <-
  create_graph(
    nodes_df = ndf,
    edges_df = edf
  )

# Create separate `nodes.csv` and
# `edges.csv` files
# graph |> export_csv()
```
