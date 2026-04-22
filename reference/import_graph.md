# Import a graph from various graph formats

Import a variety of graphs from different graph formats and create a
graph object.

## Usage

``` r
import_graph(
  graph_file,
  file_type = NULL,
  edges_extra_attr_names = NULL,
  edges_extra_attr_coltypes = NULL,
  graph_name = NULL,
  attr_theme = "default",
  write_backups = FALSE,
  display_msgs = FALSE
)
```

## Arguments

- graph_file:

  A connection to a graph file. When provided as a path to a file, it
  will read the file from disk. Files starting with `http://`,
  `https://`, `ftp://`, or `ftps://` will be automatically downloaded.

- file_type:

  The type of file to be imported. Options are: `gml` (GML), `sif`
  (SIF), `edges` (a .edges file), and `mtx` (MatrixMarket format). If
  not supplied, the type of graph file will be inferred by its file
  extension.

- edges_extra_attr_names:

  For `edges` files, a vector of attribute names beyond the `from` and
  `to` data columns can be provided in the order they appear in the
  input data file.

- edges_extra_attr_coltypes:

  For `edges` files, this is a string of column types for any attribute
  columns provided for `edges_extra_attr_names`. This string
  representation is where each character represents each of the extra
  columns of data and the mappings are: `c` -\> character, `i` -\>
  integer, `n` -\> number, `d` -\> double, `l` -\> logical, `D` -\>
  date, `T` -\> date time, `t` -\> time, `?` -\> guess, or `_/-`, which
  skips the column.

- graph_name:

  An optional string for labeling the graph object.

- attr_theme:

  The theme (i.e., collection of `graph`, `node`, and `edge` global
  graph attributes) to use for this graph. The default theme is called
  `default`; there are hierarchical layout themes called `lr`, `tb`,
  `rl`, and `bt` (these operate from left-to-right, top-to-bottom,
  right-to-left, and bottom-to-top); and, for larger graphs, the `fdp`
  theme provides a force directed layout. If this is set to `NULL` then
  no global graph attributes will be applied to the graph upon creation.

- write_backups:

  An option to write incremental backups of changing graph states to
  disk. If `TRUE`, a subdirectory within the working directory will be
  created and used to store `RDS` files. The default value is `FALSE` so
  one has to opt in to use this functionality.

- display_msgs:

  An option to display messages primarily concerned with changes in
  graph selections. By default, this is `FALSE`.

## Value

A graph object of class `dgr_graph`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Import a GML graph file
gml_graph <-
  import_graph(
    system.file(
      "extdata/karate.gml",
      package = "DiagrammeR"))

# Get a count of the graph's nodes
gml_graph |>
  count_nodes()

# Get a count of the graph's edges
gml_graph |>
  count_edges()
} # }
```
