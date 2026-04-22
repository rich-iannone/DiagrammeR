# Create a graph using an adjacency matrix

Using an adjacency matrix object, generate a graph of class `dgr_graph`.

## Usage

``` r
from_adj_matrix(
  x,
  mode = "undirected",
  weighted = FALSE,
  use_diag = TRUE,
  graph_name = NULL,
  write_backups = FALSE,
  display_msgs = FALSE
)
```

## Arguments

- x:

  A square `matrix` object serving as the adjacency matrix.

- mode:

  The method in which to interpret the input adjacency matrix. Options
  include: `undirected`, `directed`, `upper`, `lower`, `max`, `min`, and
  `plus`.

- weighted:

  Whether to create a weighted graph from the adjacency matrix.

- use_diag:

  Whether to use the diagonal of the adjacency matrix in calculations.
  If `TRUE` then the diagonal values will be included as is. If `FALSE`
  then the diagonal values will be replaced with zero values before
  inclusion in any calculations.

- graph_name:

  An optional string for labeling the graph object.

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
# Create an adjacency matrix
adj_matrix <-
  sample(
    0:1, 100,
    replace = TRUE,
    prob = c(0.9,0.1)
  ) |>
  matrix(ncol = 10)

# Create a graph from the adjacency matrix
graph <- from_adj_matrix(adj_matrix)
```
