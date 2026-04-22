# Convert an igraph graph to a DiagrammeR one

Convert an igraph graph to a DiagrammeR graph object.

## Usage

``` r
from_igraph(
  igraph,
  graph_name = NULL,
  write_backups = FALSE,
  display_msgs = FALSE
)
```

## Arguments

- igraph:

  An igraph graph object.

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
# Create a DiagrammeR graph object
dgr_graph_orig <-
  create_graph() |>
  add_gnm_graph(
    n = 36,
    m = 50,
    set_seed = 23)

# Convert the DiagrammeR
# graph to an igraph object
ig_graph <-
  dgr_graph_orig |>
  to_igraph()

# Convert the igraph graph
# back to a DiagrammeR graph
dgr_graph_new <-
  ig_graph |>
  from_igraph()

# Get some graph information
graph_info <- dgr_graph_new |>
  get_graph_info()

graph_info[, 1:6]
#>             name  n  e   dens mn_deg mx_deg
#> 1 graph_hWHYG7a9 36 50 0.0571      1      7
```
