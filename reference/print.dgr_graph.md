# Print the graph to the terminal

This function will provide a summary of the graph.

## Usage

``` r
# S3 method for class 'dgr_graph'
print(x, ...)
```

## Arguments

- x:

  A graph object of class `dgr_graph`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create a random graph using the
# `add_gnm_graph()` function
graph <-
  create_graph() |>
  add_gnm_graph(
    n = 10,
    m = 15,
    set_seed = 23)

# Get a summary of the graph
graph
} # }
```
