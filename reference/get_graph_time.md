# Get the graph date-time or timezone

Get the time and timezone for a graph object of class `dgr_graph`.

## Usage

``` r
get_graph_time(graph)
```

## Arguments

- graph:

  A graph object of class `dgr_graph`.

## Value

A single-length `POSIXct` vector with the assigned graph time.

## Examples

``` r
# Create an empty graph and
# set the graph's time; if nothing
# is supplied for the `tz` argument,
# `GMT` is used as the time zone
graph <-
  create_graph() |>
    set_graph_time(
      time = "2015-10-25 15:23:00")

# Get the graph's time as a POSIXct
# object using `get_graph_time()`
graph |> get_graph_time()
#> [1] "2015-10-25 15:23:00 UTC"
```
