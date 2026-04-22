# Subset a graph series object

Subsetting a graph series by the graphs' index positions in the graph
series or through selection via graphs' date-time attributes.

## Usage

``` r
filter_graph_series(graph_series, by = "number", values, tz = NULL)
```

## Arguments

- graph_series:

  A graph series object of type `dgr_graph_1D`.

- by:

  Either `number`, which allows for subsetting of the graph series by
  graph indices, or `time` which for graph series objects of type
  `temporal` allows for a subsetting of graphs by a date-time or time
  range.

- values:

  Where the subsetting of the graph series by to occur via graph indices
  (where `by = number`), provide a vector of those indices; when
  subsetting by time (where `by = time`), a range of times can be
  provided as a vector.

- tz:

  The time zone (`tz`) corresponding to dates or date-time string
  provided in `values` (if `by = "date"`).

## Value

A graph series object of type `dgr_graph_1D`.

## Examples

``` r
# Create three graphs
graph_time_1 <-
  create_graph(
    graph_name = "graph_with_time_1") |>
  set_graph_time(
    time = "2015-03-25 03:00",
    tz = "GMT")

graph_time_2 <-
  create_graph(
    graph_name = "graph_with_time_2") |>
  set_graph_time(
    time = "2015-03-26 03:00",
    tz = "GMT")

graph_time_3 <-
  create_graph(
    graph_name = "graph_with_time_3") |>
  set_graph_time(
    time = "2015-03-27 15:00",
    tz = "GMT")

# Create an empty graph series and add
# the graphs
series_temporal <-
  create_graph_series(
    series_type = "temporal") |>
  add_graph_to_graph_series(
    graph = graph_time_1) |>
  add_graph_to_graph_series(
    graph = graph_time_2) |>
  add_graph_to_graph_series(
    graph = graph_time_3)

# Subset graph series by sequence
series_sequence_subset <-
  filter_graph_series(
    graph_series = series_temporal,
    by = "number",
    values = 2)

# Get a count of graphs in
# the series
series_sequence_subset |>
  count_graphs_in_graph_series()
#> [1] 1

# Subset graph series by date-time
series_time_subset <-
  filter_graph_series(
    graph_series = series_temporal,
    by = "time",
    values = c("2015-03-25 12:00",
               "2015-03-26 12:00"),
    tz = "GMT")

# Get a count of graphs in
# the series
series_time_subset |>
  count_graphs_in_graph_series()
#> [1] 2
```
