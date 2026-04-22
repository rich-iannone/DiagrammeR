# Create an edge data frame

Combine several vectors for edges and their attributes into a data
frame, which can be combined with other similarly-generated data frames,
or, added to a graph object. An edge data frame, or edf, has at least
the following columns:

- `id` (of type `integer`)

- `from` (of type `integer`)

- `to` (of type `integer`)

- `rel` (of type `character`)

An arbitrary number of additional columns containing aesthetic or data
attributes can be part of the edf, so long as they follow the
aforementioned columns. Some examples are included in
[`edge_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_aes.md)

## Usage

``` r
create_edge_df(from, to, rel = NULL, ...)
```

## Arguments

- from:

  A vector of node ID values from which edges are outbound. The vector
  length must equal that of the `to` vector.

- to:

  A vector of node ID values to which edges are incoming. The vector
  length must equal that of the `from` vector.

- rel:

  An optional `rel` label for each edge.

- ...:

  One or more vectors for associated edge attributes. Can be some of
  [`edge_aes()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_aes.md)

## Value

An edge data frame (edf).

## See also

Other edge creation and removal:
[`add_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge.md),
[`add_edge_clone()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge_clone.md),
[`add_edge_df()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edge_df.md),
[`add_edges_from_table()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edges_from_table.md),
[`add_edges_w_string()`](https://rich-iannone.github.io/DiagrammeR/reference/add_edges_w_string.md),
[`add_forward_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/add_forward_edges_ws.md),
[`add_reverse_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/add_reverse_edges_ws.md),
[`copy_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/copy_edge_attrs.md),
[`delete_edge()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_edge.md),
[`delete_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_edges_ws.md),
[`delete_loop_edges_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/delete_loop_edges_ws.md),
[`drop_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/drop_edge_attrs.md),
[`edge_data()`](https://rich-iannone.github.io/DiagrammeR/reference/edge_data.md),
[`join_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/join_edge_attrs.md),
[`mutate_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/mutate_edge_attrs.md),
[`mutate_edge_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/mutate_edge_attrs_ws.md),
[`recode_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/recode_edge_attrs.md),
[`rename_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/rename_edge_attrs.md),
[`rescale_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/rescale_edge_attrs.md),
[`rev_edge_dir()`](https://rich-iannone.github.io/DiagrammeR/reference/rev_edge_dir.md),
[`rev_edge_dir_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/rev_edge_dir_ws.md),
[`set_edge_attr_to_display()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attr_to_display.md),
[`set_edge_attrs()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs.md),
[`set_edge_attrs_ws()`](https://rich-iannone.github.io/DiagrammeR/reference/set_edge_attrs_ws.md)

## Examples

``` r
# Create a simple edge data frame (edf) and
# view the results
edf <-
  create_edge_df(
    from = c(1, 2, 3),
    to = c(4, 3, 1),
    rel = "a")

# Display the edge data frame
edf
#>   id from to rel
#> 1  1    1  4   a
#> 2  2    2  3   a
#> 3  3    3  1   a

# Create an edf with additional edge
# attributes (where their classes will
# be inferred from the input vectors)
edf <-
  create_edge_df(
    from = c(1, 2, 3),
    to = c(4, 3, 1),
    rel = "a",
    length = c(50, 100, 250),
    color = "green",
    width = c(1, 5, 2))

# Display the edge data frame
edf
#>   id from to rel length color width
#> 1  1    1  4   a     50 green     1
#> 2  2    2  3   a    100 green     5
#> 3  3    3  1   a    250 green     2
```
