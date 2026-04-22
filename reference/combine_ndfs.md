# Combine multiple node data frames

Combine several node data frames into a single node data frame.

## Usage

``` r
combine_ndfs(...)
```

## Arguments

- ...:

  Two or more node data frames, which contain node IDs and associated
  attributes.

## Value

A combined node data frame.

## Examples

``` r
# Create two node data frames
node_df_1 <-
  create_node_df(
    n = 2,
    type = c("a", "b"),
    label = c("D", "Z"),
    value = c(8.4, 3.4))

node_df_2 <-
  create_node_df(
    n = 2,
    type = c("b", "c"),
    label = c("U", "A"),
    value = c(0.4, 3.4))

# Combine the ndfs using the
# `combine_ndfs()` function
node_df_combined <-
  combine_ndfs(
    node_df_1,
    node_df_2)

# Inspect the combined ndf
node_df_combined
#>   id type label value
#> 1  1    a     D   8.4
#> 2  2    b     Z   3.4
#> 3  3    b     U   0.4
#> 4  4    c     A   3.4
```
