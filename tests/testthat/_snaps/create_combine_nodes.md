# a correct node data frame is generated

    Code
      create_node_df(n = "a")
    Condition
      Error in `create_node_df()`:
      ! The value supplied to `n` must be numeric
    Code
      create_node_df(n = c(1, 2))
    Condition
      Error in `create_node_df()`:
      ! The value supplied to `n` must be a single numeric value

