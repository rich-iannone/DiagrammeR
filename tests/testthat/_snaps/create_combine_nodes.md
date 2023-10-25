# a correct node data frame is generated

    Code
      create_node_df(n = "a")
    Condition
      Error in `create_node_df()`:
      ! `n` must be a whole number, not the string "a".
    Code
      create_node_df(n = c(1, 2))
    Condition
      Error in `create_node_df()`:
      ! `n` must be a whole number, not a double vector.

