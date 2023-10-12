# a correct node data frame is generated

    Code
      create_node_df(n = "a")
    Condition
      Error:
      ! `create_node_df()` REASON:
      * The value supplied to `n` must be numeric
    Code
      create_node_df(n = c(1, 2))
    Condition
      Error:
      ! `create_node_df()` REASON:
      * The value supplied to `n` must be a single numeric value

