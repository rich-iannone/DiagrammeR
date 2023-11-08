# Setting a cache is possible

    Code
      set_cache(graph = graph, name = "closeness_df_2", to_cache = closeness_df)
    Condition
      Error in `set_cache()`:
      ! `col` must be a column name that exists in the data frame.
    Code
      set_cache(graph = graph, name = "closeness_df_3", to_cache = closeness_df, col = "nonexistent")
    Condition
      Error in `set_cache()`:
      ! `col` must be a column name that exists in the data frame.

