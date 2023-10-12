# Setting a cache is possible

    Code
      set_cache(graph = graph, name = "closeness_df_2", to_cache = closeness_df)
    Condition
      Error:
      ! `set_cache()` REASON:
      * You must provide a column name from the data frame
    Code
      set_cache(graph = graph, name = "closeness_df_3", to_cache = closeness_df, col = "nonexistent")
    Condition
      Error:
      ! `set_cache()` REASON:
      * The column name provided doesn't exist in the data frame`()` REASON:
      * The column name provided doesn't exist in the data frame

