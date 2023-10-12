# adding edges from a table to a graph is possible

    Code
      graph %>% add_edges_from_table(edge_table, from_col = from, to_col = to_currency,
        from_to_map = iso_4217_code)
    Condition
      Error:
      ! `add_edges_from_table()` REASON:
      * The value specified in `from_col` is not in the table`()` REASON:
      * The value specified in `from_col` is not in the table
    Code
      graph %>% add_edges_from_table(edge_table, from_col = from_currency, to_col = to,
        from_to_map = iso_4217_code)
    Condition
      Error:
      ! `add_edges_from_table()` REASON:
      * The value specified in `to_col` is not in the table`()` REASON:
      * The value specified in `to_col` is not in the table
    Code
      graph %>% add_edges_from_table(edge_table, from_col = from_currency, to_col = to_currency,
        from_to_map = iso_4217)
    Condition
      Error:
      ! `add_edges_from_table()` REASON:
      * The value specified in `from_to_map` is not in the graph`()` REASON:
      * The value specified in `from_to_map` is not in the graph

