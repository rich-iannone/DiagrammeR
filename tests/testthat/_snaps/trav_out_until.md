# selective traversals with `trav_in_until()` are possible

    Code
      create_graph() %>% add_path(n = 10, node_data = node_data(value = 1:10)) %>%
        trav_in_until(conditions = value == 1)
    Condition
      Error in `trav_in_until()`:
      ! There is no selection of nodes available.
      * any traversal requires an active selection
      * this type of traversal requires a selection of nodes

