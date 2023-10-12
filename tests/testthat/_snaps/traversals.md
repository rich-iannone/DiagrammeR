# simple traversals are possible

    Code
      graph <- graph %>% clear_selection()
      trav_in(graph)
    Condition
      Error:
      ! `trav_in()` REASONS:
      * There is no selection of nodes available.
      * any traversal requires an active selection
      * this type of traversal requires a selection of nodes
    Code
      trav_out(graph)
    Condition
      Error:
      ! `trav_out()` REASONS:
      * There is no selection of nodes available.
      * any traversal requires an active selection
      * this type of traversal requires a selection of nodes
    Code
      trav_both(graph)
    Condition
      Error:
      ! `trav_both()` REASONS:
      * There is no selection of nodes available.
      * any traversal requires an active selection
      * this type of traversal requires a selection of nodes
    Code
      trav_in_node(graph)
    Condition
      Error:
      ! `trav_in_node()` REASONS:
      * The graph contains no selection of edges
      * any traversal requires an active selection
      * this type of traversal requires a selection of edges
    Code
      trav_out_node(graph)
    Condition
      Error:
      ! `trav_out_node()` REASONS:
      * The graph contains no selection of edges
      * any traversal requires an active selection
      * this type of traversal requires a selection of edges
    Code
      trav_in_edge(graph)
    Condition
      Error:
      ! `trav_in_edge()` REASONS:
      * There is no selection of nodes available.
      * any traversal requires an active selection
      * this type of traversal requires a selection of nodes
    Code
      trav_out_edge(graph)
    Condition
      Error:
      ! `trav_out_edge()` REASONS:
      * There is no selection of nodes available.
      * any traversal requires an active selection
      * this type of traversal requires a selection of nodes

