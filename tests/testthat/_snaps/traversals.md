# simple traversals are possible

    Code
      graph <- graph %>% clear_selection()
      trav_in(graph)
    Condition
      Error in `trav_in()`:
      ! There is no selection of nodes available.
      * any traversal requires an active selection
      * this type of traversal requires a selection of nodes
    Code
      trav_out(graph)
    Condition
      Error in `trav_out()`:
      ! There is no selection of nodes available.
      * Any traversal requires an active selection.
      * This type of traversal requires a selection of nodes.
    Code
      trav_both(graph)
    Condition
      Error in `trav_both()`:
      ! There is no selection of nodes available.
      * Any traversal requires an active selection.
      * This type of traversal requires a selection of nodes.
    Code
      trav_in_node(graph)
    Condition
      Error in `trav_in_node()`:
      ! The graph contains no selection of edges.
      * any traversal requires an active selection
      * this type of traversal requires a selection of edges
    Code
      trav_out_node(graph)
    Condition
      Error in `trav_out_node()`:
      ! The graph contains no selection of edges.
      * any traversal requires an active selection
      * this type of traversal requires a selection of edges
    Code
      trav_in_edge(graph)
    Condition
      Error in `trav_in_edge()`:
      ! There is no selection of nodes available.
      * any traversal requires an active selection
      * this type of traversal requires a selection of nodes
    Code
      trav_out_edge(graph)
    Condition
      Error in `trav_out_edge()`:
      ! There is no selection of nodes available.
      * any traversal requires an active selection
      * this type of traversal requires a selection of nodes

