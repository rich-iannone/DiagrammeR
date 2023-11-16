# Getting edge attributes is possible

    Code
      get_edge_attrs(graph = graph, edge_attr = from)
    Condition
      Error in `get_edge_attrs()`:
      ! This is not an edge attribute.
    Code
      get_edge_attrs(graph = graph, edge_attr = to)
    Condition
      Error in `get_edge_attrs()`:
      ! This is not an edge attribute.
    Code
      get_edge_attrs(graph = graph, edge_attr = value, from = c(1, 2), to = 2)
    Condition
      Error in `get_edge_attrs()`:
      ! The number of nodes in `from` and `to` must be the same.

