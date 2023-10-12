# Getting edge attributes is possible

    Code
      get_edge_attrs(graph = graph, edge_attr = from)
    Condition
      Error:
      ! `get_edge_attrs()` REASON:
      * This is not an edge attribute
    Code
      get_edge_attrs(graph = graph, edge_attr = to)
    Condition
      Error:
      ! `get_edge_attrs()` REASON:
      * This is not an edge attribute
    Code
      get_edge_attrs(graph = graph, edge_attr = value, from = c(1, 2), to = 2)
    Condition
      Error:
      ! `get_edge_attrs()` REASON:
      * The number of nodes in `from` and `to` must be the same`()` REASON:
      * The number of nodes in `from` and `to` must be the same

