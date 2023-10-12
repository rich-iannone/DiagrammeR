# edges can be deleted from a graph using node label values

    Code
      graph_labeled_nodes %>% delete_edge(from = "zero", to = "two")
    Condition
      Error:
      ! `delete_edge()` REASON:
      * The value provided in `from` does not exist as a node `label` value
    Code
      graph_labeled_nodes %>% delete_edge(from = "one", to = "three")
    Condition
      Error:
      ! `delete_edge()` REASON:
      * The value provided in `to` does not exist as a node `label` value

