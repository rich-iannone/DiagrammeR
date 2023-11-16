# reverse edges can be added given a selection of edges

    Code
      create_graph() %>% add_n_nodes(n = 2, type = "type_a", label = c("a_1", "a_2")) %>%
        add_edge(from = 1, to = 2, rel = "a") %>% add_reverse_edges_ws("b")
    Condition
      Error in `add_reverse_edges_ws()`:
      ! The graph contains no selection of edges.

