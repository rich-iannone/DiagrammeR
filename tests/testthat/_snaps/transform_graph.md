# Reversing edges in a selection is possible

    Code
      create_graph(directed = FALSE) %>% add_balanced_tree(k = 2, h = 2) %>%
        select_edges_by_node_id(nodes = 1:2) %>% rev_edge_dir_ws()
    Condition
      Error in `rev_edge_dir_ws()`:
      ! The input graph must be a directed graph

# Fully disconnecting selected nodes is possible

    Code
      create_graph() %>% add_path(n = 2) %>% fully_disconnect_nodes_ws()
    Condition
      Error in `fully_disconnect_nodes_ws()`:
      ! There is no selection of nodes available.

