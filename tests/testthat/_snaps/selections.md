# selecting a node in a graph is possible

    Code
      select_nodes(graph = graph, nodes = 5)
    Condition
      Error in `select_nodes()`:
      ! `nodes` must correspond to values in the graph.
      i `Graph values IDs include 1, 2, 3, and 4, not 5.
    Code
      select_nodes(graph = graph, nodes = 4:5)
    Output
      DiagrammeR Graph // 4 nodes / 3 edges
        -- directed / connected / DAG / property graph / simple
      
        NODES / type: 1 vals - complete / label: 4 vals - complete & unique
          -- 1 additional node attribute (value)
        EDGES / rel: 1 vals - complete                           info: `get_edge_df()`
          -- 1 additional edge attribute (width)
        SELECTION / 1 node selected                            info: `get_selection()`
        CACHE / <none>
        GLOBAL ATTRS / 17 are set                 info: `get_global_graph_attr_info()`
        GRAPH ACTIONS / <none>
        GRAPH LOG / create_graph() -> select_nodes()

