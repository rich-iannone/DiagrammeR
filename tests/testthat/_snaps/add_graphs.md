# Adding a balanced tree is possible

    Code
      add_balanced_tree(graph = graph, k = 1, h = 3, type = "a", rel = "z")
    Condition
      Error in `add_balanced_tree()`:
      ! `k` must be a whole number larger than or equal to 2, not the number 1.
    Code
      add_balanced_tree(graph = graph, k = 3, h = 1, type = "a", rel = "z")
    Condition
      Error in `add_balanced_tree()`:
      ! `h` must be a whole number larger than or equal to 2, not the number 1.

# Adding a path is possible

    Code
      add_path(graph = graph, n = 1, type = "a", rel = "z")
    Condition
      Error in `add_path()`:
      ! `n` must be a whole number larger than or equal to 2, not the number 1.

# Adding a preferential attachment graph is possible

    Code
      create_graph() %>% add_pa_graph(n = 0, m = 1)
    Condition
      Error in `add_pa_graph()`:
      ! `n` must be a whole number larger than or equal to 1, not the number 0.
    Code
      create_graph() %>% add_pa_graph(n = 0, m = 1, algo = "plumtree")
    Condition
      Error in `add_pa_graph()`:
      ! `n` must be a whole number larger than or equal to 1, not the number 0.

