# Adding color based on edge attributes is possible

    Code
      create_graph() %>% add_gnm_graph(n = 10, m = 10, edge_data = edge_data(weight = rnorm(
        10, 5, 2)), set_seed = 23) %>% set_edge_attrs(edge_attr = "rel", values = c(
        "A", "A", "B", "B", "D", "A", "B", "C", "D", "A")) %>% colorize_edge_attrs(
        edge_attr_from = rel, edge_attr_to = color, palette = c("#458b00l15", "foo",
          "#00eeee", "bar", "orange"))
    Condition
      Error in `colorize_edge_attrs()`:
      ! The color palette contains invalid hexadecimal values.

