#' Render several graphs in a single panel
#' @description Using 2 or more \code{dgr_graph}
#' objects, render theses graph in a single panel.
#' @param ... 2 or more graph objects of class
#' \code{dgr_graph}.
#' @param ncols an option to provide the number
#' of columns in the graph layout.
#' @param nrows an option to provide the number
#' of rows in the graph layout.
#' @param titles an optional vector of titles for
#' each of the graphs displayed.
#' @examples
#' \dontrun{
#' # Create 4 random graphs with 25 nodes and
#' # increasing numbers of edges
#' graph_1 <-
#'   create_random_graph(
#'     n = 25, m = 15, set_seed = 23)
#'
#' graph_2 <-
#'   create_random_graph(
#'     n = 25, m = 25, set_seed = 23)
#'
#' graph_3 <-
#'   create_random_graph(
#'     n = 25, m = 35, set_seed = 23)
#'
#' graph_4 <-
#'   create_random_graph(
#'     n = 25, m = 45, set_seed = 23)
#'
#' # Create a function to apply the
#' # PageRank algorithm to the graph
#' # and then color nodes by their
#' # PageRank values
#' pagerank_color <- function(x) {
#'   x %>%
#'   join_node_attrs(get_pagerank(.)) %>%
#'   rescale_node_attrs(
#'     node_attr_from = "pagerank",
#'     to_lower_bound = "steelblue",
#'     to_upper_bound = "red",
#'     node_attr_to = "fillcolor",
#'     from_lower_bound = 0,
#'     from_upper_bound = 1)
#' }
#'
#' # Render all four graphs in the
#' # order specified while applying
#' # the `pagerank_color` function
#' render_graph_panel(
#'   graph_1 %>% pagerank_color(),
#'   graph_2 %>% pagerank_color(),
#'   graph_3 %>% pagerank_color(),
#'   graph_4 %>% pagerank_color(),
#'   ncols = 2,
#'   nrows = 2,
#'   titles = c("one", "two",
#'              "three", "four"))
#' }
#' @importFrom dplyr filter rename select mutate bind_cols bind_rows
#' @importFrom igraph layout_nicely
#' @importFrom purrr flatten_dbl
#' @importFrom scales rescale
#' @importFrom tibble as_tibble
#' @export render_graph_panel

render_graph_panel <- function(...,
                               ncols = NULL,
                               nrows = NULL,
                               titles = NULL) {

  # Collect graphs into a single list object
  graphs <- list(...)

  # Create bindings for specific variables
  V1 <- V2 <- x <- y <- NULL

  # Set the number of columns and rows for the
  # panel if either `nrows` or `ncols` is not provided
  if (is.null(ncols) | is.null(nrows)) {

    # Set the number of columns for the panel
    ncols <- (length(graphs) / 2) %>% ceiling()

    # Set the number of rows for the panel
    nrows <- (length(graphs) / 2) %>% ceiling()

    # Remove rows to determine whether less could be used
    for (i in nrows:1) {

      if (i * ncols > length(graphs)) {
        nrows <- i
      }

      if (i * ncols == length(graphs)) {
        nrows <- i
        break
      }

      if (i * ncols < length(graphs)) {
        nrows <- i + 1
        break
      }
    }
  }

  # Get list of coordinates for each of the graphs
  for (i in 1:length(graphs)) {
    if (i == 1) coords <- list()

    coords[[i]] <-
      graphs[[i]] %>%
      to_igraph() %>%
      igraph::layout_nicely() %>%
      tibble::as_tibble() %>%
      dplyr::rename(x = V1, y = V2)
  }

  for (i in 1:length(coords)) {
    if (i == 1) {
      span_x <- vector(mode = "numeric")
      span_y <- vector(mode = "numeric")
    }

    span_x <-
      c(span_x,
        max(coords[[i]] %>% dplyr::select(x) %>% purrr::flatten_dbl()) -
          min(coords[[i]] %>% dplyr::select(x) %>% purrr::flatten_dbl()))

    span_y <-
      c(span_y,
        max(coords[[i]] %>% dplyr::select(y) %>% purrr::flatten_dbl()) -
          min(coords[[i]] %>% dplyr::select(y) %>% purrr::flatten_dbl()))
  }

  # Get the plot area size for each graph
  plot_area_size <- c(max(span_x), max(span_y))

  # Get the maximum size of the plot square
  max_square <-
    ceiling(max(plot_area_size)) +
    (.15 * (max(plot_area_size)))

  # Get a list of coordinates for each graph's nodes
  for (i in 1:length(graphs)) {
    if (i == 1) xy_coords <- list()

    xy_coords[[i]] <-
      graphs[[i]] %>%
      to_igraph() %>%
      igraph::layout_nicely() %>%
      tibble::as_tibble() %>%
      dplyr::rename(x = V1, y = V2) %>%
      dplyr::mutate(
        x = scales::rescale(
          x,
          to = c(
            max_square / 2 - ((max(x) - min(x)) / 2),
            max_square / 2 + ((max(x) - min(x)) / 2)))) %>% # center x
      dplyr::mutate(
        y = scales::rescale(
          y,
          to =
            c(max_square / 2 - ((max(y) - min(y)) / 2),
              max_square / 2 + ((max(y) - min(y)) / 2)))) # center y
  }

  #
  # Create a grid for the graph panels
  #

  grid <-
    create_graph() %>%
    add_global_graph_attrs(attr = "shape", value = "plaintext", attr_type = "node") %>%
    add_global_graph_attrs(attr = "width", value = 0.001, attr_type = "node") %>%
    add_global_graph_attrs(attr = "height", value = 0.001, attr_type = "node")

  for (i in 1:(ncols + 1)) {

    grid <-
      grid %>%
      add_node(
        1,
        type = paste0("plot_sep_line_v_top_", i)) %>%
      select_last_nodes_created() %>%
      set_node_attrs_ws(node_attr = "label", value = " ") %>%
      set_node_attrs_ws(node_attr = "shape", value = "plaintext") %>%
      set_node_attrs_ws(node_attr = "x", value = (i - 1) * max_square)  %>%
      set_node_attrs_ws(node_attr = "y", value = max_square * nrows) %>%
      clear_selection() %>%
      add_node(
        1,
        type = paste0("plot_sep_line_v_bottom_", i)) %>%
      select_last_nodes_created() %>%
      set_node_attrs_ws(node_attr = "label", value = " ") %>%
      set_node_attrs_ws(node_attr = "shape", value = "plaintext") %>%
      set_node_attrs_ws(node_attr = "x", value = (i - 1) * max_square) %>%
      set_node_attrs_ws(node_attr = "y", value = 0) %>%
      clear_selection() %>%
      add_edge(
        from = get_node_ids(., conditions = paste0("type == 'plot_sep_line_v_top_", i, "'")),
        to = get_node_ids(., conditions = paste0("type == 'plot_sep_line_v_bottom_", i, "'"))) %>%
      select_last_edges_created() %>%
      set_edge_attrs_ws(edge_attr = "arrowhead", value = "none") %>%
      clear_selection()
  }

  for (i in 1:(nrows + 1)) {

    grid <-
      grid %>%
      add_node(
        1,
        type = paste0("plot_sep_line_h_left_", i)) %>%
      select_last_nodes_created() %>%
      set_node_attrs_ws(node_attr = "label", value = " ") %>%
      set_node_attrs_ws(node_attr = "shape", value = "plaintext") %>%
      set_node_attrs_ws(node_attr = "x", value = 0)  %>%
      set_node_attrs_ws(node_attr = "y", value = (i - 1) * max_square) %>%
      clear_selection() %>%
      add_node(
        1,
        type = paste0("plot_sep_line_h_right_", i)) %>%
      select_last_nodes_created() %>%
      set_node_attrs_ws(node_attr = "label", value = " ") %>%
      set_node_attrs_ws(node_attr = "shape", value = "plaintext") %>%
      set_node_attrs_ws(node_attr = "x", value = max_square * ncols) %>%
      set_node_attrs_ws(node_attr = "y", value = (i - 1) * max_square) %>%
      clear_selection() %>%
      add_edge(
        from = get_node_ids(., conditions = paste0("type == 'plot_sep_line_h_left_", i, "'")),
        to = get_node_ids(., conditions = paste0("type == 'plot_sep_line_h_right_", i, "'"))) %>%
      select_last_edges_created() %>%
      set_edge_attrs_ws(edge_attr = "arrowhead", value = "none") %>%
      clear_selection()
  }

  #
  # Get layout matrix for graphs
  #

  vector_for_matrix <-
    1:length(graphs) %>%
    tibble::as_tibble() %>%
    dplyr::bind_rows(
      tibble::as_tibble(
        rep(
          as.numeric(NA),
          (nrows * ncols) - length(graphs)))) %>%
    purrr::flatten_dbl()

  layout_matrix <-
    matrix(
      nrow = nrows, ncol = ncols,
      data = vector_for_matrix,
      byrow = TRUE)

  # Translate node positions for each graph
  for (i in 1:length(graphs)) {

    for (j in 1:nrow(layout_matrix)) {
      if (i %in% layout_matrix[j, ]) {
        row_ <- j
        break
      }
    }

    for (k in 1:ncol(layout_matrix)) {
      if (i %in% layout_matrix[, k]) {
        col_ <- k
        break
      }
    }

    graphs[[i]]$nodes_df <-
      graphs[[i]]$nodes_df %>%
      dplyr::bind_cols(xy_coords[[i]]) %>%
      dplyr::mutate(x = x + (max_square * (col_ - 1))) %>%
      dplyr::mutate(y = y - (max_square * (row_ - nrows)))
  }


  # Add graph titles to each graph panel
  if (!is.null(titles)) {

    graph_titles <-
      create_graph() %>%
      add_global_graph_attrs(attr = "shape", value = "plaintext", attr_type = "node") %>%
      add_global_graph_attrs(attr = "width", value = 0.001, attr_type = "node") %>%
      add_global_graph_attrs(attr = "height", value = 0.001, attr_type = "node")

    for (i in 1:length(graphs)) {

      for (j in 1:nrow(layout_matrix)) {
        if (i %in% layout_matrix[j, ]) {
          row_ <- j
          break
        }
      }

      for (k in 1:ncol(layout_matrix)) {
        if (i %in% layout_matrix[, k]) {
          col_ <- k
          break
        }
      }

      graph_titles <-
        graph_titles %>%
        add_node(
          label = titles[i],
          type = "title",
          shape = "plaintext",
          fillcolor = "white",
          color = "white",
          x = (max_square * (col_ - 1)) + (0.5 * max_square),
          y = (nrows * max_square) - ((row_ - 1) * max_square))
    }
  }

  # Combine graphs into one
  for (i in 1:(length(graphs) - 1)) {

    if (i == 1) {
      combo_graph <-
        combine_graphs(
          graphs[[i]],
          graphs[[i + 1]]
        )
    }

    if (i > 1) {
      combo_graph <-
        combine_graphs(
          combo_graph,
          graphs[[i + 1]]
        )
    }
  }

  if (!is.null(titles)) {
    combo_graph <-
      combine_graphs(combo_graph, graph_titles) %>%
      select_nodes(conditions = "type == 'title'") %>%
      set_node_attrs_ws(node_attr = "shape", value = "rectangle") %>%
      set_node_attrs_ws(node_attr = "fontsize", value = 16) %>%
      set_node_attrs_ws(node_attr = "labeljust", value = "r") %>%
      clear_selection()
  }

  combo_graph <-
    combine_graphs(combo_graph, grid) %>%
    select_nodes(conditions = "grepl('plot_', type)") %>%
    set_node_attrs_ws(node_attr = "width", value = 0.001) %>%
    set_node_attrs_ws(node_attr = "height", value = 0.001) %>%
    invert_selection() %>%
    set_node_attrs_ws(
      node_attr = "width", value = get_global_graph_attrs(.) %>%
        dplyr::filter(attr == "width") %>% .$value) %>%
    set_node_attrs_ws(
      node_attr = "height", value = get_global_graph_attrs(.) %>%
        dplyr::filter(attr == "width") %>% .$value) %>%
    clear_selection()

  render_graph(combo_graph)
}
