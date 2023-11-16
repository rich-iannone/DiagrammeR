#' Render the graph in various formats
#'
#' @description
#'
#' Using a `dgr_graph` object, render the graph in the RStudio Viewer.
#'
#' @param graph A graph object of class `dgr_graph`.
#' @param layout A string specifying a layout type to use for node placement in
#'   this rendering. Possible layouts include: `nicely`, `circle`, `tree`, `kk`,
#'   and `fr`.
#' @param output A string specifying the output type; `graph` (the default)
#'   renders the graph using the [grViz()] function and `visNetwork` renders the
#'   graph using the [visnetwork()] function.
#' @param as_svg An option to render the graph as an SVG document.
#' @param title An optional title for a graph when using `output = "graph"`.
#' @param width An optional parameter for specifying the width of the resulting
#'   graphic in pixels.
#' @param height An optional parameter for specifying the height of the
#'   resulting graphic in pixels.
#' @examples
#' if (interactive()) {
#'
#'   # Render a graph that's a
#'   # balanced tree
#'   create_graph() %>%
#'     add_balanced_tree(
#'       k = 2, h = 3
#'     ) %>%
#'     render_graph()
#'
#'   # Use the `tree` layout for
#'   # better node placement in this
#'   # hierarchical graph
#'   create_graph() %>%
#'     add_balanced_tree(
#'       k = 2, h = 3
#'     ) %>%
#'     render_graph(layout = "tree")
#'
#'   # Plot the same tree graph but
#'   # don't show the node ID values
#'   create_graph() %>%
#'     add_balanced_tree(
#'       k = 2, h = 3
#'     ) %>%
#'     set_node_attr_to_display() %>%
#'     render_graph(layout = "tree")
#'
#'   # Create a circle graph
#'   create_graph() %>%
#'     add_gnm_graph(
#'       n = 55,
#'       m = 75,
#'       set_seed = 23
#'     ) %>%
#'     render_graph(
#'       layout = "circle"
#'     )
#'
#'   # Render the graph using the
#'   # `visNetwork` output option
#'   create_graph() %>%
#'     add_balanced_tree(
#'       k = 2, h = 3
#'     ) %>%
#'     render_graph(
#'       output = "visNetwork"
#'     )
#' }
#'
#' @family Display and Save
#'
#' @export
render_graph <- function(
    graph,
    layout = NULL,
    output = NULL,
    as_svg = FALSE,
    title = NULL,
    width = NULL,
    height = NULL
) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  output <- output %||% "graph"

  # Not allowing partial matching.
  output <- rlang::arg_match0(output, c("graph", "visNetwork"))
  # Check layout is a single string or NULL
  check_string(layout, allow_null = TRUE)

  if (!is.null(layout)) {
    rlang::arg_match0(layout, c("circle", "tree", "kk", "fr", "nicely"))
  }

  # Return early if output is visNetwork.
  if (output == "visNetwork") {
    return(visnetwork(graph))
  }

  # output = "graph" code with out = grViz

  # Add title as attribute
  if (!is.null(title)) {

    graph <- add_global_graph_attrs(graph, "label", title, "graph")
    graph <- add_global_graph_attrs(graph, "labelloc", "t", "graph")
    graph <- add_global_graph_attrs(graph, "labeljust", "c", "graph")
    graph <- add_global_graph_attrs(graph, "fontname", "Helvetica", "graph")
    graph <- add_global_graph_attrs(graph, "fontcolor", "gray30", "graph")
  }

  # If no fillcolor provided, use default; if no default available, use white
  if (nrow(graph$nodes_df) > 0 &&
      !("fillcolor" %in% colnames(graph$nodes_df))) {

    if ("fillcolor" %in% graph$global_attrs$attr) {

      graph$nodes_df$fillcolor <-
        graph$global_attrs %>%
        dplyr::filter(attr == "fillcolor", attr_type == "node") %>%
        dplyr::pull("value") %>%
        as.character()
    } else {
      graph$nodes_df$fillcolor <- "white"
    }
  }

  # If fillcolor is available and there are NA values,
  # replace NAs with default color if available
  if (nrow(graph$nodes_df) > 0 &&
      rlang::has_name(graph$nodes_df, "fillcolor") &&
      "fillcolor" %in% graph$global_attrs$attr) {

    graph$nodes_df$fillcolor[which(is.na(graph$nodes_df$fillcolor))] <-
      graph$global_attrs[which(graph$global_attrs$attr == "fillcolor"), 2]
  }

  # Translate X11 colors to hexadecimal colors
  if ("fillcolor" %in% colnames(graph$nodes_df)) {

    graph$nodes_df <-
      graph$nodes_df %>%
      dplyr::left_join(
        x11_hex() %>%
          dplyr::as_tibble() %>%
          dplyr::mutate(hex = toupper(hex)),
        by = c("fillcolor" = "x11_name")
      ) %>%
      dplyr::mutate(
        fillcolor = dplyr::coalesce(hex, fillcolor),
        .keep = "unused"
      )
  }

  # Use adaptive font coloring for nodes that have a fill color
  if ("fillcolor" %in% colnames(graph$nodes_df) &&
      !"fontcolor" %in% colnames(graph$nodes_df)
  ) {

    graph$nodes_df$fontcolor <-
      tibble::tibble(value = graph$nodes_df$fillcolor) %>%
      dplyr::mutate(value_x = contrasting_text_color(background_color = value)) %>%
      dplyr::pull("value_x")
  }

  # Modify nodes df if a specific layout is requested.
  # and is one of the accepted values ("circle", "tree", "kk", "fr", "nicely")
  if (!is.null(layout)) {

    graph <-
      add_global_graph_attrs(
        graph,
        attr = "layout",
        value = "neato",
        attr_type = "graph"
      )

    # Remove existing x and y columns in nodes_df
    # to replace them with the layout coords
    if ("x" %in% colnames(graph$nodes_df)) {
      graph$nodes_df$x <- NULL
    }

    if ("y" %in% colnames(graph$nodes_df)) {
      graph$nodes_df$y <- NULL
    }

    # layout = "tree" is special, because layout_with_sugiyama creates two
    # different layouts
    if (layout == "tree") {
      m_coords <-
        to_igraph(graph) %>%
        igraph::layout_with_sugiyama()
      m_coords <- m_coords[["layout"]]

      # Safety
      if (!is.matrix(m_coords) && nrow(m_coords) == 0) {

        cli::cli_abort("The tree coords should be a matrix", .internal = TRUE)
      }

      coords <- data.frame(
        x = m_coords[, 1],
        y = m_coords[, 2],
        stringsAsFactors = FALSE
      )
    } else {
      # Simple cases using defaults for kk, fr, nicely, and circle.
      fn_igraph <- switch(layout,
                          "kk" = igraph::layout_with_kk,
                          "fr" = igraph::layout_with_fr,
                          "nicely" = igraph::layout_nicely,
                          "circle" = igraph::layout_in_circle
      )

      m_coords <- graph %>%
        to_igraph() %>%
        fn_igraph()

      if (!is.matrix(m_coords)) {
        cli::cli_abort("The {.val {layout}} coords should be a matrix", .internal = TRUE)
      }

      coords <- data.frame(
        x = m_coords[, 1],
        y = m_coords[, 2],
        stringsAsFactors = FALSE
      )
    }

    # Corrections for layout = "circle"
    if (layout == "circle") {
      n_nodes <- count_nodes(graph)

      if (n_nodes == 0) {
        cli::cli_warn("No nodes exist? in the circle graph?")
        coords$x <- NaN
        coords$y <- NaN
      } else {
        # Previously as x * (count_nodes(graph) + (0.25 * count_nodes(graph))) / count_nodes(graph)
        # which can be simplified to x * 1.25? if n_nodes > 0
        # coords$x <- coords$x * (n_nodes + 0.25 * n_nodes) / n_nodes
        coords$x <- coords$x * 1.25
        coords$y <- coords$y * 1.25
      }

    }
    # Bind (x, y) coordinates to the graph's
    # internal NDF
    graph$nodes_df <-
      dplyr::bind_cols(graph$nodes_df, coords)
  }

  if (as_svg || any(c("image", "fa_icon") %in% colnames(get_node_df(graph)))) {

    if (as_svg && !rlang::is_installed("DiagrammeRsvg")) {
      rlang::inform("Use `as_svg = FALSE` if you don't want to install DiagrammeRsvg.")
    }

    # Stop function if `DiagrammeRsvg` package is not available
    rlang::check_installed("DiagrammeRsvg", "to render the graph to SVG.")

    # Generate DOT code
    dot_code <- generate_dot(graph)

    # create display to make return work when `as_svg = TRUE` #482
    display <- grViz(diagram = dot_code, width = width, height = height)

    # Get a vector of SVG lines
    svg_vec <-
      strsplit(DiagrammeRsvg::export_svg(
        grViz(diagram = dot_code)
      ), "\n") %>%
      unlist()

    # Get a tibble of SVG data
    svg_tbl <- get_svg_tbl(svg_vec)

    svg_lines <-
      "<svg display=\"block\" margin=\"0 auto\" position=\"absolute\" width=\"100%\" height=\"100%\""

    svg_line_no <- svg_tbl %>%
      dplyr::filter(type == "svg") %>%
      dplyr::pull("index")

    # Modify <svg> attrs
    svg_vec[svg_line_no] <- svg_lines

    if ("image" %in% colnames(graph %>% get_node_df())) {
      node_id_images <-
        graph %>%
        get_node_df() %>%
        dplyr::select("id", "image") %>%
        dplyr::filter(nzchar(image)) %>%
        dplyr::pull("id")

      filter_lines <-
        graph %>%
        get_node_df() %>%
        dplyr::select("id", "image") %>%
        dplyr::filter(nzchar(image)) %>%
        dplyr::mutate(filter_lines = as.character(glue::glue("<filter id=\"{id}\" x=\"0%\" y=\"0%\" width=\"100%\" height=\"100%\"><feImage xlink:href=\"{image}\"/></filter>"))) %>%
        dplyr::pull("filter_lines") %>%
        paste(collapse = "\n")

      filter_shape_refs <- as.character(glue::glue(" filter=\"url(#{node_id_images})\" "))

      svg_shape_nos <-
        svg_tbl %>%
        dplyr::filter(node_id %in% node_id_images) %>%
        dplyr::filter(type == "node_block") %>%
        dplyr::pull("index")

      svg_shape_nos <- svg_shape_nos + 3
      svg_text_nos <- svg_shape_nos + 1

      # Modify shape lines
      for (i in seq(node_id_images)) {
        svg_vec[svg_shape_nos[i]] <-
          sub(" ", paste0(filter_shape_refs[i]), svg_vec[svg_shape_nos[i]])

        svg_vec[svg_text_nos[i]] <- ""
      }

      # Add in <filter> lines
      svg_vec[svg_line_no + 1] <-
        paste0(svg_vec[svg_line_no + 1], "\n\n", filter_lines, "\n")
    }

    # # Get the name of the function
    # if ("fa_icon" %in% colnames(graph %>% get_node_df())) {
    #
    #   # Using a fontawesome icon requires the fontawesome package;
    #   # if it's not present, stop with a message
    #   if (requireNamespace("fontawesome", quietly = TRUE)) {
    #
    #     node_id_fa <-
    #       graph %>%
    #       get_node_df() %>%
    #       dplyr::select(id, fa_icon) %>%
    #       dplyr::filter(fa_icon != "") %>%
    #       dplyr::filter(!is.na(fa_icon)) %>%
    #       dplyr::mutate(fa_uri = NA_character_)
    #
    #     node_id_svg <-
    #       node_id_fa %>%
    #       dplyr::pull(id)
    #
    #     for (i in seq(nrow(node_id_fa))) {
    #
    #       random_name <- paste(sample(letters[1:10], 10), collapse = "")
    #       tmp_svg_file <- paste0(random_name, ".svg")
    #
    #       fa_icon <- node_id_fa[i, ]$fa_icon
    #       id <- node_id_fa[i, ]$id
    #
    #       writeLines(fontawesome::fa(fa_icon), tmp_svg_file)
    #
    #       svg_uri <- get_image_uri(tmp_svg_file)
    #
    #       file.remove(tmp_svg_file)
    #
    #       node_id_fa[i, "fa_uri"] <-
    #         as.character(glue::glue("<filter id=\"{id}\" x=\"0%\" y=\"0%\" width=\"100%\" height=\"100%\"><feImage xlink:href=\"{svg_uri}\"/></filter>"))
    #     }
    #
    #     filter_lines <-
    #       node_id_fa %>%
    #       dplyr::pull(fa_uri) %>%
    #       paste(collapse = "\n")
    #
    #     filter_shape_refs <- as.character(glue::glue(" filter=\"url(#{node_id_svg})\" "))
    #
    #     svg_shape_nos <-
    #       svg_tbl %>%
    #       dplyr::filter(node_id %in% node_id_svg) %>%
    #       dplyr::filter(type == "node_block") %>%
    #       dplyr::pull(index)
    #
    #     svg_shape_nos <- svg_shape_nos + 3
    #     svg_text_nos <- svg_shape_nos + 1
    #
    #     # Modify shape lines
    #     for (i in seq_len(node_id_svg)) {
    #
    #       svg_vec[svg_shape_nos[i]] <-
    #         sub(" ", paste0(filter_shape_refs[i]), svg_vec[svg_shape_nos[i]])
    #
    #       svg_vec[svg_text_nos[i]] <- ""
    #     }
    #
    #     # Add in <filter> lines
    #     svg_vec[svg_line_no + 1] <-
    #       paste0(svg_vec[svg_line_no + 1], "\n\n", filter_lines, "\n")
    #   }
    #
    #   svg_vec_1 <- paste(svg_vec, collapse = "\n")
    #
    #   display <- htmltools::browsable(htmltools::HTML(svg_vec_1))
    #
    # } else {
    #
    #   cli::cli_abort(
    #     c(
    #       "Cannot currently render FontAwesome icons.",
    #       "please install the `fontawesome` package and retry",
    #       "pkg installed using `devtools::install_github('rstudio/fontawesome')`"))
    # }
  } else {
    # Generate DOT code
    dot_code <- generate_dot(graph)

    # Generate a `grViz` object
    grVizObject <-
      grViz(
        diagram = dot_code,
        width = width,
        height = height
      )

    display <- grVizObject
  }

  display
}
