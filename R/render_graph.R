#' Render the graph or output in various formats
#' Using a \code{dgr_graph} object, either render graph in the Viewer or output in various formats.
#' @param graph a \code{dgr_graph} object, created using the \code{create_graph} function.
#' @param output a string specifying the output type; \code{graph} (the default) renders the graph using the \code{grViz} function, \code{DOT} outputs DOT code for the graph, and \code{SVG} provides SVG code for the rendered graph.
#' @param width an optional parameter for specifying the width of the resulting graphic in pixels.
#' @param height an optional parameter for specifying the height of the resulting graphic in pixels.
#' @import stringr
#' @export render_graph

render_graph <- function(graph,
                         output = "graph",
                         width = NULL,
                         height = NULL){

  stopifnot(class(graph) == "dgr_graph")

  dot_code <- graph$dot_code

  if (output == "DOT"){

    return(dot_code)
  }

  if (output == "SVG"){

    svg_code <- exportSVG(grViz(diagram = dot_code,
                                width = width,
                                height = height))

    return(svg_code)
  }

  if (output == "graph"){

    # If the DOT code contains references to images, create an HTML file that
    # can access those images and display them in the graph
    if (grepl("\\[.*?img[ ]*?=[ ]*?", dot_code) |
        grepl("\\[.*?icon[ ]*?=[ ]*?", dot_code)){

      all_replacement_nodes_circles <-
        str_detect(dot_code, "\\[.*?shape[ ]*?=[ ]*?'circle'.*?, img.*?].*") &
        str_detect(dot_code, "\\[.*?shape[ ]*?=[ ]*?'circle'.*?, icon.*?].*")

      # Add in 'shape = circle' for all nodes containing to image links
      if (all_replacement_nodes_circles == FALSE){

        dot_code <- str_replace_all(dot_code, "\\[(.*?img.*?)]",
                                    "\\[\\1, shape = 'circle']")
        dot_code <- str_replace_all(dot_code, "\\[(.*?icon.*?)]",
                                    "\\[\\1, shape = 'circle']")
      }

      # For icons, substitute their shorthand names for web links to png graphics
      while (grepl("icon[ ]*?=[ ]*?", dot_code)){

        icon_name <- gsub("'", "",
                          gsub("icon[ ]*?=[ ]*?'", "",
                               str_extract(dot_code, "icon[ ]*?=[ ]*?'(.*?)'")))

        icon_link <- image_icon(icon_name)

        dot_code <- str_replace(dot_code, icon_name, icon_link)
        dot_code <- str_replace(dot_code, "icon", "img")
      }

      gv <- grViz(dot_code)

      gv_SVG <- exportSVG(gv)

      if (str_detect(gv$x$diagram, "\\[.*?img[ ]*?=[ ]*?")){

        string_count <- str_count(gv$x$diagram, "\\[.*?img[ ]*?=[ ]*?")

        gv$x$diagram <- str_replace_all(gv$x$diagram,
                                        "(.*)img([ ]*?=[ ]*?)", "\\1id\\2")

        # Create vectors of node ID and image links
        image_links <- unlist(str_extract_all(gv$x$diagram, "(http.*?png)"))

        id_collection <-
          gsub("^\"([a-zA-Z0-9_]*)\".*$", "\\1",
               unlist(str_extract_all(gv$x$diagram,
                                      "(\"[a-zA-Z0-9_]*?\") \\[.*?id.*?\\]")))
      }

      # Extract 'g' elements from SVG that correspond to IDs
      # in 'id_collection'
      g_elements <-
        unlist(str_extract_all(gv_SVG,
                               paste0("<g id=\"node.*?\" class=\"node\"><title>(",
                                      paste(id_collection, collapse = "|"),")</title>",
                                      "[a-zA-Z]*?.*\\n.*\\n.*\\n.*\\n")))


      g_translate_x_y <-
        as.numeric(unlist(strsplit(gsub("\\)", "",
                                        gsub("translate\\(", "",
                                             str_extract(gv_SVG,
                                                         "translate\\(.*?\\)"))), " ")))

      # Obtain the positioning metrics for the ellipses
      for (i in 1:length(g_elements)){

        if (i == 1){
          cx_value <- cy_value <- rx_value <- ry_value <- vector(mode = "numeric")
        }

        cx_value <-
          c(cx_value,
            as.numeric(gsub("cx=", "",
                            gsub("\"", "",
                                 (str_extract(g_elements[i],
                                              "cx=\"[0-9]*?\""))))))

        cy_value <-
          c(cy_value,
            as.numeric(gsub("cy=", "",
                            gsub("\"", "",
                                 (str_extract(g_elements[i],
                                              "cy=\"[0-9-]*?\""))))))

        rx_value <-
          c(rx_value,
            as.numeric(gsub("rx=", "",
                            gsub("\"", "",
                                 (str_extract(g_elements[i],
                                              "rx=\"[0-9\\.]*?\""))))))

        ry_value <-
          c(ry_value,
            as.numeric(gsub("ry=", "",
                            gsub("\"", "",
                                 (str_extract(g_elements[i],
                                              "ry=\"[0-9\\.]*?\""))))))
      }

      # Perform calculations for image positioning
      cx_minus_rx <- round(cx_value - rx_value, 2)

      cy_minus_ry <- round(cy_value - ry_value, 2)

      image_height_width <- round(rx_value * 2, 2)

      # Create an SVG image statement and position the graphic
      svg_image_statement <-
        paste0("<image xlink:href=\"",
               image_links, "\" x=\"", cx_minus_rx,
               "\" y=\"", cy_minus_ry, "\" height=\"",
               image_height_width, "\" width=\"",
               image_height_width, "\"/>")

      g_elements_less_title_polygon <-
        str_replace(g_elements, "<ellipse.*?\\n.*?\\n", "")


      for (i in 1:length(g_elements_less_title_polygon)){

        if (i == 1) svg_statements <- vector(mode = "character")

        svg_statements <-
          c(svg_statements,
            str_c(gsub("</g>\n", "", g_elements_less_title_polygon[i]),
                  svg_image_statement[i], "</g>\n"))
      }

      # Remove the SVG statements for the polygon text and the shape itself
      for (i in 1:length(g_elements_less_title_polygon)){
        gv_SVG <-
          str_replace(gv_SVG,
                      unlist(str_extract_all(gv_SVG,
                                             paste0("<g id=\"node.*?\" class=\"node\"><title>(",
                                                    paste(id_collection[i], collapse = "|"),")</title>",
                                                    "[a-zA-Z]*?.*\\n.*\\n.*\\n.*\\n"))),
                      svg_statements[i])
      }

      # Remove SVG header from file and add in HTML header
      gv_SVG <-
        str_replace(gv_SVG,
                    "^.*?\\n.*?\\n.*?\\n.*?\\n.*?\\n.*?\\n",
                    "<!DOCTYPE html>\n<html>\n<body>\n")

      # Add the closing tags for the HTML file
      gv_HTML <-
        str_replace(gv_SVG,
                    "$",
                    "\n</body>\n</html>\n")

      # Create a temporary file
      dir <- tempfile()
      dir.create(dir)

      # Name the temporary file 'index.html'
      htmlFile <- file.path(dir, "index.html")

      # Write the 'gv_HTML' file to 'index.html'
      writeLines(gv_HTML, htmlFile)

      # Display the HTML file in the Viewer
      rstudioapi::viewer(htmlFile)
    }

    # If the DOT code contains no references to external images, then simply
    # use the 'grViz' function to render the graph
    if (grepl("\\[.*?img[ ]*?=[ ]*?", dot_code) == FALSE){

      grViz(diagram = dot_code, width = width, height = height)
    }
  }
}
