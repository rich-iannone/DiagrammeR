#' R + viz.js
#'
#' Make diagrams in R using `viz.js` with infrastructure provided by
#' pkg{htmlwidgets}.
#'
#' @param diagram Spec for a diagram as either text, filename string, or file
#'   connection.
#' @param engine String for the Graphviz layout engine; can be `dot` (default),
#'   `neato`, `circo`, or `twopi`.
#' @param allow_subst A boolean that enables/disables substitution
#'   functionality.
#' @param options Parameters supplied to the htmlwidgets framework.
#' @param width An optional parameter for specifying the width of the resulting
#'   graphic in pixels.
#' @param height An optional parameter for specifying the height of the
#'   resulting graphic in pixels.
#' @param envir The environment in which substitution functionality takes place.
#'
#' @return An object of class `htmlwidget` that will intelligently print itself
#'   into HTML in a variety of contexts including the R console, within R
#'   Markdown documents, and within Shiny output bindings.
#'
#' @export
grViz <- function(diagram = "",
                  engine = "dot",
                  allow_subst = TRUE,
                  options = NULL,
                  width = NULL,
                  height = NULL,
                  envir = parent.frame()) {

  # Check for a connection or file
  is_connection_or_file <-
    inherits(diagram[1], "connection") || file.exists(diagram[1])

  # Obtain the diagram text via `readLines()`
  if (is_connection_or_file) {
    diagram <- readLines(diagram, encoding = "UTF-8", warn = FALSE)
  }

  diagram <- paste0(diagram, collapse = "\n")

  if (allow_subst == TRUE) {
    diagram <- replace_in_spec(diagram, envir = envir)
  }

  # Single quotes within a diagram spec are problematic
  # so try to replace with `\"`
  diagram <- gsub(x = diagram, "'", "\"")

  # Forward options using `x`
  x <-
    list(
      diagram = diagram,
      config = list(
        engine = engine,
        options = options
      )
    )

  # Only use the Viewer for newer versions of RStudio,
  # but allow other, non-RStudio viewers
  viewer.suppress <-
    rstudioapi::isAvailable() &&
    !rstudioapi::isAvailable("0.99.120")

  # Create the widget
  htmlwidgets::createWidget(
    name = "grViz",
    x = x,
    width = width,
    height = height,
    package = "DiagrammeR",
    htmlwidgets::sizingPolicy(viewer.suppress = viewer.suppress)
  )
}

#' Widget output function for use in Shiny
#'
#' @param outputId Output variable to read from.
#' @param width A valid CSS unit for the width or a number, which will be
#'   coerced to a string and have `px` appended.
#' @param height A valid CSS unit for the height or a number, which will be
#'   coerced to a string and have `px` appended.
#'
#' @export
grVizOutput <- function(outputId,
                        width = "100%",
                        height = "400px") {

  htmlwidgets::shinyWidgetOutput(
    outputId = outputId,
    name = "grViz",
    width = width,
    height = height,
    package = "DiagrammeR"
  )
}

#' Widget render function for use in Shiny
#'
#' @param expr an expression that generates a DiagrammeR graph
#' @param env the environment in which to evaluate expr.
#' @param quoted is expr a quoted expression (with quote())? This is useful if
#'   you want to save an expression in a variable.
#' @seealso [grVizOutput()] for an example in Shiny.
#' @export
renderGrViz <- function(expr,
                        env = parent.frame(),
                        quoted = FALSE) {

  if (!quoted) expr <- substitute(expr)

  htmlwidgets::shinyRenderWidget(
    expr = expr,
    outputFunction = grVizOutput,
    env = env,
    quoted = TRUE
  )
}

#' Add MathJax-formatted equation text
#'
#' @param gv A `grViz` htmlwidget.
#' @param include_mathjax A `logical` to add mathjax JS. Change to `FALSE` if
#'   using with \pkg{rmarkdown} since MathJax will likely already be added.
#'
#' @return A `grViz` htmlwidget
#'
#' @export
add_mathjax <- function(gv = NULL,
                        include_mathjax = TRUE) {

  stopifnot(!is.null(gv), inherits(gv, "grViz"))

  gv$dependencies <-
    c(
      gv$dependencies,
      list(htmltools::htmlDependency(
        name = "svg_mathjax2",
        version = "0.1.0",
        src = c(href="https://cdn.rawgit.com/timelyportfolio/svg_mathjax2/master/"),
        script = "svg_mathjax2.js")))

  if (include_mathjax){
    htmltools::browsable(
      htmltools::tagList(
        gv,
        htmltools::tags$script(src = "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_SVG"),
        htmlwidgets::onStaticRenderComplete(
          "setTimeout(function(){new Svg_MathJax().install()}, 4000);"
        )
      ))
  } else {
    htmltools::browsable(htmltools::tagList(
      gv,
      htmlwidgets::onStaticRenderComplete(
        "setTimeout(function(){new Svg_MathJax().install()}, 4000);"
      )
    ))
  }
}
