#' R + mermaid.js
#'
#' Make diagrams in R using \href{https://github.com/knsv/mermaid/wiki}{mermaid.js}
#' with infrastructure provided by \href{http://www.htmlwidgets.org/}{htmlwidgets}.
#' 
#' @param diagram string diagram in mermaid markdown-like language.
#' If no diagram is provided \code{diagram = ""} then the function will assume that
#' a diagram will be provided by \code{\link[htmltools]{tags}} and
#' \code{DiagrammeR} is just being used for dependency injection.
#' @return An object of class \code{htmlwidget} that will
#' intelligently print itself into HTML in a variety of contexts
#' including the R console, within R Markdown documents,
#' and within Shiny output bindings.
#' @examples 
#' # note the whitespace is not important
#' DiagrammeR("
#'   graph LR;
#'     A-->B;
#'     A-->C;
#'     C-->E;
#'     B-->D;
#'     C-->D;
#'     D-->F;
#'     E-->F;
#' ")
#' 
#' DiagrammeR("
#'    graph TB;
#'    A-->B;
#'    A-->C;
#'    C-->E;
#'    B-->D;
#'    C-->D;
#'    D-->F;
#'    E-->F;
#' ")
#' 
#' DiagrammeR("graph LR;A(Rounded)-->B[Squared];B-->C{A Decision};
#'  C-->D[Square One];C-->E[Square Two];
#'  style A fill:#E5E25F;  style B fill:#87AB51; style C fill:#3C8937;
#'  style D fill:#23772C;  style E fill:#B6E6E6;"
#' )
#' 
#' # Load in the 'mtcars' dataset
#' data(mtcars)
#' # Obtain column names
#' column_names <- colnames(mtcars)
#' # Use for in loop to generate summary strings for each mtcars column
#' for (i in 1:length(column_names)){
#'  if (i == 1) connections <- vector(mode = "character", length = 0L)
#'
#'  connections <-
#'  c(connections,
#'    paste0(i, "(", column_names[i], ")---", i, "-stats(",
#'           "min: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[((i - 1) * 6) + 1]))), "<br/>",
#'           "1Q: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[((i - 1) * 6) + 2]))), "<br/>",
#'           "med: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[((i - 1) * 6) + 3]))), "<br/>",
#'           "mean: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[((i - 1) * 6) + 4]))), "<br/>",
#'           "3Q: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[((i - 1) * 6) + 5]))), "<br/>",
#'           "max: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[((i - 1) * 6) + 6]))),
#'    ")"))
#' }
#' DiagrammeR("
#'    paste0(
#'      "graph TD;", "\n",
#'      paste(connections, collapse = "\n"),"\n",
#'      "classDef column fill:#0001CC, stroke:#0D3FF3, stroke-width:1px;" ,"\n",
#'      "class ", paste0(1:length(column_names), collapse = ","), " column;"
#'    )
#'  )
#'
#' @import htmlwidgets
#'
#' @export
DiagrammeR <- function(diagram = "", width = NULL, height = NULL) {

  # forward options using x
  x = list(
    diagram = diagram
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'DiagrammeR',
    x,
    width = width,
    height = height,
    package = 'DiagrammeR'
  )
}

#' Widget output function for use in Shiny
#'
#' @export
DiagrammeROutput <- function(outputId, width = '100%', height = '400px'){
  shinyWidgetOutput(outputId, 'DiagrammeR', width, height, package = 'DiagrammeR')
}

#' Widget render function for use in Shiny
#'
#' @export
renderDiagrammeR <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, DiagrammeROutput, env, quoted = TRUE)
}
