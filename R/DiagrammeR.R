#' R + mermaid.js
#'
#' Make diagrams in R using \href{https://github.com/mdaines/viz.js}{viz.js} or
#' \href{https://github.com/mermaid-js/mermaid}{mermaid.js} with infrastructure
#' provided by \href{http://www.htmlwidgets.org/}{htmlwidgets}.
#'
#' @param diagram The diagram in `graphviz` or `mermaid` format, or, a file (as
#'   a connection or file name) containing a diagram specification. The
#'   recommended filename extensions are `.gv` and `.mmd` for the Graphviz and
#'   the mermaid diagram specifications, respectively. If no diagram is provided
#'   (`diagram = ""`) then the function will assume that a diagram will be
#'   provided by [htmltools::tags()] and `DiagrammeR` is just being used for
#'   dependency injection.
#' @param type A string, either `mermaid` (default) or `grViz` indicating the
#'   type of diagram spec and the desired parser/renderer.
#' @param ... Any other parameters to pass to `grViz` or `mermaid`
#'
#' @return An object of class `htmlwidget` that will intelligently print itself
#'   into HTML in a variety of contexts including the R console, within R
#'   Markdown documents, and within Shiny output bindings.
#'
#' @examples
#' \dontrun{
#' # note the whitespace is not important
#' DiagrammeR("
#'   graph LR
#'     A-->B
#'     A-->C
#'     C-->E
#'     B-->D
#'     C-->D
#'     D-->F
#'     E-->F
#' ")
#'
#' DiagrammeR("
#'    graph TB
#'    A-->B
#'    A-->C
#'    C-->E
#'    B-->D
#'    C-->D
#'    D-->F
#'    E-->F
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
#' connections <- sapply(
#'  1:ncol(mtcars)
#'   ,function(i) {
#'      paste0(
#'         i
#'       ,"(",colnames(mtcars)[i],")---"
#'       ,i,"-stats("
#'       ,paste0(
#'         names(summary(mtcars[,i]))
#'         ,": "
#'         ,unname(summary(mtcars[,i]))
#'         ,collapse="<br/>"
#'       )
#'       ,")"
#'    )
#'  }
#' )
#'
#' DiagrammeR(
#'    paste0(
#'      "graph TD;", "\n",
#'      paste(connections, collapse = "\n"),"\n",
#'      "classDef column fill:#0001CC, stroke:#0D3FF3, stroke-width:1px;" ,"\n",
#'      "class ", paste0(1:length(connections), collapse = ","), " column;"
#'    )
#'  )
#'
#' # also with DiagrammeR() you can use tags from htmltools
#' # just make sure to use class = "mermaid"
#' library(htmltools)
#' diagramSpec = "
#' graph LR;
#'   id1(Start)-->id2(Stop);
#'   style id1 fill:#f9f,stroke:#333,stroke-width:4px;
#'   style id2 fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5;
#' "
#' html_print(tagList(
#'   tags$h1("R + mermaid.js = Something Special")
#'   ,tags$pre(diagramSpec)
#'   ,tags$div(class="mermaid",diagramSpec)
#'   ,DiagrammeR()
#' ))
#'
#' # sequence diagrams
#' # Using this "How to Draw a Sequence Diagram"
#' #   http://www.cs.uku.fi/research/publications/reports/A-2003-1/page91.pdf
#' # draw some sequence diagrams with DiagrammeR
#'
#' library(DiagrammeR)
#'
#' DiagrammeR("
#' sequenceDiagram;
#'    customer->>ticket seller: ask for ticket;
#'    ticket seller->>database: seats;
#'    alt tickets available
#'      database->>ticket seller: ok;
#'      ticket seller->>customer: confirm;
#'      customer->>ticket seller: ok;
#'      ticket seller->>database: book a seat;
#'      ticket seller->>printer: print ticket;
#'    else sold out
#'      database->>ticket seller: none left;
#'      ticket seller->>customer:  sorry;
#'    end
#' ")
#' }
#'
#' @import htmlwidgets
#' @export
DiagrammeR <- function(diagram = "", type = "mermaid", ...) {

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # DiagrammeR will serve as a wrapper function for mermaid and grVis
  if (grepl(x = type, pattern = "[m,M](erm).*")) {

    mermaid(diagram, ... )

  } else if (grepl(x = type, pattern = "[g,G]?[r,R]?.*[v,V][i].*" )) {
    grViz(diagram, ... )

  } else {

    emit_error(
      fcn_name = fcn_name,
      reasons = "The type should be `mermaid` or `grViz`")
  }
}

#' Widget output function for use in Shiny
#'
#' @param outputId Output variable to read from
#' @param width A valid CSS unit for the width or a number, which will be
#'   coerced to a string and have `px` appended.
#' @param height A valid CSS unit for the height or a number, which will be
#'   coerced to a string and have `px` appended.
#' @export
DiagrammeROutput <- function(outputId,
                             width = '100%',
                             height = 'auto') {

  htmlwidgets::shinyWidgetOutput(
    outputId,
    'DiagrammeR',
    width,
    height,
    package = 'DiagrammeR')
}

#' Widget render function for use in Shiny
#'
#' @param expr An expression that generates a DiagrammeR graph
#' @param env The environment in which to evaluate expr.
#' @param quoted Is expr a quoted expression (with quote())? This is useful if
#'   you want to save an expression in a variable.
#' @export
renderDiagrammeR <- function(expr,
                             env = parent.frame(),
                             quoted = FALSE) {

  if (!quoted) expr <- substitute(expr)

  htmlwidgets::shinyRenderWidget(
    expr,
    DiagrammeROutput,
    env,
    quoted = TRUE)
}
