#' R + mermaid.js
#' @description Make diagrams in R using \href{https://github.com/knsv/mermaid/wiki}{mermaid.js} with infrastructure provided by \href{http://www.htmlwidgets.org/}{htmlwidgets}.
#' @param diagram diagram in mermaid markdown-like language or
#'  file (as a connection or file name) containing a diagram specification.
#' If no diagram is provided \code{diagram = ""} then the function will assume that
#' a diagram will be provided by \code{\link[htmltools]{tags}} and
#' \code{DiagrammeR} is just being used for dependency injection.
#' @param ... other arguments and parameters you would like to send to Javascript
#' @param width the width of the resulting graphic in pixels.
#' @param height the height of the resulting graphic in pixels.
#' @return An object of class \code{htmlwidget} that will
#' intelligently print itself into HTML in a variety of contexts
#' including the R console, within R Markdown documents,
#' and within Shiny output bindings.
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
#'   ,function(i){
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
#'  http://www.cs.uku.fi/research/publications/reports/A-2003-1/page91.pdf
#' draw some sequence diagrams with DiagrammeR
#'
#' library(DiagrammeR)
#'
#' DiagrammeR("
#' sequenceDiagram;
#'    customer->>ticket seller: ask ticket;
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

mermaid <- function(diagram = "", ..., width = NULL, height = NULL) {

  # check for a connection or file
  if (inherits(diagram, "connection") || file.exists(diagram)) {
    diagram <- readLines(diagram, warn = FALSE)
    diagram <- paste0(diagram, collapse = "\n")
  } else {
    # check for vector with length > 1 and concatenate
    if (length(diagram) > 1 ){

      nosep <- grep(x = diagram, pattern = "[;\n]")

      if (length(nosep) < length(diagram)){
        diagram[-nosep] <- sapply(diagram[-nosep],
                                  function(c){paste0(c, ";")})
      }

      diagram = paste0( diagram, collapse = "" )
    }
  }

  # forward options using x
  x <- list(
    diagram = diagram
  )

  # create widget
  htmlwidgets::createWidget(name = 'DiagrammeR',
                            x,
                            width = width,
                            height = height,
                            package = 'DiagrammeR')
}
