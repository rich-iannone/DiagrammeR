#' R + mermaid.js
#'
#' Make diagrams in R using
#' \href{https://github.com/mermaid-js/mermaid/wiki}{mermaid.js} with
#' infrastructure provided by \href{http://www.htmlwidgets.org/}{htmlwidgets}.
#'
#' @param diagram Diagram in mermaid markdown-like language or file (as a
#'   connection or file name) containing a diagram specification. If no diagram
#'   is provided `diagram = ""` then the function will assume that a diagram
#'   will be provided by [htmltools::tags()] and `DiagrammeR` is just being used
#'   for dependency injection.
#' @param ... Other arguments and parameters you would like to send to
#'   JavaScript.
#' @param width The width of the resulting graphic in pixels.
#' @param height The height of the resulting graphic in pixels.
#'
#' @return An object of class `htmlwidget` that will intelligently print itself
#'   into HTML in a variety of contexts including the R console, within R
#'   Markdown documents, and within Shiny output bindings.
#'
#' @examples
#' # Create a simple graph running left to right (note
#' # that the whitespace is not important)
#' # DiagrammeR("
#' #   graph LR
#' #     A-->B
#' #     A-->C
#' #     C-->E
#' #     B-->D
#' #     C-->D
#' #     D-->F
#' #     E-->F
#' # ")
#'
#' # Create the equivalent graph but have it running
#' # from top to bottom
#' # DiagrammeR("
#' #    graph TB
#' #    A-->B
#' #    A-->C
#' #    C-->E
#' #    B-->D
#' #    C-->D
#' #    D-->F
#' #    E-->F
#' # ")
#'
#' # Create a graph with different node shapes and
#' # provide fill styles for each node
#' # DiagrammeR("graph LR;A(Rounded)-->B[Squared];B-->C{A Decision};
#' #  C-->D[Square One];C-->E[Square Two];
#' #  style A fill:#E5E25F;  style B fill:#87AB51; style C fill:#3C8937;
#' #  style D fill:#23772C;  style E fill:#B6E6E6;"
#' # )
#'
#' # Load in the 'mtcars' dataset
#' # data(mtcars)
#' # connections <- sapply(
#' #  1:ncol(mtcars)
#' #   ,function(i) {
#' #      paste0(
#' #         i
#' #       ,"(",colnames(mtcars)[i],")---"
#' #       ,i,"-stats("
#' #       ,paste0(
#' #         names(summary(mtcars[,i]))
#' #         ,": "
#' #         ,unname(summary(mtcars[,i]))
#' #         ,collapse="<br/>"
#' #       )
#' #       ,")"
#' #    )
#' #  }
#' # )
#'
#' # Create a diagram using the 'connections' object
#' # DiagrammeR(
#' #    paste0(
#' #      "graph TD;", "\n",
#' #      paste(connections, collapse = "\n"),"\n",
#' #      "classDef column fill:#0001CC, stroke:#0D3FF3, stroke-width:1px;" ,"\n",
#' #      "class ", paste0(1:length(connections), collapse = ","), " column;"
#' #    )
#' #  )
#'
#' # Also with `DiagrammeR()`, you can use tags
#' # from `htmltools` (just make sure to use
#' # `class = "mermaid"`)
#' library(htmltools)
#' # diagramSpec = "
#' # graph LR;
#' #   id1(Start)-->id2(Stop);
#' #   style id1 fill:#f9f,stroke:#333,stroke-width:4px;
#' #   style id2 fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5;
#' # "
#' # html_print(tagList(
#' #   tags$h1("R + mermaid.js = Something Special")
#' #   ,tags$pre(diagramSpec)
#' #   ,tags$div(class="mermaid", diagramSpec)
#' #   ,DiagrammeR()
#' # ))
#'
#' # Create a sequence diagram
#' # DiagrammeR("
#' # sequenceDiagram;
#' #    customer->>ticket seller: ask for a ticket;
#' #    ticket seller->>database: seats;
#' #    alt tickets available
#' #      database->>ticket seller: ok;
#' #      ticket seller->>customer: confirm;
#' #      customer->>ticket seller: ok;
#' #      ticket seller->>database: book a seat;
#' #      ticket seller->>printer: print a ticket;
#' #    else sold out
#' #      database->>ticket seller: none left;
#' #      ticket seller->>customer: sorry;
#' #    end
#' # ")
#'
#' @import htmlwidgets
#' @export
mermaid <- function(diagram = "",
                    ...,
                    width = NULL,
                    height = NULL) {

  # Check for a connection or file
  is_connection_or_file <-
    inherits(diagram[1], "connection") || file.exists(diagram[1])

  # Obtain the diagram text via `readLines()`
  if (is_connection_or_file) {

    diagram <- readLines(diagram, encoding = "UTF-8", warn = FALSE)
    diagram <- paste0(diagram, collapse = "\n")

  } else {

    # Check for vector with length > 1 and concatenate
    if (length(diagram) > 1) {

      nosep <- grep("[;\n]", diagram)

      if (length(nosep) < length(diagram)) {

        diagram[-nosep] <-
          sapply(diagram[-nosep], function(c) { paste0(c, ";") })
      }

      diagram = paste0(diagram, collapse = "")
    }
  }

  # Forward options using x
  x <- list(diagram = diagram)

  # Create widget
  htmlwidgets::createWidget(
    name = "DiagrammeR",
    x = x,
    width = width,
    height = height,
    package = "DiagrammeR"
  )
}
