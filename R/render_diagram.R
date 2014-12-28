#' Render diagram in viewer pane
#' @description This function renders the diagram code in RStudio's viewer pane.
#' @export render_diagram

render_diagram <- function(diagram_code){

# Create a blank 'index.html' file in a temporary directory
tempDir <- tempfile()
dir.create(tempDir)
htmlFile <- file.path(tempDir, "index.html")

# Add HTML to index.html file
writeLines(paste0("
<!DOCTYPE html>
  <html>
  <head>
  <script src=\"https://cdn.rawgit.com/knsv/mermaid/master/dist/mermaid.full.min.js\"></script>
    <meta charset=\"utf-8\">
      <title></title>
      
      </head>
      <body>
      <div class=\"mermaid\">",
diagram_code,
"     </div>
  </body>
  </html>
"), con = htmlFile)

# Display diagram in viewer pane
rstudio::viewer(htmlFile)

}
