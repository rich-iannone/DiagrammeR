#' Several examples of diagrams that can be created
#' @description This function provides several examples for diagramming code and output
#' @import htmltools
#' @export diagram_examples

diagram_examples <- function(){
  diagram_example = character()
  
  # Example 1: Simple relationships running from left to right
  diagram_example[1] <- "
    graph LR;
    A-->B;
    A-->C;
    C-->E;
    B-->D;
    C-->D;
    D-->F;
    E-->F;
    "
  
  # Example 2: Simple relationships running from top to bottom
  diagram_example[2] <- "
    graph TB;
    A-->B;
    A-->C;
    C-->E;
    B-->D;
    C-->D;
    D-->F;
    E-->F;
    "
  
  # Example 3: Add some CSS styles
  diagram_example[3] <- "
    graph LR;
    A(Rounded)-->B[Squared];
    B-->C{A Decision};
    C-->D[Square One];
    C-->E[Square Two];
    
    style A fill:#E5E25F;
    style B fill:#87AB51;
    style C fill:#3C8937;
    style D fill:#23772C;
    style E fill:#B6E6E6;
    "
 
  # Example 4: Include link text
  diagram_example[4] <- "
    graph LR;
    A(Start)-->|Line Text|B(Keep Going)
    B-->|More Line Text|C(Stop);
    
    style A fill:#A2EB86, stroke:#04C4AB, stroke-width:2px;
    style B fill:#FFF289, stroke:#FCFCFF, stroke-width:2px, stroke-dasharray: 4, 4;
    style C fill:#FFA070, stroke:#FF5E5E, stroke-width:2px;
    "
  
  # Example 5: Display summary information on the 'mtcars' dataset
    # Load in the 'mtcars' dataset
    data(mtcars)
    
    # Obtain column names
    column_names <- colnames(mtcars)
    
    # Use for in loop to generate summary strings for each mtcars column
    for (i in 1:length(column_names)){
      if (i == 1) connections <- vector(mode = "character", length = 0L)
      
      connections <-
      c(connections,
        paste0(i, "(", column_names[i], ")---", i, "-stats(",
               "min: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[((i - 1) * 6) + 1]))), "<br/>",
               "1Q: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[((i - 1) * 6) + 2]))), "<br/>",
               "med: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[((i - 1) * 6) + 3]))), "<br/>",
               "mean: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[((i - 1) * 6) + 4]))), "<br/>",
               "3Q: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[((i - 1) * 6) + 5]))), "<br/>",
               "max: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[((i - 1) * 6) + 6]))),
               ")"))
    }
    
  diagram_example[5] <-
    paste0(
      "graph TD;", "\n",
      paste(connections, collapse = "\n"),"\n",
      "classDef column fill:#0001CC, stroke:#0D3FF3, stroke-width:1px;" ,"\n",
      "class ", paste0(1:length(column_names), collapse = ","), " column;
    ")
    
  html_print(tagList(
    lapply(
      1:5,
      function(num){
        tags$div(
          tags$pre(diagram_example[num])
          ,DiagrammeR(diagram_example[num], height = 400, width = 400)
        )
      }
    )
  ))
}
