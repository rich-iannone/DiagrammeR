#' Several examples of diagrams that can be created
#' @description This function provides several examples for diagramming code and output
#' @export diagram_examples

diagram_examples <- function(example_number){
  
  # Validate the value provided for 'example_number'
  stopifnot(example_number %in% seq(1:5))
  
  # Example 1: Simple relationships running from left to right
  if (example_number == 1){
    
    diagram_example <- "
    graph LR;
    A-->B;
    A-->C;
    C-->E;
    B-->D;
    C-->D;
    D-->F;
    E-->F;
    "
  }
  
  # Example 2: Simple relationships running from top to bottom
  if (example_number == 2){
    
    diagram_example <- "
    graph TB;
    A-->B;
    A-->C;
    C-->E;
    B-->D;
    C-->D;
    D-->F;
    E-->F;
    "
  }
  
  # Example 3: Add some CSS styles
  if (example_number == 3){
  
    diagram_example <- "
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
  }
 
  # Example 4: Include link text
  if (example_number == 4){
    
    diagram_example <- "
    graph LR;
    A(Start)-->|Line Text|B(Keep Going)
    B-->|More Line Text|C(Stop);
    
    style A fill:#A2EB86, stroke:#04C4AB, stroke-width:2px;
    style B fill:#FFF289, stroke:#FCFCFF, stroke-width:2px, stroke-dasharray: 4, 4;
    style C fill:#FFA070, stroke:#FF5E5E, stroke-width:2px;
    "
  }
  
  # Example 5: Display summary information on the 'mtcars' dataset
  if (example_number == 5){
    
    # Load in the 'mtcars' dataset
    data(mtcars)
    
    # Obtain column names
    column_names <- colnames(mtcars)
    
    # Use for in loop to generate summary strings for each mtcars column
    for (i in 1:length(column_names)){
      if (i == 1) stat_strings <- vector(mode = "character", length = 0L)
      
      stat_strings <-
      c(stat_strings,
        paste0(i, "(", column_names[i], ")---", i, "-stats(",
               "min: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[(i - 1) + 1]))), "<br/>",
               "1Q: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[(i - 1) + 2]))), "<br/>",
               "med: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[(i - 1) + 3]))), "<br/>",
               "mean: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[(i - 1) + 4]))), "<br/>",
               "3Q: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[(i - 1) + 5]))), "<br/>",
               "max: ", gsub(" ", "", (gsub(".*:(.*)", "\\1",summary(mtcars)[(i - 1) + 6]))),
               ")"))
    }
    
    diagram_example <-
    paste0(
      "graph TD;", "\n",
      paste(stat_strings, collapse = "\n"),"\n",
      "classDef column fill:#0001CC, stroke:#0D3FF3, stroke-width:1px;" ,"\n",
      "class ", paste0(1:length(column_names), collapse = ","), " column;
    ")
        
  }  
  
  render_diagram(diagram_example)
  
}
