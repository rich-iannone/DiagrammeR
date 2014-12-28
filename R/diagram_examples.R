#' Several examples of diagrams that can be created
#' @description This function provides several examples for diagramming code and output
#' @export diagram_examples

diagram_examples <- function(example_number){
  
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
  
  render_diagram(diagram_example)
  
}