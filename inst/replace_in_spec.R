digraph <-"
digraph agraph {
node [fontname = Helvetica]
a [shape = polygon, sides = 5, peripheries = @@2, color = lightblue, style = filled, fontcolor = white]
a -> '@@1'
'@@1' -> b
b -> c [headport=e, tailport = w]
b -> 'number: @@2' [color = blue, headport = s, tailport = n]
c -> '@@1'
d -> '@@2'
d -> c [tooltip = hello]
f -> d
d -> e
'@@1' -> c
a -> f
}

@@1: mean(c(
4, 7, 9, 1, 3, 9))
@@2: 2:5
"

replace_in_spec <- function(spec) {
  
  if (grepl("@@", spec)){
    
    # Extract the spec into several pieces: first being the body, 
    # subsequent pieces belonging the replacement references
    spec_body <- unlist(str_split(string = spec, "\\n@@1"))[1]
    
    spec_references <- paste0("\\n@@1", unlist(str_split(string = spec, "\\n@@1"))[2])
    
    
    # Split the references into a vector of R statements
    split_references <- 
    gsub("\\n", "",
         gsub("^ ", "",
              gsub("\\\\n@@1:", "",
                   unlist(str_split(string = spec_references, "\\n@@[0-9*]:")))))
    
    # Evaluate the expressions and save into a list object
    for (i in 1:length(split_references)){
      
      if (i == 1){
        eval_expressions <- list()
      }
      
      eval_expressions <- c(eval_expressions,
                            list(eval(parse(text = split_references[i]))))
    }
    
    # Make replacements to the spec body
    for (i in 1:length(split_references)){
        
      spec_body <- gsub(paste0("@@", i), eval_expressions[[i]][1], spec_body)
      
    }
    
    
    # Return the updated spec with replacements evaluated  
    return(spec_body)
  }
  
}


spec <- replace_in_spec(digraph)

grViz(spec)

