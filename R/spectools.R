#' Razor-like template for diagram specification
#' @description Use Razor-like syntax to define a template for use in a \code{grViz} diagram.
#' @param spec string spec to be parsed and evaluated
#'
#' @examples
#' \dontrun{
#'
#' # a simple example to use a LETTER as a node label
#' spec <- "
#'   digraph { '@@1' }
#'
#'  [1]: LETTERS[1]
#' "
#' grViz(replace_in_spec(spec))
#'
#'
#' spec <- "
#' digraph a_nice_graph {
#' node [fontname = Helvetica]
#' a [label = '@@1']
#' b [label = '@@2-1']
#' c [label = '@@2-2']
#' d [label = '@@2-3']
#' e [label = '@@2-4']
#' f [label = '@@2-5']
#' g [label = '@@2-6']
#' h [label = '@@2-7']
#' i [label = '@@2-8']
#' j [label = '@@2-9']
#' a -> { b c d e f g h i j}
#' }
#'
#' [1]: 'top'
#' [2]: 10:20
#' "
#' grViz(replace_in_spec(spec))
#' }
#'
#' @export


replace_in_spec <- function(spec){

  # Directive for marking subscripted text in a label or tooltip '@_'
  if (grepl("@_", spec)){

    spec <- gsub('(label|tooltip)[ ]*=[ ]*\'(.*?)@_\\{(.*?)\\}(.*?)\'',
                 '\\1 = <\\2<FONT POINT-SIZE=\'8\'><SUB>\\3</SUB></FONT>\\4>',
                 spec, perl = TRUE)
  }

  # Directive for marking superscripted text in a label or tooltip '@_'
  if (grepl("@\\^", spec)){

    spec <- gsub('(label|tooltip)[ ]*=[ ]*\'(.*?)@\\^\\{(.*?)\\}(.*?)\'',
                 '\\1 = <\\2<FONT POINT-SIZE=\'8\'><SUP>\\3</SUP></FONT>\\4>',
                 spec, perl = TRUE)
  }

  # Make a second pass to add subscripts as inline HTML
  while (grepl('(label|tooltip)[ ]*=[ ]*<(.*?)@_\\{(.+?)\\}(.*?)>', spec)){

    spec <- gsub('(label|tooltip)[ ]*=[ ]*<(.*?)@_\\{(.*?)\\}(.*?)>',
                 '\\1 = <\\2<FONT POINT-SIZE=\'8\'><SUB>\\3</SUB></FONT>\\4>',
                 spec, perl = TRUE)
  }

  # Make a second pass to add superscripts as inline HTML
  while (grepl('(label|tooltip)[ ]*=[ ]*<(.*?)@\\^\\{(.+?)\\}(.*?)>', spec)){

    spec <- gsub('(label|tooltip)[ ]*=[ ]*<(.*?)@\\^\\{(.*?)\\}(.*?)>',
                 '\\1 = <\\2<FONT POINT-SIZE=\'8\'><SUP>\\3</SUP></FONT>\\4>',
                 spec, perl = TRUE)
  }

  # Directive for substitution of arbitrary specification text '@@'
  if (grepl("@@", spec)){

    # Extract the spec into several pieces: first being the body,
    # subsequent pieces belonging the replacement references

    spec_body <- unlist(strsplit(x = spec, "\\n\\s*\\[1\\]:"))[1]

    spec_references <- paste0("[1]:",
                              unlist(strsplit(x = spec, "\\n\\s*\\[1\\]:"))[2])

    # Split the references into a vector of R statements
    split_references <-
      gsub("\\[[0-9*]\\]:[ ]?", "",
           unlist(strsplit(x = spec_references, "\\n")))

    # Evaluate the expressions and save into a list object
    for (i in 1:length(split_references)){

      if (i == 1){
        eval_expressions <- list()
      }

      eval_expressions <- c(eval_expressions,
                            list(eval(parse(text = split_references[i]))))
    }

    # Make replacements to the spec body for each replacement that has
    # no hyphen
    for (i in 1:length(split_references)){

      while (grepl(paste0("@@", i, "([^-])"), spec_body)){

        spec_body <- gsub(paste0("@@", i),
                          eval_expressions[[i]][1], spec_body)
      }
    }

    # If the replacement has a hyphen, then obtain the digit(s) immediately
    # following and return the value from that index
    for (i in 1:length(split_references)){
      while (grepl(paste0("@@", i, "-", "[0-9]+"), spec_body)){
        the_index <-
          as.numeric(gsub("^([0-9]+)(.*)", "\\1",
                          strsplit(spec_body,
                                   paste0("@@", i, "-"))[[1]][2]))

        if (the_index > length(eval_expressions[[i]])){
          spec_body <-
            gsub(paste0("@@", i, "-", the_index, "([^0-9])"),
                 paste0(eval_expressions[[i]][length(eval_expressions[[i]])],
                        "\\1"),
                 spec_body)
        } else {
          spec_body <-
            gsub(paste0("@@", i, "-", the_index, "([^0-9])"),
                 paste0(eval_expressions[[i]][the_index], "\\1"),
                 spec_body)
        }
      }
    }

    # Return the updated spec with replacements evaluated
    return(spec_body)
  }

  if (grepl("@@", spec) == FALSE){
    return(spec)
  }

}
