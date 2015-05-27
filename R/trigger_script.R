#' Trigger a script embedded in a graph series object
#' @description Run an R script located inside or referenced from the graph series object in order to migrate the state of one or more contained graphs.
#' @param graph_series a graph series object of type \code{dgr_graph_1D}.
#' @param script the index of the script character string or path reference held in in the graph series.
#' @return a graph series object of type \code{dgr_graph_1D}.
#' @export trigger_script

trigger_script <- function(graph_series,
                           script = 1){

  # Determine whether the script index provided is within range
  is_script_number_valid <-
    ifelse(script %in% 1:length(graph_series$series_scripts),
           TRUE, FALSE)

  if (is_script_number_valid == FALSE){

    message("The script number provided doesn't correspond to an available script.")
    return(graph_series)
  }

  # Determine whether the script called is a character string or
  # a reference to a script
  is_path_provided <-
   ifelse(grepl(".*.(r|R)$", graph_series$series_scripts[script]),
          TRUE, FALSE)

  # Trigger script that is provided as a path to a file
  if (is_path_provided == TRUE){

    # Determine if the file is present
    does_file_exist <-
      ifelse(file.exists(graph_series$series_scripts[script]),
             TRUE, FALSE)

    if (does_file_exist == FALSE){

      message("The script doesn't exist at the path provided.")
      return(graph_series)
    }

    # Obtain a string corresponding to the graph_series object name
    to_substitute <- deparse(substitute(graph_series))

    # Obtain script character vector from supplied path to file
    the_script <- paste(readLines(con = graph_series$series_scripts[script]), collapse = "\n")

    # Replace instances of '_SELF_' with the name of the graph series object
    substituted_script <- gsub("_SELF_", to_substitute,
                               the_script)

    # Run the script and create a revised graph series
    evaluated_series <- eval(parse(text = substituted_script))

    return(evaluated_series)
  }

  # Trigger script that is a character object in the graph series
  if (is_path_provided == FALSE){

    # Obtain a string corresponding to the graph_series object name
    to_substitute <- deparse(substitute(graph_series))

    # Obtain script character vector from graph series
    the_script <- graph_series$series_scripts[script]

    # Replace instances of '_SELF_' with the name of the graph series object
    substituted_script <- gsub("_SELF_", to_substitute,
                               the_script)

    # Run the script and create a revised graph series
    evaluated_series <- eval(parse(text = substituted_script))

    return(evaluated_series)
  }
}
