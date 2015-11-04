#' Trigger a script embedded in a graph series object
#' @description Run an R script located inside or referenced from the graph
#' series object in order to migrate the state of one or more contained graphs.
#' @param graph_series a graph series object of type \code{dgr_graph_1D}.
#' @param script the index of the script character string or path reference
#' held in in the graph series.
#' @return a graph series object of type \code{dgr_graph_1D}.
#' @examples
#' \dontrun{
#' # So, here's a script that essentially takes an empty graph series, and
#' # creates a new graph on each new day it is triggered. It will create
#' # random nodes each time it's triggered and add those nodes to the graph
#' # belonging to the current day. Throughout the script, '_SELF_' refers
#' # to the graph series in which the script is contained.
#' sample_node_script <-
#' '
#' graph_attrs <-
#'   c("layout = twopi",
#'     "overlap = FALSE",
#'     "outputorder = edgesfirst")
#'
#' node_attrs <-
#'   c("shape = circle",
#'     "fixedsize = TRUE",
#'     "width = 1",
#'     "penwidth = 1",
#'     "color = DodgerBlue",
#'     "style = filled",
#'     "fillcolor = Aqua",
#'     "alpha_fillcolor = 0.5",
#'     "fontname = Helvetica",
#'     "fontcolor = Grey25")
#'
#' edge_attrs <-
#'   c("arrowhead = dot",
#'     "minlen = 1.5",
#'     "color = Green",
#'     "penwidth = 2")
#'
#' # If there is no graph available in the series, then, make one!
#' if (graph_count(graph_series = _SELF_) == 0){
#'
#' _SELF_ <-
#'   add_to_series(graph = create_graph(graph_attrs = graph_attrs,
#'                 node_attrs = node_attrs,
#'                 edge_attrs = edge_attrs,
#'                 graph_name = paste0("data_", Sys.Date()),
#'                 graph_time = as.character(Sys.Date()),
#'                 graph_tz = Sys.timezone()),
#'                 graph_series = _SELF_)
#'
#' }
#'
#' # Determine the index of the last graph in the series
#' last_graph_in_series <- graph_count(graph_series = _SELF_)
#'
#' # If it is a new day, create a new graph in the series to populate with data
#' if (Sys.Date() > as.Date(_SELF_$graphs[[last_graph_in_series]]$graph_time,
#'                          tz = _SELF_$graphs[[last_graph_in_series]]$graph_tz)){
#'
#' _SELF_ <-
#'   add_to_series(graph = create_graph(graph_attrs = graph_attrs,
#'                 node_attrs = node_attrs,
#'                 edge_attrs = edge_attrs,
#'                 graph_name = paste0("data_", Sys.Date()),
#'                 graph_time = as.character(Sys.Date()),
#'                 graph_tz = Sys.timezone()),
#'                 graph_series = _SELF_)
#'
#' last_graph_in_series <-
#'   graph_count(graph_series = _SELF_)
#' }
#'
#' # Create a node to place into the graph
#' letters <- paste(sample(LETTERS, 5), collapse = "")
#'
#' # Add node to the most recent graph and attach it to
#' # another randomly picked node available in the graph.
#' # Note that adding an edge only works in the case that
#' # there is at least one node available in the graph.
#' # For convenience, the relevant graph is extracted from
#' # the series, then placed back in the series.
#' if (!is.na(sample(get_nodes(_SELF_$graphs[[last_graph_in_series]]), 1))){
#'
#' graph <- _SELF_$graphs[[last_graph_in_series]]
#'
#' graph <- add_node(graph = graph,
#'                   node = letters)
#'
#' graph <- add_edge(graph = graph,
#'                   from = letters,
#'                   to = sample(get_nodes(graph = graph), 1))
#'
#' } else {
#'
#' graph <- _SELF_$graphs[[last_graph_in_series]]
#'
#' graph <- add_node(graph = graph,
#'                   node = letters)
#' }
#'
#' # Remove old graph from series
#' _SELF_ <- remove_from_series(graph_series = _SELF_,
#'                              index = "last")
#'
#' # Add new graph to correct position in series
#' # The "add_to_series" function always adds a graph to the
#' # end of the graph series.
#' _SELF_ <- add_to_series(graph = graph,
#'                         graph_series = _SELF_)
#'
#' return(_SELF_)
#' '
#'
#' # Create an empty graph series of the 'temporal' type and add
#' # that script as one of the graph series' 'series scripts'
#' series_temporal <- create_series(series_type = "temporal",
#'                                  series_scripts = sample_node_script)
#'
#' # Call the function 60 times, this will generate 60 random nodes
#' # with 59 edges
#' for (i in seq(1, 60)){
#'
#'   series_temporal <-
#'     trigger_script(graph_series = series_temporal,
#'                    script = 1)
#'   if (i == 60) break
#' }
#'
#' # Display the results in the RStudio Viewer
#' render_graph_from_series(graph_series = series_temporal,
#'                          graph_no = graph_count(series_temporal))
#'
#' # Get some basic information about the graphs in the graph series object
#' series_info(series_temporal)
#'
#' # Write the script to a file
#' cat(sample_node_script, file = "~/Desktop/sample_node_script.R")
#'
#' # Create a reference to the file instead of including text directly
#' # in the 'series_temporal' object
#' series_temporal <-
#'   create_series(series_type = "temporal",
#'                 series_scripts = "~/Desktop/sample_node_script.R")
#'
#' # Call the function 60 times, this will generate 60 random nodes
#' # with 59 edges
#' for (i in seq(1, 60)){
#'
#'   series_temporal <-
#'     trigger_script(graph_series = series_temporal,
#'                    script = 1)
#'   if (i == 60) break
#' }
#'
#' # Display the results in the RStudio Viewer
#' render_graph_from_series(graph_series = series_temporal,
#'                          graph_no = graph_count(series_temporal))
#' }
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
