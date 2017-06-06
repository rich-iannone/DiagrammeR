#' Import a graph from various graph formats
#' @description Import a variety of graphs from
#' different graph formats and create a graph object.
#' @param graph_file a connection to a graph file.
#' When provided as a path to a file, it will read the
#' file from disk. Files starting with \code{http://},
#' \code{https://}, \code{ftp://}, or \code{ftps://}
#' will be automatically downloaded.
#' @param file_type the type of file to be imported.
#' Options are: \code{graphml} (GraphML), \code{gml}
#' (GML), \code{sif} (SIF), \code{edges} (a .edges
#' file), and \code{mtx} (MatrixMarket format). If not
#' supplied, the type of graph file will be inferred by
#' its file extension.
#' @param edges_extra_attr_names for \code{edges} files,
#' a vector of attribute names beyond the \code{from}
#' and \code{to} data columns can be provided in the
#' order they appear in the input data file.
#' @param edges_extra_attr_coltypes for \code{edges}
#' files, this is a string of column types for any
#' attribute columns provided for
#' \code{edges_extra_attr_names}. This string
#' representation is where each character represents
#' each of the extra columns of data and the mappings
#' are: \code{c} -> character, \code{i} -> integer,
#' \code{n} -> number, \code{d} -> double,
#' \code{l} -> logical, \code{D} -> date, \code{T} ->
#' date time, \code{t} -> time, \code{?} -> guess,
#' or \code{_/-}, which skips the column.
#' @return a graph object of class \code{dgr_graph}.
#' @examples
#' \dontrun{
#' # Import a GraphML graph file
#' graphml_graph <-
#'   import_graph(
#'     system.file(
#'       "extdata/power_grid.graphml",
#'       package = "DiagrammeR"))
#'
#' # Get a count of the graph's nodes
#' graphml_graph %>%
#'   node_count()
#' #> [1] 4941
#'
#' # Get a count of the graph's edges
#' graphml_graph %>%
#'   edge_count()
#' #> [1] 6594
#'
#' # Import an SIF graph file
#' sif_graph <-
#'   import_graph(
#'     system.file(
#'       "extdata/Human_Interactome.sif",
#'       package = "DiagrammeR"))
#'
#' # Get a count of the graph's nodes
#' sif_graph %>%
#'   node_count()
#' #> [1] 8347
#'
#' # Get a count of the graph's edges
#' sif_graph %>%
#'   edge_count()
#' #> [1] 61263
#'
#' # Import a GML graph file
#' gml_graph <-
#'   import_graph(
#'     system.file(
#'       "extdata/karate.gml",
#'       package = "DiagrammeR"))
#'
#' # Get a count of the graph's nodes
#' gml_graph %>%
#'   node_count()
#' #> [1] 34
#'
#' # Get a count of the graph's edges
#' gml_graph %>%
#'   edge_count()
#' #> [1] 78
#' }
#' @importFrom dplyr right_join select rename mutate everything bind_rows arrange distinct
#' @importFrom downloader download
#' @importFrom purrr flatten_int
#' @importFrom stringr str_extract str_detect str_split str_count
#' str_replace_all str_extract_all
#' @importFrom tibble tibble as_tibble
#' @importFrom readr read_delim
#' @importFrom utils unzip
#' @export import_graph

import_graph <- function(graph_file,
                         file_type = NULL,
                         edges_extra_attr_names = NULL,
                         edges_extra_attr_coltypes = NULL) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Assign NULL to several objects
  id <- to_label <- from_label <-  NULL

  # Stop function if `file_type` specified is not part
  # of the group that can be imported
  if (!is.null(file_type)) {
    if (!(tolower(file_type) %in%
          c("graphml", "gml", "sif", "edges", "mtx"))) {
      stop("The file type as specified cannot be imported.")
    }
  }

  # Stop function if file doesn't exist
  if (grepl("(^http:|^https:|^ftp:|^ftp:)", graph_file) == FALSE) {
    if (file.exists(graph_file) == FALSE) {
      stop("The file as specified doesn't exist.")
    }
  }

  if (grepl("(^http:|^https:|^ftp:|^ftp:)", graph_file)) {

    dest_file <-
      unlist(strsplit(graph_file, "/"))[
        length(unlist(strsplit(graph_file, "/")))]

    # Download the file
    downloader::download(graph_file, destfile = dest_file)

    # Extract the file and get the filename of the extracted file
    if (gsub(".*\\.([a-zA-Z]*?)", "\\1", graph_file) == "zip") {

      # Extract from the .zip archive
      utils::unzip(zipfile = dest_file)

      # Get the file name
      base_name <- strsplit(dest_file, split = "\\.") %>% unlist() %>% .[[1]]

      graph_file <-
        list.files(pattern = paste0(base_name, ".*"))[
        !grepl(".*\\.zip$", list.files(pattern = paste0(base_name, ".*")))][1]
    }
  }

  # Obtain file extension if no value supplied
  # for `file_type`
  if (is.null(file_type)) {
    file_extension <- gsub(".*\\.([a-zA-Z]*?)", "\\1", graph_file)

    # Determine file type from file extension
    if (file_extension == "graphml") {
      file_type <- "graphml"
    } else if (file_extension == "gml") {
      file_type <- "gml"
    } else if (file_extension == "sif") {
      file_type <- "sif"
    } else if (file_extension == "edges") {
      file_type <- "edges"
    } else if (file_extension == "mtx") {
      file_type <- "mtx"
    } else {
      stop("The file type is not known so it can't be imported.")
    }
  }

  if (file_type == "edges") {

    # Read in the .edges document as a vector object
    edges_document <- readLines(graph_file, 10)

    # Determine which line the data fields begin
    first_line <- grep("^[^%].*", edges_document)[1]

    # Determine the number of lines to skip
    lines_to_skip <- first_line - 1

    # Set default attribute names and column types
    # for an `edges` file
    attr_names <- c("from", "to")
    attr_coltypes <- c("i", "i")

    if (!is.null(edges_extra_attr_names)) {
      attr_names <- c(attr_names, edges_extra_attr_names)
    }

    if (!is.null(edges_extra_attr_coltypes)) {
      attr_coltypes <- paste(c(attr_coltypes, edges_extra_attr_coltypes), collapse = "")
    }

    # Create an edge data frame
    edges <-
      readr::read_delim(
        file = graph_file,
        delim = " ",
        skip = lines_to_skip,
        col_names = attr_names,
        col_types = attr_coltypes,
        progress = FALSE) %>%
      dplyr::mutate(id = 1:nrow(.)) %>%
      dplyr::mutate(rel = as.character(NA)) %>%
      dplyr::select(id, from, to, rel, dplyr::everything()) %>%
      as.data.frame(stringsAsFactors = FALSE)

    # Create a node data frame
    nodes <-
      dplyr::bind_rows(
        tibble::tibble(
          id = edges %>%
            tibble::as_tibble() %>%
            dplyr::select(from) %>%
            purrr::flatten_int()),
        tibble::tibble(
          id = edges %>%
            tibble::as_tibble() %>%
            dplyr::select(to) %>%
            purrr::flatten_int())) %>%
      dplyr::distinct() %>%
      dplyr::arrange(id) %>%
      dplyr::mutate(type = as.character(NA)) %>%
      dplyr::mutate(label = as.character(id)) %>%
      as.data.frame(stringsAsFactors = FALSE)

    # Create the graph
    the_graph <-
      create_graph(
        nodes_df = nodes,
        edges_df = edges)

    # Return the graph
    return(the_graph)
  }

  if (file_type == "mtx") {

    # Read in the .mtx document as a vector object
    mtx_document <- readLines(graph_file)

    # Determine which line the data fields begin
    first_line <- grep("^(\\w*) (\\w*)$", mtx_document)[1]

    # Create an edge data frame
    edges <-
      create_edge_df(
        from = sapply(
          strsplit(
            mtx_document[first_line:length(mtx_document)],
            " "), "[[", 1),
        to = sapply(
          strsplit(
            mtx_document[first_line:length(mtx_document)],
            " "), "[[", 2))

    # Create a node data frame
    nodes <-
      tibble::tibble(
        id = as.integer(unique(
          unlist(
            strsplit(
              mtx_document[first_line:length(mtx_document)],
              " ")))),
        type = as.character(NA),
        label = as.integer(unique(
          unlist(
            strsplit(
              mtx_document[first_line:length(mtx_document)],
              " "))))) %>%
      as.data.frame(stringsAsFactors = FALSE)

    # Create the graph
    the_graph <-
      create_graph(
        nodes_df = nodes,
        edges_df = edges)

    # Return the graph
    return(the_graph)
  }

  if (file_type == "graphml") {

    # Read in the .graphml document as a vector object
    graphml_document <- readLines(graph_file)

    # Determine the starting and ending indices of
    # the <node> tags
    xml_nodes <-
      list(node_start = grep("<node ", graphml_document),
           node_end = grep("</node>", graphml_document))

    # Determine the starting and ending indices of the
    # <edge> tags
    xml_edges <-
      list(edge_start = grep("<edge ", graphml_document),
           edge_end = grep("</edge>", graphml_document))

    # Determine all node ID values for the graph
    for (i in 1:length(xml_nodes[[1]])) {

      if (i == 1) nodes_ids <- vector(mode = "character")

      nodes_ids <-
        c(nodes_ids,
          str_replace_all(
            str_extract(graphml_document[xml_nodes[[1]][i]],
                        "\".*?\""), "\"", ""))
    }

    # Determine indices that contain first
    # node attributes
    node_key_indices <-
      xml_nodes[[1]][1] - 1 +
      grep("key",
           graphml_document[xml_nodes[[1]][1]:xml_nodes[[2]][1]])

    # Obtain names of keys
    node_key_names <-
      gsub(".*?\"(.*?)\".*", "\\1",
           graphml_document[node_key_indices])

    # Obtain list of vectors for all node attributes
    node_attributes <- list()

    for (i in 1:length(node_key_names)) {
      for (j in 1:length(xml_nodes[[1]])) {
        if (j == 1) {
          attribute <- vector(mode = "character")
        }

        attribute <-
          c(attribute,
            gsub(".*?>(.*?)<.*", "\\1",
                 graphml_document[xml_nodes[[1]][j] + i]))

        if (j == length(xml_nodes[[1]])) {
          node_attributes[[i]] <-  attribute
        }
      }

      if (i == length(node_key_names)) {
        names(node_attributes) <- node_key_names
      }
    }

    # Create all nodes for graph
    all_nodes <-
      cbind(
        tibble::tibble(
          id = as.integer(nodes_ids),
          type = as.character(NA)),
        data.frame(node_attributes))

    # Determine all edge values for the graph
    for (i in 1:length(xml_edges[[1]])) {

      if (i == 1) {
        edges_from <- vector(mode = "character")
        edges_to <- vector(mode = "character")
      }

      edges_from <-
        c(edges_from,
          str_replace_all(
            unlist(str_extract_all(
              graphml_document[xml_edges[[1]][i]],
              "\".*?\""))[1], "\"", ""))

      edges_to <-
        c(edges_to,
          str_replace_all(
            unlist(str_extract_all(
              graphml_document[xml_edges[[1]][i]],
              "\".*?\""))[2], "\"", ""))
    }

    # Create all edges for graph
    all_edges <-
      create_edge_df(
        from = edges_from,
        to = edges_to)

    # Create the graph
    the_graph <-
      create_graph(
        nodes_df = all_nodes,
        edges_df = all_edges)

    # Return the graph
    return(the_graph)
  }

  if (file_type == "gml") {

    # Read in the .gml document as a vector object
    gml_document <-
      paste(readLines(graph_file), collapse = "")

    # Extract information on whether graph is directed
    graph_directed <-
      unlist(
        str_replace_all(
          str_extract_all(gml_document,
                          "directed [0-1]"),
          "directed ", ""))

    # Extract all node definitions
    node_defs <-
      unlist(
        str_extract_all(gml_document,
                        "node[ ]*?\\[.*?\\]"))

    # Get all node ID values
    node_id <-
      as.integer(
        str_replace_all(
          str_extract_all(
            node_defs,
            "id [a-z0-9_]*"),
          "id ", ""))

    # Get all node label values, if they exist
    if (any(str_detect(node_defs, "label"))) {
      node_label <-
        str_replace_all(
          str_replace_all(
            str_extract_all(
              node_defs,
              "label \\\".*?\\\""),
            "label \"", ""),
          "\"", "")
    }

    # Extract all edge definitions
    edge_defs <-
      unlist(str_extract_all(
        gml_document,
        "edge[ ]*?\\[.*?\\]"))

    edges_from <-
      as.integer(
        str_replace_all(
          str_extract_all(
            edge_defs,
            "source [a-z0-9_]*"),
          "source ", ""))

    edges_to <-
      as.integer(
        str_replace_all(
          str_extract_all(
            edge_defs,
            "target [a-z0-9_]*"),
          "target ", ""))


    if (any(str_detect(edge_defs, "label"))) {
      edge_label <-
        str_replace_all(
          str_replace_all(
            str_extract_all(
              edge_defs,
              "label \\\".*?\\\""),
            "label \"", ""),
          "\"", "")
    }

    if (any(str_detect(edge_defs, "value"))) {
      edge_value <-
        str_replace_all(
          str_extract_all(
            edge_defs,
            "value [a-z0-9\\.]*"),
          "value ", "")
    }

    # Create all nodes for graph
    all_nodes <-
      tibble::tibble(
        id = node_id,
        type = as.character(NA),
        label = as.character(NA)) %>%
      as.data.frame(stringsAsFactors = FALSE)

    if (exists("node_label")) {
      all_nodes$label <- node_label
    }

    # Create all edges for graph
    all_edges <-
      create_edge_df(
        from = edges_from,
        to = edges_to)

    if (exists("edge_value")) {
      all_edges$data_value <- edge_value
    }

    # Create the graph
    the_graph <-
      create_graph(
        nodes_df = all_nodes,
        edges_df = all_edges,
        directed = ifelse(graph_directed == "1",
                          TRUE, FALSE))

    # Return the graph
    return(the_graph)
  }

  if (file_type == "sif") {

    # Read in the SIF document as a vector object
    sif_document <- readLines(graph_file)

    # Initialize the vector for a node data frame
    nodes <- vector(mode = "character")

    # Determine which nodes are present in the graph
    for (i in 1:length(sif_document)) {
      nodes <-
        c(nodes,
          ifelse(
            length(
              unlist(str_split(sif_document[i], "\t"))) == 1,
            unlist(str_split(sif_document[i], "\t"))[1],
            unlist(str_split(sif_document[i], "\t"))[-2]))
    }

    # Obtain a unique vector of nodes in the graph
    nodes <- unique(nodes)

    # Create a node data frame
    ndf <-
      create_node_df(
        n = length(nodes),
        label = nodes)

    # Determine which lines have single nodes
    if (any(!str_detect(sif_document, "\\t"))) {
      single_nodes <- which(!str_detect(sif_document, "\\t"))
    }

    # Initialize vectors for an edge data frame
    from <- to <- vector(mode = "integer")
    rel <- vector(mode = "character")

    # Obtain complete vectors for the edge data frame
    for (i in which(str_count(sif_document, "\\t") > 1)) {
      length_stmt <- length(str_split(sif_document[i], "\t")[[1]])
      from <- c(from, str_split(sif_document[i], "\t")[[1]][1])
      rel <- c(rel, str_split(sif_document[i], "\t")[[1]][2])
      to <- c(to, str_split(sif_document[i], "\t")[[1]][3:length_stmt])
    }

    # Create an edge data frame
    edf <-
      tibble::tibble(
        from_label = from,
        to_label = to,
        rel = rel) %>%
      dplyr::right_join(ndf, c("from_label" = "label")) %>%
      dplyr::select(id, to_label, rel) %>%
      dplyr::rename(from = id) %>%
      dplyr::right_join(ndf, c("to_label" = "label")) %>%
      dplyr::select(from, id, rel) %>%
      dplyr::rename(to = id) %>%
      as.data.frame(stringsAsFactors = FALSE)

    # Create a graph object
    the_graph <-
      create_graph(
        nodes_df = ndf,
        edges_df = edf)

    # Return the graph
    return(the_graph)
  }
}
