#' Import a graph from various graph formats
#'
#' @description
#'
#' Import a variety of graphs from different graph formats and create a graph
#' object.
#'
#' @param graph_file A connection to a graph file. When provided as a path to a
#'   file, it will read the file from disk. Files starting with `http://`,
#'   `https://`, `ftp://`, or `ftps://` will be automatically downloaded.
#' @param file_type The type of file to be imported. Options are: `gml` (GML),
#'   `sif` (SIF), `edges` (a .edges file), and `mtx` (MatrixMarket format). If
#'   not supplied, the type of graph file will be inferred by its file
#'   extension.
#' @param edges_extra_attr_names For `edges` files, a vector of attribute names
#'   beyond the `from` and `to` data columns can be provided in the order they
#'   appear in the input data file.
#' @param edges_extra_attr_coltypes For `edges` files, this is a string of
#'   column types for any attribute columns provided for
#'   `edges_extra_attr_names`. This string representation is where each
#'   character represents each of the extra columns of data and the mappings
#'   are: `c` -> character, `i` -> integer, `n` -> number, `d` -> double, `l` ->
#'   logical, `D` -> date, `T` -> date time, `t` -> time, `?` -> guess, or
#'   `_/-`, which skips the column.
#' @inheritParams create_graph
#'
#' @return A graph object of class `dgr_graph`.
#'
#' @examples
#' \dontrun{
#' # Import a GML graph file
#' gml_graph <-
#'   import_graph(
#'     system.file(
#'       "extdata/karate.gml",
#'       package = "DiagrammeR"))
#'
#' # Get a count of the graph's nodes
#' gml_graph %>%
#'   count_nodes()
#'
#' # Get a count of the graph's edges
#' gml_graph %>%
#'   count_edges()
#' }
#'
#' @export
import_graph <- function(
    graph_file,
    file_type = NULL,
    edges_extra_attr_names = NULL,
    edges_extra_attr_coltypes = NULL,
    graph_name = NULL,
    attr_theme = "default",
    write_backups = FALSE,
    display_msgs = FALSE
) {

  # Get the time of function start
  time_function_start <- Sys.time()

  # Get the name of the function
  fcn_name <- get_calling_fcn()

  # Stop function if `file_type` specified is not part
  # of the group that can be imported
  if (!is.null(file_type)) {
    if (!(tolower(file_type) %in%
          c("gml", "sif", "edges", "mtx"))) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "The file type as specified cannot be imported")
    }
  }

  # Stop function if file doesn't exist
  if (!grepl("(^http:|^https:|^ftp:|^ftp:)", graph_file)) {
    if (!file.exists(graph_file)) {

      emit_error(
        fcn_name = fcn_name,
        reasons = "The file as specified doesn't exist")
    }
  }

  if (grepl("(^http:|^https:|^ftp:|^ftp:)", graph_file)) {

    dest_file <-
      unlist(strsplit(graph_file, "/"))[
        length(unlist(strsplit(graph_file, "/")))]

    # Download the file
    curl::curl_download(graph_file, destfile = dest_file)

    # Extract the file and get the filename of the extracted file
    if (gsub(".*\\.([a-zA-Z]*?)", "\\1", graph_file) == "zip") {

      # Extract from the .zip archive
      utils::unzip(zipfile = dest_file)

      # Get the file name
      base_name <- (strsplit(dest_file, split = "\\.") %>% unlist())[[1]]

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
    if (file_extension == "gml") {
      file_type <- "gml"
    } else if (file_extension == "sif") {
      file_type <- "sif"
    } else if (file_extension == "edges") {
      file_type <- "edges"
    } else if (file_extension == "mtx") {
      file_type <- "mtx"
    } else {

      emit_error(
        fcn_name = fcn_name,
        reasons = "The file type is not known so it can't be imported")
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
        progress = FALSE)

    n_rows <- nrow(edges)

    edges <-
      edges %>%
      dplyr::mutate(id = seq_len(n_rows)) %>%
      dplyr::mutate(rel = NA_character_) %>%
      dplyr::relocate(id, from, to, rel) %>%
      as.data.frame(stringsAsFactors = FALSE)

    # Create a node data frame
    nodes <-
      dplyr::bind_rows(
        dplyr::tibble(
          id = edges %>%
            dplyr::as_tibble() %>%
            dplyr::select("from") %>%
            purrr::flatten_int()),
        dplyr::tibble(
          id = edges %>%
            dplyr::as_tibble() %>%
            dplyr::select("to") %>%
            purrr::flatten_int())) %>%
      dplyr::distinct() %>%
      dplyr::arrange(id) %>%
      dplyr::mutate(type = NA_character_,
                    label = as.character(id)) %>%
      as.data.frame(stringsAsFactors = FALSE)

    # Create the graph
    the_graph <-
      create_graph(
        nodes_df = nodes,
        edges_df = edges,
        graph_name = graph_name,
        write_backups = write_backups,
        display_msgs = display_msgs
      )

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
      dplyr::tibble(
        id = as.integer(unique(
          unlist(
            strsplit(
              mtx_document[first_line:length(mtx_document)],
              " ")))),
        type = NA_character_,
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
        edges_df = edges,
        graph_name = graph_name,
        write_backups = write_backups,
        display_msgs = display_msgs
      )

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
        stringr::str_replace_all(
          stringr::str_extract_all(gml_document,
                          "directed [0-1]"),
          "directed ", ""))

    # Extract all node definitions
    node_defs <-
      unlist(
        stringr::str_extract_all(gml_document,
                        "node[ ]*?\\[.*?\\]"))

    # Get all node ID values
    node_id <-
      as.integer(
        stringr::str_replace_all(
          stringr::str_extract_all(
            node_defs,
            "id [a-z0-9_]*"),
          "id ", ""))

    # Get all node label values, if they exist
    if (any(stringr::str_detect(node_defs, "label"))) {
      node_label <-
        stringr::str_replace_all(
          stringr::str_replace_all(
            stringr::str_extract_all(
              node_defs,
              "label \\\".*?\\\""),
            "label \"", ""),
          "\"", "")
    }

    # Extract all edge definitions
    edge_defs <-
      unlist(stringr::str_extract_all(
        gml_document,
        "edge[ ]*?\\[.*?\\]"))

    edges_from <-
      as.integer(
        stringr::str_replace_all(
          stringr::str_extract_all(
            edge_defs,
            "source [a-z0-9_]*"),
          "source ", ""))

    edges_to <-
      as.integer(
        stringr::str_replace_all(
          stringr::str_extract_all(
            edge_defs,
            "target [a-z0-9_]*"),
          "target ", ""))


    if (any(stringr::str_detect(edge_defs, "label"))) {
      edge_label <-
        stringr::str_replace_all(
          stringr::str_replace_all(
            stringr::str_extract_all(
              edge_defs,
              "label \\\".*?\\\""),
            "label \"", ""),
          "\"", "")
    }

    if (any(stringr::str_detect(edge_defs, "value"))) {
      edge_value <-
        stringr::str_remove_all(
          stringr::str_extract_all(
            edge_defs,
            "value [a-z0-9\\.]*"),
          "value ")
    }

    # Create all nodes for graph
    all_nodes <-
      dplyr::tibble(
        id = node_id,
        type = NA_character_,
        label = NA_character_) %>%
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
        directed = graph_directed == "1",
        graph_name = graph_name,
        write_backups = write_backups,
        display_msgs = display_msgs
      )

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
              unlist(stringr::str_split(sif_document[i], "\t"))) == 1,
            unlist(stringr::str_split(sif_document[i], "\t"))[1],
            unlist(stringr::str_split(sif_document[i], "\t"))[-2]))
    }

    # Obtain a unique vector of nodes in the graph
    nodes <- unique(nodes)

    # Create a node data frame
    ndf <-
      create_node_df(
        n = length(nodes),
        label = nodes)

    # Determine which lines have single nodes
    if (!all(stringr::str_detect(sif_document, "\\t"))) {
      single_nodes <- which(!stringr::str_detect(sif_document, "\\t"))
    }

    # Initialize vectors for an edge data frame
    from <- to <- vector(mode = "integer")
    rel <- vector(mode = "character")

    # Obtain complete vectors for the edge data frame
    for (i in which(stringr::str_count(sif_document, "\\t") > 1)) {
      length_stmt <- length(stringr::str_split(sif_document[i], "\t")[[1]])
      from <- c(from, stringr::str_split(sif_document[i], "\t")[[1]][1])
      rel <- c(rel, stringr::str_split(sif_document[i], "\t")[[1]][2])
      to <- c(to, stringr::str_split(sif_document[i], "\t")[[1]][3:length_stmt])
    }

    # Create an edge data frame
    edf <-
      dplyr::tibble(
        from_label = from,
        to_label = to,
        rel = rel) %>%
      dplyr::right_join(ndf, c("from_label" = "label")) %>%
      dplyr::select(from = "id", "to_label", "rel") %>%
      dplyr::right_join(ndf, c("to_label" = "label")) %>%
      dplyr::select("from", to = "id", "rel") %>%
      as.data.frame(stringsAsFactors = FALSE)

    # Create a graph object
    the_graph <-
      create_graph(
        nodes_df = ndf,
        edges_df = edf,
        graph_name = graph_name,
        write_backups = write_backups,
        display_msgs = display_msgs
      )

    # Return the graph
    return(the_graph)
  }
}
