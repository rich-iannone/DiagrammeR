#' Get paths from a specified node in a directed graph
#'
#' @description
#'
#' Obtain a list of all possible paths from a given node within a directed
#' graph.
#'
#' @inheritParams render_graph
#' @param from The node from which all paths will be determined.
#' @param to The node to which all paths will be determined.
#' @param shortest_path An option to return paths that are the shortest in the
#'   set of all determined paths.
#' @param longest_path An option to return paths that are the longest in the set
#'   of all determined paths.
#' @param distance A vector of integer values that specify which of the valid
#'   paths to return when filtering by distance.
#'
#' @return A list of paths, sorted by ascending traversal length, comprising
#'   vectors of node IDs in sequence of traversal through the graph.
#'
#' @examples
#' # Create a simple graph
#' graph <-
#'   create_graph() %>%
#'   add_n_nodes(n = 8) %>%
#'   add_edge(from = 1, to = 2) %>%
#'   add_edge(from = 1, to = 3) %>%
#'   add_edge(from = 3, to = 4) %>%
#'   add_edge(from = 3, to = 5) %>%
#'   add_edge(from = 4, to = 6) %>%
#'   add_edge(from = 2, to = 7) %>%
#'   add_edge(from = 7, to = 5) %>%
#'   add_edge(from = 4, to = 8)
#'
#' # Get a list of all paths outward from node `1`
#' graph %>%
#'   get_paths(from = 1)
#'
#' # Get a list of all paths leading to node `6`
#' graph %>%
#'   get_paths(to = 6)
#'
#' # Get a list of all paths from `1` to `5`
#' graph %>%
#'   get_paths(
#'    from = 1,
#'    to = 5)
#'
#' # Get a list of all paths from `1` up to a distance
#' # of 2 node traversals
#' graph %>%
#'   get_paths(
#'     from = 1,
#'     distance = 2)
#'
#' # Get a list of the shortest paths from `1` to `5`
#' get_paths(
#'   graph,
#'   from = 1,
#'   to = 5,
#'   shortest_path = TRUE)
#'
#' # Get a list of the longest paths from `1` to `5`
#' get_paths(
#'   graph,
#'   from = 1,
#'   to = 5,
#'   longest_path = TRUE)
#'
#' @export
get_paths <- function(
    graph,
    from = NULL,
    to = NULL,
    shortest_path = FALSE,
    longest_path = FALSE,
    distance = NULL
) {

  # Validation: Graph object is valid
  check_graph_valid(graph)

  reverse_paths <- FALSE

  if (is.null(from) && !is.null(to)) {

    from_switch <- graph$edges_df$from
    to_switch <- graph$edges_df$to

    graph$edges_df$from <- to_switch
    graph$edges_df$to <- from_switch

    from <- to
    to <- NULL

    reverse_paths <- TRUE
  }

  for (m in seq_along(from)) {
    if (m == 1) all_paths <- list()

    # Initialize paths with starting node
    paths <- list(from[m])

    repeat {
      for (i in seq_along(paths)) {
        if (!all(is.na(
          get_successors(
            graph,
            paths[[i]][
              length(paths[[i]])])))) {

          # Get the successors for the last node
          # in the given path
          next_nodes <-
            get_successors(
              graph,
              paths[[i]][
                length(paths[[i]])])

          # Filter next_nodes if cycles are detected
          next_nodes <-
            next_nodes[
              which(!(
                next_nodes %in%
                  paths[[i]][
                    1:length(paths[[i]]) - 1]))]

          # Apply traversed nodes to each of the
          # path vectors in a multiple degree context
          if (length(next_nodes) > 1) {
            for (j in seq_along(next_nodes)) {
              if (j == 1) paths[[i]] <-
                  c(paths[[i]], next_nodes[1])

              if (j > 1) paths[[length(paths) + 1]] <-
                  c(paths[[i]][
                    -length(paths[[i]])],
                    next_nodes[j])
            }
          }

          # Apply traversed nodes to each of the
          # path vectors in a single degree context
          if (length(next_nodes) == 1) {
            paths[[i]] <- c(paths[[i]], next_nodes[1])
          }
        }
      }

      # Check each node visited in the present
      # iteration for whether their traversals
      # should end
      for (k in seq_along(paths)) {
        if (k == 1) check <- vector()

        check <-
          c(check,
            any(is.na(
              get_successors(
                graph,
                paths[[k]][length(paths[[k]])]))|
                all(get_successors(
                  graph,
                  paths[[k]][length(paths[[k]])]) %in%
                    paths[[k]])))

        # Remove nodes from vectors within paths
        # if they were previously traversed
        if (paths[[k]][length(paths[[k]])] %in%
            paths[[k]][seq_along(paths[[k]]) - 1]) {
          paths[[k]] <- paths[[k]][-length(paths[[k]])]
        }
      }

      if (all(check)) break
    }

    if (!(length(paths) == 1 && is.na(paths[1]))) {
      all_paths <- c(all_paths, paths)
    }

    if (m == length(from)) paths <- all_paths
  }

  # Arrange vectors in list in order of
  # increasing length
  order <- sapply(seq_along(paths),
                  function(x) length(paths[[x]]))

  names(order) <- seq_along(paths)
  order <- sort(order)
  order <- as.numeric(names(order))
  paths <- paths[order]

  # If only a single vector returned, return NA
  if (length(paths) == 1 && length(paths[[1]]) == 1) {
    return(NA)
  }

  # Remove paths of single length
  for (i in seq_along(paths)) {
    if (i == 1) {
      single_length_paths <- vector(mode = "numeric")
    }

    if (length(paths[[i]]) == 1) {
      single_length_paths <- c(single_length_paths, i)
    }

    if (i == length(paths)) {
      paths[single_length_paths] <- NULL
    }
  }

  # If only `from` but not the `to` node specified,
  # get paths that consider the filtering criteria
  if (!is.null(from) && is.null(to)) {

    # Filter the `path_lengths` vector
    # by the chosen criteria
    if (shortest_path && !longest_path) {

      # Remove paths not of shortest length
      for (i in seq_along(paths)) {
        if (i == 1) {
          not_shortest_length_paths <-
            vector(mode = "numeric")
          shortest_length <-
            min(sapply(seq_along(paths),
                       function(x) length(paths[[x]])))
        }

        if (length(paths[[i]]) != shortest_length) {
          not_shortest_length_paths <-
            c(not_shortest_length_paths, i)
        }

        if (i == length(paths)) {
          paths[not_shortest_length_paths] <- NULL
        }
      }

    } else if (!shortest_path && longest_path) {

      # Remove paths not of longest length
      for (i in seq_along(paths)) {
        if (i == 1) {
          not_longest_length_paths <-
            vector(mode = "numeric")
          longest_length <-
            max(sapply(1:length(paths),
                       function(x) length(paths[[x]])))
        }

        if (length(paths[[i]]) != longest_length) {
          not_longest_length_paths <-
            c(not_longest_length_paths, i)
        }

        if (i == length(paths)) {
          paths[not_longest_length_paths] <- NULL
        }
      }

    } else if (!is.null(distance)) {

      # Remove paths not of the specified distances
      for (i in seq_along(paths)) {
        if (i == 1) {
          not_specified_length_paths <-
            vector(mode = "numeric")
          specified_lengths <- distance
        }

        if (length(paths[[i]]) <
            specified_lengths + 1) {
          not_specified_length_paths <-
            c(not_specified_length_paths, i)
        }

        if (i == length(paths)) {
          paths[not_specified_length_paths] <- NULL
        }
      }

      # Trim paths to specified distance
      for (i in seq_along(paths)) {
        paths[[i]] <-
          paths[[i]][seq_len(specified_lengths + 1)]
      }

      # Create a unique list of paths
      paths <- unique(paths)
    }
  }

  # If both a 'from' node and a 'to' node specified,
  # get paths that begin and end with those nodes
  if (!is.null(from) && !is.null(to)) {

    # Determine which paths contain the 2nd node target
    paths_with_end_node <-
      which(sapply(seq_along(paths),
                   function(x) to %in% paths[[x]]))

    if (length(paths_with_end_node) == 0) {
      return(NA)
    }

    # Obtain a vector of path lengths for each of
    # the valid paths
    path_lengths <-
      sapply(paths_with_end_node,
             function(x) which(paths[[x]] %in% to))

    # Apply list indices as names for the
    # `paths_lengths` vector
    names(path_lengths) <- paths_with_end_node

    # Filter the `path_lengths` vector by the
    # chosen criteria
    if (shortest_path && !longest_path) {
      path_lengths <-
        as.numeric(names(path_lengths[
          which(path_lengths == min(path_lengths))]))
    } else if (!shortest_path && longest_path) {
      path_lengths <-
        as.numeric(names(path_lengths[
          which(path_lengths == max(path_lengths))]))
    } else if (!is.null(distance)) {
      path_lengths <-
        as.numeric(names(path_lengths[
          which(path_lengths %in% distance)]))
    } else if (is.null(distance) && !shortest_path && !longest_path) {
      path_lengths <- as.numeric(names(path_lengths))
    }

    # If there are no paths that match the chosen
    # criteria, return NA
    if (length(path_lengths) == 0) {
      return(NA)
    }

    paths <- paths[path_lengths]

    # Modify `paths` list of vectors such that nodes
    # at the beginning and end of each vector are the
    # `from` and `to` nodes
    for (i in seq_along(paths)) {
      paths[[i]] <-
        paths[[i]][1:which(paths[[i]] == to)]
    }
  }

  if (reverse_paths) {

    for (i in seq_along(paths)) {
      paths[[i]] <- rev(paths[[i]])
    }
  }

  paths
}
