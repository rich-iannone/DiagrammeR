#' Perform a depth-first search
#' @description With either a single node serving as
#' the starting point or using the whole graph,
#' perform a depth-first search.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node an optional node ID value to specify
#' a single starting point for the dfs.
#' @param direction using \code{all} (the default), the
#' dfs will ignore edge direction while traversing
#' through the graph. With \code{out}, traversals
#' between adjacent nodes will respect the edge
#' direction.
#' @return a list object containing dfs information.
#' @examples
#' library(magrittr)
#'
#' # Create a graph containing two balanced trees
#' graph <-
#'   create_graph() %>%
#'   add_balanced_tree(2, 2) %>%
#'   add_balanced_tree(3, 2)
#'
#' # Perform a depth-first search of the smaller tree,
#' # beginning at the root node `1`
#' dfs_info <-
#'   graph %>% do_dfs(1)
#'
#' # The do_dfs function creates a list with
#' # information on the nodes visited (in the order
#' # visited) and the complete search path
#' dfs_info
#' #> $visited
#' #> [1] "1" "2" "4" "5" "3" "6" "7"
#' #>
#' #> $search_path
#' #>  [1] "1" "2" "4" "2" "5" "2" "1" "3" "6" "3" "7"
#' #> [12] "3" "1"
#'
#' # The dfs can be done on the entire graph, where
#' # random nodes are chosen as starting points until
#' # the entire graph has been traversed; here, dfs
#' # started at one tree and finished on the other
#' graph %>% do_dfs
#' #> $visited
#' #>  [1] "3"  "1"  "2"  "4"  "5"  "6"  "7"  "11"
#' #>  [9] "8"  "9"  "12" "13" "14" "10" "15" "16"
#' #> [17] "17" "18" "19" "20"
#' #>
#' #> $search_path
#' #>  [1] "3"  "1"  "2"  "4"  "2"  "5"  "2"  "1"
#' #>  [9] "3"  "6"  "3"  "7"  "3"  "11" "8"  "9"
#' #> [17] "12" "9"  "13" "9"  "14" "9"  "8"  "10"
#' #> [25] "15" "10" "16" "10" "17" "10" "8"  "11"
#' #> [33] "18" "11" "19" "11" "20" "11"
#'
#' # It's also possible to perform dfs while
#' # taking into account edge direction; using
#' # `direction = out` causes the dfs routine to
#' # visit nodes along outward edges
#' graph %>%
#'   do_dfs(3, "out")
#' #> $visited
#' #> [1] "3" "6" "7"
#' #>
#' #> $search_path
#' #> [1] "3" "6" "3" "7" "3"
#'
#' # Reversing the edge directions with the
#' # `reverse_edge_direction()` function and performing
#' # dfs at the same node as before results in a
#' # different set of visited nodes
#' graph %>%
#'   reverse_edge_direction %>%
#'   do_dfs(3, "out")
#' #> $visited
#' #> [1] "3" "1"
#' #>
#' #> $search_path
#' #> [1] "3" "1" "3"
#' @export do_dfs

do_dfs <- function(graph,
                   node = NULL,
                   direction = "all") {

  if (direction == "all") {

    if (is.null(node)) {

      # Get all nodes available in the graph
      graph_nodes <- get_nodes(graph)

      # Initialize the `search_path` and `visited` vectors
      search_path <- vector(mode = "character")
      visited <- vector(mode = "character")

      repeat {

        if (all(graph_nodes %in% unique(search_path))) {
          break
        } else {
          starting_node <-
            sample(setdiff(graph_nodes,
                           unique(search_path)), 1)

          current <- starting_node
          stack <- starting_node
          search_path <- c(search_path, starting_node)
          visited <- c(visited, starting_node)
        }

        repeat {

          if (stack[length(stack)] == starting_node &
              all(get_nbrs(graph, current) %in% visited)) {

            # Entirely traversed, empty stack
            stack <- stack[-length(stack)]
            break

          } else if (!all(get_nbrs(graph, current) %in% visited)) {

            # Available neighbor to visit, add to stack
            current <-
              setdiff(get_nbrs(graph, current),
                      visited)[1]

            stack <- unique(c(stack, current))
            visited <- c(visited, current)
            search_path <- c(search_path, current)

          } else if (all(get_nbrs(graph, current) %in% visited)) {

            # No neighbors to visit, revisit previous in stack
            stack <- stack[-length(stack)]
            current <- stack[length(stack)]
            search_path <- c(search_path, current)

          } else {
            break
          }
        }
      }
    }

    if (!is.null(node)) {

      starting_node <- node
      stack <- starting_node
      search_path <- node
      current <- node
      visited <- node

      repeat {

        if (stack[length(stack)] == starting_node &
            all(get_nbrs(graph, current) %in% visited)) {

          # Entirely traversed, empty stack
          stack <- stack[-length(stack)]
          break

        } else if (!all(get_nbrs(graph, current) %in% visited)) {

          # Available neighbor to visit, add to stack
          current <-
            setdiff(get_nbrs(graph, current),
                    visited)[1]

          stack <- unique(c(stack, current))
          visited <- c(visited, current)
          search_path <- c(search_path, current)

        } else if (all(get_nbrs(graph, current) %in% visited)) {

          # No neighbors to visit, revisit previous in stack
          stack <- stack[-length(stack)]
          current <- stack[length(stack)]
          search_path <- c(search_path, current)

        } else {
          break
        }
      }
    }
  }

  if (direction == "out") {

    if (is.null(node)) {

      # Get all nodes available in the graph
      graph_nodes <- get_nodes(graph)

      # Initialize the `search_path` vector
      search_path <- vector(mode = "character")

      repeat {

        if (all(graph_nodes %in% unique(search_path))) {
          break
        } else {
          starting_node <-
            sample(setdiff(graph_nodes,
                           unique(search_path)), 1)
          search_path <- c(search_path, starting_node)
          current <- starting_node
        }

        repeat {

          if (all(current == starting_node &
                  (all(get_successors(graph, current) %in%
                       search_path) |
                   is.na(get_successors(graph, current))))) {
            break
          } else if (!any(is.na(get_successors(graph, current))) &
                     length(get_successors(graph, current)) > 0 &
                     !all(get_successors(graph, current) %in%
                          search_path)) {
            current <-
              setdiff(get_successors(graph, current),
                      search_path)[1]
          } else if (any(is.na(get_successors(graph, current))) |
                     all(get_successors(graph, current) %in%
                         search_path)) {
            current <-
              intersect(get_predecessors(graph, current),
                        search_path)
          } else {
            break
          }

          search_path <- c(search_path, current)
        }
      }
    }

    if (!is.null(node)) {

      starting_node <- node
      search_path <- starting_node
      current <- starting_node

      repeat {

        if (all(current == starting_node &
                (all(get_successors(graph, current) %in%
                     search_path) |
                 is.na(get_successors(graph, current))))) {
          break
        } else if (!any(is.na(get_successors(graph, current))) &
                   length(get_successors(graph, current)) > 0 &
                   !all(get_successors(graph, current) %in%
                        search_path)) {
          current <-
            setdiff(get_successors(graph, current),
                    search_path)[1]
        } else if (any(is.na(get_successors(graph, current))) |
                   all(get_successors(graph, current) %in%
                       search_path)) {
          current <-
            intersect(get_predecessors(graph, current),
                      search_path)
        } else {
          break
        }

        search_path <- c(search_path, current)
      }
    }
  }

  # Create an empty list object `dfs`
  dfs <- list()

  # Add the `visited` and `search_path` vectors to `dfs`
  dfs$visited <- unique(search_path)
  dfs$search_path <- search_path

  return(dfs)
}
