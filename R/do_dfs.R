#' Perform a depth-first search
#' @description With either a single node serving as
#' the starting point or using the whole graph,
#' perform a depth-first search.
#' @param graph a graph object of class
#' \code{dgr_graph} that is created using
#' \code{create_graph}.
#' @param node an optional node ID value to specify
#' a single starting point for the dfs.
#' @return a list object containing dfs information.
#' @examples
#' \dontrun{
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
#' # The `dfs_info` list object contains information
#' # on the nodes visited (in the order visited) and
#' # the complete search path
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
#' # the entire graph has been traversed
#' graph %>% do_dfs
#' #> $visited
#' #>  [1] "8"  "9"  "12" "13" "14" "10" "15" "16" "17"
#' #> [10] "11" "18" "19" "20" "1"  "2"  "4"  "5"  "3"
#' #> [19] "6"  "7"
#' #>
#' #> $search_path
#' #>  [1] "8"  "9"  "12" "9"  "13" "9"  "14" "9"  "8"
#' #> [10] "10" "15" "10" "16" "10" "17" "10" "8"  "11"
#' #> [19] "18" "11" "19" "11" "20" "11" "8"  "1"  "2"
#' #> [28] "4"  "2"  "5"  "2"  "1"  "3"  "6"  "3"  "7"
#' #> [37] "3"  "1"
#' }
#' @export do_dfs

do_dfs <- function(graph,
                   node = NULL) {

  if (is.null(node)) {

    # Get all nodes available in the graph
    graph_nodes <- get_nodes(graph)

    # Initialize the `search_path` vector
    search_path <- vector(mode = "character")

    repeat {

      if (all(graph_nodes %in% unique(search_path))){
        break
      } else {
        starting_node <- sample(setdiff(graph_nodes, unique(search_path)), 1)
        search_path <- c(search_path, starting_node)
        current <- starting_node
      }

      repeat {

        if (all(current == starting_node &
                (all(get_successors(graph, current) %in% search_path) |
                 is.na(get_successors(graph, current))))) {
          break
        } else if (!any(is.na(get_successors(graph, current))) &
                   length(get_successors(graph, current)) > 0 &
                   !all(get_successors(graph, current) %in% search_path)) {
          current <- setdiff(get_successors(graph, current), search_path)[1]
        } else if (any(is.na(get_successors(graph, current))) |
                   all(get_successors(graph, current) %in% search_path)) {
          current <- intersect(get_predecessors(graph, current), search_path)
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
              (all(get_successors(graph, current) %in% search_path) |
               is.na(get_successors(graph, current))))) {
        break
      } else if (!any(is.na(get_successors(graph, current))) &
                 length(get_successors(graph, current)) > 0 &
                 !all(get_successors(graph, current) %in% search_path)) {
        current <- setdiff(get_successors(graph, current), search_path)[1]
      } else if (any(is.na(get_successors(graph, current))) |
                 all(get_successors(graph, current) %in% search_path)) {
        current <- intersect(get_predecessors(graph, current), search_path)
      } else {
        break
      }

      search_path <- c(search_path, current)
    }
  }

  # Create an empty list object `dfs`
  dfs <- list()

  # Add the `visited` and `search_path` vectors to `dfs`
  dfs$visited <- unique(search_path)
  dfs$search_path <- search_path

  return(dfs)
}
