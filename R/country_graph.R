#' Create a graph object that contains the boundaries of a country
#' @description Create a graph object that contains the boundaries of a country.
#' @param iso_a2 the ISO 2-letter identifier for a country.
#' @param option an integer that is used for alternate views of certain
#' countries.
#' @return a graph object of class \code{dgr_graph}.
#' @export country_graph

country_graph <- function(iso_a2 = NULL,
                          option = 1){

  coordinates_by_country <-
    read.csv(system.file("examples/boundary_coordinates_by_country.csv",
                         package = "DiagrammeR"),
             stringsAsFactors = FALSE)

  if (!is.null(iso_a2)){
    countries <- iso_a2
  }

  if (is.null(iso_a2)){
    countries <- unique(coordinates_by_country$country_iso_a2)
  }


  for (i in 1:length(unique(country_subset$poly_no))){

    if (i == 1){

      nodes <- create_nodes(nodes = "")

      edges <- create_edges(from = "", to = "")
      edges <- edges[-1,]
    }

    subset_poly <- subset(country_subset, poly_no == i)

    nodes <-
      combine_nodes(nodes,
                    create_nodes(nodes = rownames(subset_poly),
                                 shape = "image",
                                 x = subset_poly$lon * 20,
                                 y = subset_poly$lat * 20,
                                 image = "https://raw.githubusercontent.com/rich-iannone/DiagrammeR/master/inst/examples/NFFFFFF-0.png"))

    edges <-
      combine_edges(edges,
                    create_edges(from = rownames(subset_poly),
                                 to = c(rownames(subset_poly)[2:length(rownames(subset_poly))],
                                        rownames(subset_poly)[1]),
                                 color = "black",
                                 arrow = FALSE))
  }

  nodes <- nodes[-1,]

  dgr_graph <- create_graph(nodes_df = nodes,
                            edges_df = edges)

  return(dgr_graph)
}
