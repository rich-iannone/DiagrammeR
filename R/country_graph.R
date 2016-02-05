#' Create a graph object that contains the boundaries of a country
#' @description Create a graph object that contains the boundaries of a country.
#' @param iso_a2 the ISO 2-letter identifier for a country.
#' @param scaling_factor the factor for which degree coordinates for country
#' boundries will be multiplied.
#' @return a graph object of class \code{dgr_graph}.
#' @importFrom utils read.csv
#' @export country_graph

country_graph <- function(iso_a2 = NULL,
                          scaling_factor = 40){

  country_iso_a2 <- poly_no <- NA

  coordinates_by_country <-
    read.csv(system.file("examples/boundary_coordinates_by_country.csv",
                         package = "DiagrammeR"),
             na.strings = "NANA",
             stringsAsFactors = FALSE)

  all_iso_a2 <-
    c("AF", "AO", "AL", "AE", "AR", "AM", "AQ", "TF", "AU", "AT", "AZ", "BI",
      "BE", "BJ", "BF", "BD", "BG", "BS", "BA", "BY", "BZ", "BO", "BR", "BN",
      "BT", "BW", "CF", "CA", "CH", "CL", "CN", "CI", "CM", "CD", "CG", "CO",
      "CR", "CU", "CY", "CZ", "DE", "DJ", "DK", "DO", "DZ", "EC", "EG", "ER",
      "ES", "EE", "ET", "FI", "FJ", "FK", "FR", "GA", "GB", "GE", "GH", "GN",
      "GM", "GW", "GQ", "GR", "GL", "GT", "GY", "HN", "HR", "HT", "HU", "ID",
      "IN", "IE", "IR", "IQ", "IS", "IL", "IT", "JM", "JO", "JP", "KZ", "KE",
      "KG", "KH", "KR", "KW", "LA", "LB", "LR", "LY", "LK", "LS", "LT", "LU",
      "LV", "MA", "MD", "MG", "MX", "MK", "ML", "MM", "ME", "MN", "MZ", "MR",
      "MW", "MY", "NA", "NC", "NE", "NG", "NI", "NL", "NO", "NP", "NZ", "OM",
      "PK", "PA", "PE", "PH", "PG", "PL", "PR", "KP", "PT", "PY", "PS", "QA",
      "RO", "RU", "RW", "EH", "SA", "SD", "SS", "SN", "SB", "SL", "SV", "SO",
      "RS", "SR", "SK", "SI", "SE", "SZ", "SY", "TD", "TG", "TH", "TJ", "TM",
      "TL", "TT", "TN", "TR", "TW", "TZ", "UG", "UA", "UY", "US", "UZ", "VE",
      "VN", "VU", "YE", "ZA", "ZM", "ZW")

  if (!is.null(iso_a2)){
    countries <- iso_a2
  }

  if (is.null(iso_a2)){
    countries <- all_iso_a2
  }

  for (k in 1:length(countries)){

    country_subset <- subset(coordinates_by_country,
                             country_iso_a2 == countries[k])

    for (i in 1:length(unique(country_subset$poly_no))){

      if (i == 1) {

        nodes <- create_nodes(nodes = "")

        edges <- create_edges(from = "", to = "")

        edges <- edges[-1,]
      }

      subset_poly <- subset(country_subset, poly_no == i)

      nodes <-
        combine_nodes(nodes,
                      create_nodes(nodes = paste0(countries[k],
                                                  "_",
                                                  rownames(subset_poly)),
                                   label = FALSE,
                                   type = "border_point",
                                   shape = "image",
                                   x = subset_poly$lon * scaling_factor,
                                   y = subset_poly$lat * scaling_factor,
                                   image = "https://raw.githubusercontent.com/rich-iannone/DiagrammeR/master/inst/examples/NFFFFFF-0.png"))

      edges <-
        combine_edges(edges,
                      create_edges(from = paste0(countries[k],
                                                 "_",
                                                 rownames(subset_poly)),
                                   to = c(paste0(countries[k],
                                                 "_",
                                                 rownames(subset_poly)[2:length(rownames(subset_poly))]),
                                          paste0(countries[k],
                                                 "_",
                                                 rownames(subset_poly)[1])),
                                   color = "black",
                                   arrow = FALSE))
    }

    nodes <- nodes[-1,]

    if (k == 1){
      dgr_graph <- create_graph(nodes_df = nodes,
                                edges_df = edges,
                                generate_dot = FALSE)
    }

    if (k > 1){
      graph_2 <- create_graph(nodes_df = nodes,
                              edges_df = edges,
                              generate_dot = FALSE)

      dgr_graph <- combine_graphs(dgr_graph, graph_2)
    }
  }

  return(dgr_graph)
}
