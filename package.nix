{ pkgs ? import <nixpkgs> {}, displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "DiagrammeR";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = ''
    Build graph/network structures using functions for stepwise addition and
    deletion of nodes and edges. Work with data available in tables for bulk
    addition of nodes, edges, and associated metadata. Use graph selections
    and traversals to apply changes to specific nodes or edges. A wide
    selection of graph algorithms allow for the analysis of graphs. Visualize
    the graphs and take advantage of any aesthetic properties assigned to
    nodes and edges.'';
  propagatedBuildInputs = with pkgs.rPackages; [ 
    visNetwork
    rlang
    tidyr
    magrittr
    downloader
    htmlwidgets
    rstudioapi
    glue
    htmltools
    dplyr
    stringr
    tibble
    readr
    RColorBrewer
    scales
    influenceR
    igraph
    viridis
    purrr
 ];

}
