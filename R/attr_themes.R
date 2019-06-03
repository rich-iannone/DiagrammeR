#' Transform a tibble to a data frame
#'
#' Simply uses `as.data.frame()` with an input tibble and ensures that
#' `stringsAsFactors` is `FALSE`.
#' @param tbl A tibble.
#' @noRd
asdf <- function(tbl) {
  tbl %>% as.data.frame(stringsAsFactors = FALSE)
}

#' The default attribute theme
#' @noRd
#' @importFrom dplyr tribble
attr_theme_default <- function() {

  dplyr::tribble(
    ~attr,                  ~value,                  ~attr_type,
    "layout",               "neato",                 "graph",
    "outputorder",          "edgesfirst",            "graph",
    "bgcolor",              "white",                 "graph",
    "fontname",             "Helvetica",             "node",
    "fontsize",             "10",                    "node",
    "shape",                "circle",                "node",
    "fixedsize",            "true",                  "node",
    "width",                "0.5",                   "node",
    "style",                "filled",                "node",
    "fillcolor",            "aliceblue",             "node",
    "color",                "gray70",                "node",
    "fontcolor",            "gray50",                "node",
    "fontname",             "Helvetica",             "edge",
    "fontsize",             "8",                     "edge",
    "len",                  "1.5",                   "edge",
    "color",                "gray80",                "edge",
    "arrowsize",            "0.5",                   "edge"
  ) %>%
    asdf()
}

#' The lr attribute theme
#' @noRd
#' @importFrom dplyr bind_rows tribble
attr_theme_lr <- function() {

  dplyr::bind_rows(
    dplyr::tribble(
      ~attr,                  ~value,                  ~attr_type,
      "layout",               "dot",                   "graph",
      "rankdir",              "LR",                    "graph"
    ) %>%
      asdf(),
    attr_theme_default()[-1, ])
}

#' The tb attribute theme
#' @noRd
#' @importFrom dplyr bind_rows tribble
attr_theme_tb <- function() {

  dplyr::bind_rows(
    dplyr::tribble(
      ~attr,                  ~value,                  ~attr_type,
      "layout",               "dot",                   "graph",
      "rankdir",              "TB",                    "graph"
    ) %>%
      asdf(),
    attr_theme_default()[-1, ])
}

#' The rl attribute theme
#' @noRd
#' @importFrom dplyr bind_rows tribble
attr_theme_rl <- function() {

  dplyr::bind_rows(
    dplyr::tribble(
      ~attr,                  ~value,                  ~attr_type,
      "layout",               "dot",                   "graph",
      "rankdir",              "RL",                    "graph"
    ) %>%
      asdf(),
    attr_theme_default()[-1, ])
}

#' The bt attribute theme
#' @noRd
#' @importFrom dplyr bind_rows tribble
attr_theme_bt <- function() {

  dplyr::bind_rows(
    dplyr::tribble(
      ~attr,                  ~value,                  ~attr_type,
      "layout",               "dot",                   "graph",
      "rankdir",              "BT",                    "graph"
    ) %>%
      asdf(),
    attr_theme_default()[-1, ])
}

#' The fdp attribute theme
#' @noRd
#' @importFrom dplyr tribble
attr_theme_fdp <- function() {

  dplyr::tribble(
    ~attr,                  ~value,                  ~attr_type,
    "layout",               "fdp",                   "graph",
    "outputorder",          "edgesfirst",            "graph",
    "bgcolor",              "white",                 "graph",
    "fontname",             "Helvetica",             "node",
    "fontsize",             "5",                     "node",
    "shape",                "circle",                "node",
    "fixedsize",            "true",                  "node",
    "width",                "0.12",                  "node",
    "style",                "filled",                "node",
    "fillcolor",            "aliceblue",             "node",
    "color",                "gray70",                "node",
    "fontcolor",            "gray70",                "node",
    "fontname",             "Helvetica",             "edge",
    "fontsize",             "5",                     "edge",
    "len",                  "1.5",                   "edge",
    "color",                "gray80",                "edge",
    "arrowsize",            "0.5",                   "edge"
  ) %>%
    asdf()
}

#' The kk attribute theme
#' @noRd
#' @importFrom dplyr bind_rows tribble
attr_theme_kk <- function() {

  dplyr::bind_rows(
    dplyr::tribble(
      ~attr,                  ~value,                  ~attr_type,
      "layout",               "neato",                 "graph",
      "mode",                 "KK",                    "graph"
    ) %>%
      asdf(),
    attr_theme_default()[-1, ])
}
