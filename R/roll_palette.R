#' Create a simple color palette
#' @description Generate a randomized set of colors from the CIE Lab color
#' space using a set of HCL range values.
#' @param number the number of colors to include in the palette.
#' @param hue_range a vector representing a range of hues from \code{0} to
#' \code{360}.
#' @param chroma_range a vector representing a range of chroma values from
#' \code{0} to \code{3}.
#' @param lightness_range a vector representing a range of lightness values
#' from \code{0} to \code{2}.
#' @param alpha an optional alpha value in the range of \code{0}-\code{100} to
#' append to the hexadecimal color values.
#' @return a vector of hexadecimal color values.
#' @examples
#' \dontrun{
#' # Create a palette of 12 colors
#' roll_palette(number = 12,
#'              hue_range = c(0, 360),
#'              chroma_range = c(0, 3),
#'              lightness_range = c(0.75, 1.5),
#'              alpha = NULL,
#'              display_colors = TRUE)
#'
#' #> [1] "#F7DC9D" "#D937E5" "#8880B9" "#119F35" "#7CEAB7"
#' #> [6] "#F8877D" "#C08B1B" "#B1E2F9" "#8E9573" "#74A31C"
#' #> [11] "#51A053" "#50C9A4"
#' }
#' @importFrom V8 new_context
#' @importFrom utils packageVersion
#' @export roll_palette

roll_palette <- function(number,
                         hue_range = c(0, 360),
                         chroma_range = c(0, 3),
                         lightness_range = c(0.75, 1.5),
                         alpha = NULL){

  # Check to make sure that V8 is available
  if (!requireNamespace("V8")) stop("V8 is required to export.", call. = F)

  stopifnot(packageVersion("V8") >= "0.6")

  # Construct the JS call
  js_call <- paste0("var colors = createPalette.generate(",
                    number, ", function(color){ var hcl = color.hcl(); return hcl[0]>=",
                    hue_range[1], " && hcl[0]<=",
                    hue_range[2], "&& hcl[1]>=",
                    chroma_range[1], "&& hcl[1]<=",
                    chroma_range[2], "&& hcl[2]>=",
                    lightness_range[1], "&& hcl[2]<=",
                    lightness_range[2], "; }, true, 50);",
                    "var colors = createPalette.diffSort(colors); colors;")

  ct <- new_context("window")

  invisible(ct$source(system.file("htmlwidgets/lib/chromatography/chromatography.js",
                                  package = "DiagrammeR")))

  hex_colors <- unlist(strsplit(ct$eval(js_call), ","))

  if (!is.null(alpha)){

    is_alpha_numeric <- ifelse(is.numeric(alpha), TRUE, FALSE)

    if (is_alpha_numeric == FALSE) is_alpha_in_range <- FALSE

    if (is_alpha_numeric == TRUE){
      is_alpha_in_range <- ifelse(alpha >= 0 & alpha <= 100, TRUE, FALSE)
    }

    if (is_alpha_in_range == TRUE){

      alpha <- round(alpha, digits = 0)

      if (alpha != 100){

        hex_colors <- paste0(hex_colors,
                             formatC(alpha,
                                     width = 2,flag = "0"))
      }
    }
  }

  return(hex_colors)
}
