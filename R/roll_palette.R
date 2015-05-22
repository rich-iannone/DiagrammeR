#' Create a simple color palette
#' Generate a randomized set of colors from the CIE Lab color space using a set of HCL range values.
#' @param hue_range a vector representing a range of hues from \code{0} to \code{360}.
#' @param chroma_range a vector representing a range of chroma values from \code{0} to \code{3}.
#' @param lightness_range a vector representing a range of lightness values from \code{0} to \code{2}.
#' @param alpha an optional alpha value in the range of \code{0}-\code{100} to append to the hexadecimal color values.
#' @return a vector of hexadecimal color values.
#' @export roll_palette

roll_palette <- function(number,
                        hue_range = c(0, 360),
                        chroma_range = c(0, 3),
                        lightness_range = c(0.75, 1.5),
                        alpha = NULL,
                        display_colors = TRUE){

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

  # Add function to display colors in the RStudio Viewer
  display_in_viewer <- function(col, border = "light gray", ...){
    n <- length(col)
    plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
         axes = FALSE, xlab = "", ylab = "", ...)
    rect(0:(n-1)/n, 0, 1:n/n, 1, col = col, border = border)
  }

  ct <- V8::new_context("window")

  invisible(ct$source(system.file("htmlwidgets/lib/chromatography/chromatography.js",
                                  package = "DiagrammeR")))

  hex_colors <- unlist(strsplit(ct$eval(js_call), ","))

  if (display_colors == TRUE){

    display_in_viewer(hex_colors)
  }

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
