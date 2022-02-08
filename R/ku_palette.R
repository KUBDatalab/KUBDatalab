ku_palette <- list(
  ## dark ----
  dark  = c(
    "#901a1E", # Rød
    "#122947", # Blå
    "#0a5963", # Petroleum
    "#39641c", # Grøn
    "#3d3d3d"  # Grå
  ),
  mellem = c(
    "#c73028", # Rød
    "#425570", # Blå
    "#197f8e", # Petroleum
    "#4b8325", # Grøn
    "#666666", # Grå
    "#fefaf2"  # Champagne
  ),
  lys = c(
    "#dB3B0A", # Rød
    "#bac7d9", # Blå
    "#b7d7de", # Petroleum
    "#becaa8", # Grøn
    "#e1dfdf", # Grå
    "#ffbd38"  # Gul
  )
)



#' @title UCPH palette
#' @description UCPH palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname ucph_pal
#' @export
#' @examples
#' library(scales)
#' show_col(ucph_pal()(5))
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

ucph_pal <- function(
                     n, type = c("discrete", "continuous"),palette = "dark",
                     reverse = FALSE){

    ucph <- ku_palette[[palette]]

  if (reverse == TRUE){
    ucph <- rev(ucph)
  }

  if (missing(n)){
    n <- length(ucph)
  }

  type <- match.arg(type)

  if ( type == "discrete" && n > length(ucph)){
    stop(glue::glue("Palette does not have {n} colors, maksimum is {length(ucph)}!"))
  }

  ucph <- switch(type,
                 continuous = grDevices::colorRampPalette(ucph)(n),
                 discrete = ucph[1:n])

  ucph <- scales::manual_pal(values=ucph)

  return(ucph)

}



#' @title scale_colour_ucph
#' @rdname ucph_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_ucph()
#' @importFrom ggplot2 discrete_scale scale_colour_gradientn

scale_colour_ucph <- function(palette = "dark", n, type = "discrete",
                             reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("colour", "ucph",
                            ucph_pal(palette = palette, n = n, type = type,
                                     reverse = reverse), ...)
  } else { ## needs work...
    ggplot2::scale_colour_gradientn(colours = ucph_pal(palette = palette, n = n, type = type,
                                                     reverse = reverse)(length(ku_palette[[palette]])))
  }
}


#' @title scale_color_ucph
#' @rdname ucph_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_ucph()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_ucph <- scale_colour_ucph


#' @title scale_fill_ucph
#' @rdname ucph_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'      geom_histogram(aes(fill = class), col = "black", size = 0.1) +
#'      scale_fill_ucph()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_ucph <- function(n, type = "discrete",
                            palette= "lys",
                                reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("fill", "ucph",
                            ucph_pal(n = n, type = type, palette=palette,
                                         reverse = reverse), ...)
  } else { ## needs work...
      ggplot2::scale_fill_gradientn(colours = ucph_pal(palette = palette, n = n, type = type,
                                                       reverse = reverse)(length(ku_palette[[palette]])))

  }
}

