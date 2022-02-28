kb_palette <- list(
  ## Blå er den primære kb farve - de øvrige er sekundære klare farver
  prim_klare = c(
    "#002e70", # Blå - dark blue 100 - primary
    "#323232", # Dark gray
    "#B30018", # red 100 - danger
    "#F391B8", # Pink100
    "#F4B664", # Orange
    "#FEEC89", # Yellow 100
    "#96EcFD", # Sky bliue 100
    "#26D466" # green 100
  ),
  sek_dark = c(
    "#002546", # Blå (mørk) - Pantone 289 EC
    "#281c41", # Lilla (mørk) - Pantone 5265
    "#003e5c", # Cyan (mørk) - Pantone 3025 EC
    "#004543", # Turkis (mørk) - Pantone 567 EC
    "#425821", # Grøn (mørk) - Pantone 574 EC
    "#634b03", # Gul (mørk) - Pantone 455 EC
    "#5f3408", # Orange (mørk) - Pantone 463 EC
    "#5b0c0c", # Rød (mørk) - Pantone 490 EC
    "#5f0030", # Magenta (mørk) - Pantone 229 EC
    "#4b4b4a"  # Grå (mørk) - Pantone 11 C
  )
)



#' @title KB palette
#' @description KB palette of colours used by the University of Århus
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname kb_pal
#' @export
#' @examples
#' library(scales)
#' show_col(kb_pal()(5))
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

kb_pal <- function(palette = "prim_klare",
                     n, type = c("discrete", "continous"),
                     reverse = FALSE){
  kb <- kb_palette[[palette]]

  if (reverse == TRUE){
    kb <- rev(kb)
  }

  if (missing(n)){
    n <- length(kb)
  }

  type <- match.arg(type)

  if ( type == "discrete" && n > length(kb)){
    stop(glue::glue("Palette does not have {n} colors, maximum is {length(kb)}!"))
  }

  kb <- switch(type,
                 continuous = grDevices::colorRampPalette(kb)(n),
                 discrete = kb[1:n])

  kb <- scales::manual_pal(kb)

  return(kb)

}


#' @title scale_colour_kb
#' @rdname kb_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_kb()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_kb <- function(palette = "prim_klare", n, type = "discrete",
                             reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("colour", "kb",
                            kb_pal(palette = palette, n = n, type = type,
                                     reverse = reverse), ...)
  } else { ## needs work...
    ggplot2::scale_colour_gradientn(colours = kb_pal(palette = palette, n = n, type = type,
                                                     reverse = reverse)(8))
  }
}


#' @title scale_color_kb
#' @rdname kb_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_kb()
#' @importFrom ggplot2 discrete_scale scale_colour_gradientn

scale_color_kb <- scale_colour_kb


#' @title scale_fill_kb
#' @rdname kb_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'      geom_histogram(aes(fill = class), col = "black", size = 0.1) +
#'      scale_fill_kb()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_kb <- function(n, type = "discrete",
                                reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("fill", "kb",
                            kb_pal(n = n, type = type,
                                         reverse = reverse), ...)
  } else { ## needs work...
    ggplot2::scale_fill_gradientn(colours = kb_pal(n = n, type = type,
                                                        reverse = reverse)(8))
  }
}

