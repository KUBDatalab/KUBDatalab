au_palette <- list(
  ## Blå er den primære AU farve - de øvrige er sekundære klare farver
  sek_klare = c(
    "#003d73", # Blå - Pantone 287 EC
    "#655a9f", # Lilla - Pantone 2665 EC
    "#37a0cb", # Cyan - Pantone Process Cyan EC
    "#00aba4", # Turkis - Pantone 326 EC
    "#8bad3f", # Grøn - Pantone 376 EC
    "#fabb00", # Gul - Pantone 7408 EC
    "#ee7f00", # Orange - Pantone 144 EC
    "#e2001a", # Rød - Pantone 485 EC
    "#e2007a", # Magenta - Pantone Process Magenta EC
    "#878787"  # Grå - Pantone Cool Gray 7 EC
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



#' @title AU palette
#' @description AU palette of colours used by the University of Århus
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname au_pal
#' @export
#' @examples
#' library(scales)
#' show_col(au_pal()(5))
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

au_pal <- function(palette = "sek_klare",
                     n, type = c("discrete", "continous"),
                     reverse = FALSE){
  au <- au_palette[[palette]]

  if (reverse == TRUE){
    au <- rev(au)
  }

  if (missing(n)){
    n <- length(au)
  }

  type <- match.arg(type)

  if ( type == "discrete" && n > length(au)){
    stop(glue::glue("Palette does not have {n} colors, maximum is {length(au)}!"))
  }

  au <- switch(type,
                 continuous = grDevices::colorRampPalette(au)(n),
                 discrete = au[1:n])

  au <- scales::manual_pal(values=au)

  return(au)

}


#' @title scale_colour_au
#' @rdname au_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_au()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_colour_au <- function(palette = "sek_dark", n, type = "discrete",
                             reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("colour", "au",
                            au_pal(palette = palette, n = n, type = type,
                                     reverse = reverse), ...)
  } else { ## needs work...
    ggplot2::scale_colour_gradientn(colours = au_pal(palette = palette, n = n, type = type,
                                                     reverse = reverse)(length(au_palette[[palette]])))
  }
}


#' @title scale_color_au
#' @rdname au_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_au()
#' @importFrom ggplot2 discrete_scale scale_colour_gradientn

scale_color_au <- scale_colour_au


#' @title scale_fill_au
#' @rdname au_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'      geom_histogram(aes(fill = class), col = "black", size = 0.1) +
#'      scale_fill_au()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_au <- function(n, type = "discrete",
                                reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("fill", "au",
                            au_pal(n = n, type = type,
                                         reverse = reverse), ...)
  } else { ## needs work...
    ggplot2::scale_fill_gradientn(colours = au_pal(n = n, type = type,
                                                        reverse = reverse)(8))
  }
}

