ruc_palette <- list(
  lys = c(
    "#B7C9D3", # Lys grå - pantone 5445C
    "#71b790", # Lys grøn - pantone 2248C
    "#59cbe8", # Lys blå - Pantone 305C
    "#ef95cf", # Lys rød - Pantone 223C
    "#f6eb61"  # Lys gul - Pantone 100C
  ),
  dark = c(
    "#425563", # Mørk grå - Pantone 7545C
    "#00685e", # Støvet grøn - Pantone 329C
    "#10069f", # Mørk blå - Pantone 072C
    "#f65275", # Orange rød - pantone 184c
    "#f4da40"  # Varm gul - pantone 7404C
  )
)



#' @title RUC palette
#' @description RUC palette
#' @inheritDotParams ggplot2::discrete_scale
#' @param n number of colors
#' @param type discrete or continuous
#' @param reverse reverse order, Default: FALSE
#' @rdname ruc_pal
#' @export
#' @examples
#' library(scales)
#' show_col(ruc_pal()(5))
#' @importFrom scales manual_pal
#' @importFrom glue glue
#' @importFrom grDevices colorRampPalette

ruc_pal <- function(palette = "dark",
                     n, type = c("discrete", "continous"),
                     reverse = FALSE){
  ruc <- ruc_palette[[palette]]

  if (reverse == TRUE){
    ruc <- rev(ruc)
  }

  if (missing(n)){
    n <- length(ruc)
  }

  type <- match.arg(type)

  if ( type == "discrete" && n > length(ruc)){
    stop(glue::glue("Palette does not have {n} colors, maximum is {length(ruc)}!"))
  }

  ruc <- switch(type,
                 continuous = grDevices::colorRampPalette(ruc)(n),
                 discrete = ruc[1:n])

  ruc <- scales::manual_pal(ruc)

  return(ruc)

}


#' @title scale_colour_ruc
#' @rdname ruc_pal
#' @export
#' @examples
#'
#' library(ggplot2)
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_colour_ruc()
#' @importFrom ggplot2 discrete_scale scale_colour_gradientn

scale_colour_ruc <- function(n, type = "discrete",
                             reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("colour", "ruc",
                            ruc_pal(n = n, type = type,
                                     reverse = reverse), ...)
  } else { ## needs work...
    ggplot2::scale_colour_gradientn(colours = ruc_pal(n = n, type = type,
                                                     reverse = reverse)(8)) # det her 8 - hvorfor, og er det rettet i de andre paletter?
  }
}


#' @title scale_color_ruc
#' @rdname ruc_pal
#' @export
#' @examples
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_ruc()
#' @importFrom ggplot2 discrete_scale scale_color_gradientn

scale_color_ruc <- scale_colour_ruc


#' @title scale_fill_ruc
#' @rdname ruc_pal
#' @export
#' @examples
#'
#' ggplot(mpg, aes(displ)) +
#'      geom_histogram(aes(fill = class), col = "black", size = 0.1) +
#'      scale_fill_ruc()
#' @importFrom ggplot2 discrete_scale scale_fill_gradientn

scale_fill_ruc <- function(n, type = "discrete",
                                reverse = FALSE, ...){
  if (type == "discrete") {
    ggplot2::discrete_scale("fill", "ruc",
                            ruc_pal(n = n, type = type,
                                         reverse = reverse), ...)
  } else { ## needs work...
    ggplot2::scale_fill_gradientn(colours = ruc_pal(n = n, type = type,
                                                        reverse = reverse)(8))  # dette 8 - hvorfor, og rettet i andre paletter?
  }
}

