#' @title KUBDatalab theme
#' @description KUBDatalab theme
#' @details Inspired by tvthemes by Ryo Nakagawara
#' @param text.font text font, Default: NULL
#' @param title.font title font, Default: NULL
#' @param legend.font legend font, Default: NULL
#' @param title.size title font size, Default: 18
#' @param text.size text font size, Default: 14
#' @param subtitle.size subtitle font size, Default: 12
#' @param axis.title.size axis title font size, Default: 14
#' @param axis.text.size axis text font size, Default: 10
#' @param legend.title.size legend title font size, Default: 10
#' @param legend.text.size legend text font size, Default: 9
#' @param title.color title color, Default: "#FFD235"
#' @param subtitle.color subtitle.color, Default: "#fee8c8"
#' @param text.color text color, Default: "#fee8c8"
#' @param axis.title.color axis title color, Default: "#fee8c8"
#' @param axis.text.color axis text color, Default: "#fee8c8"
#' @param legend.title.color legend title color, Default: "#ffffff"
#' @param legend.text.color legend text color, Default: "#ffffff"
#' @param legend.position legend position, Default: "bottom"
#' @param ticks add axis ticks, Default: FALSE
#' @param panel.grid.major.color en farveting
#' @param panel.grid.minor.color en anden farveting
#' @examples
#' library(ggplot2)
#'
#' ggplot(airquality, aes(x = Day, y = Temp,
#'      group = as.factor(Month), color = as.factor(Month))) +
#'      geom_point(size = 2.5) +
#'      scale_color_ucph() +
#'      theme_kubdatalab()
#' @rdname theme_kubdatalab
#' @seealso [ggplot2::theme]
#' @importFrom ggplot2 ggplot theme element_text element_rect element_blank element_line
#' theme_minimal
#' @importFrom grid unit
#' @export

theme_kubdatalab <- function(
  text.font = NULL,
  title.font = NULL,
  legend.font = NULL,
  title.size = 18,
  text.size = 14,
  subtitle.size = 12,
  axis.title.size = 14,
  axis.text.size = 10,
  legend.title.size = 10,
  legend.text.size = 9,
  title.color = "#000000",
  subtitle.color = "#000000",
  text.color = "#000000",
  axis.title.color = "#000000",
  axis.text.color = "#000000",
  panel.grid.major.color = "#000000",
  panel.grid.minor.color = "grey",
  legend.title.color = "#000000",
  legend.text.color = "#000000",
  legend.position = "bottom",
  ticks = FALSE){
  kub_datalab <- ggplot2::theme_minimal() +
    theme(text = element_text(family = text.font, size = text.size, color = text.color),
          plot.title = element_text(family = title.font, size = title.size, color = title.color),
          plot.subtitle = element_text(family = title.font, size = subtitle.size, color = subtitle.color),
          # axis options
          axis.text = element_text(family = text.font, size = axis.text.size, color = axis.text.color),
          axis.title = element_text(family = title.font, size = axis.title.size, color = axis.title.color),
          # background/panel options
          plot.background = element_rect(color = NA, fill = "#ffffff"),  # #7199E1
          panel.background = element_rect(color = NA, fill = "#ffffff"), # #7199E1
          panel.grid.major = element_line(color = panel.grid.major.color),
          panel.grid.minor = element_line(color = panel.grid.minor.color),
          # legend options
          legend.text = element_text(family = legend.font, size = legend.text.size, color = legend.text.color),
          legend.title = element_text(family = legend.font, size = legend.title.size, color = legend.title.color),
          legend.position = legend.position,
          legend.key = element_rect(colour = "#000000", linetype = "solid", size = 1.25),
          legend.background = element_rect(color = "#000000", fill = "white", linetype = "solid")
    )

  if (ticks == FALSE) {
    kub_datalab <- kub_datalab + theme(axis.ticks = element_blank(),
                                       axis.ticks.x = element_blank(),
                                       axis.ticks.y = element_blank())

  } else {
    kub_datalab <- kub_datalab + theme(axis.ticks = element_line(size = 0.15, color = "white"),
                                       axis.ticks.x = element_line(size = 0.15, color = "white"),
                                       axis.ticks.y = element_line(size = 0.15, color = "white"),
                                       axis.ticks.length = grid::unit(4, "pt"))
  }

}
