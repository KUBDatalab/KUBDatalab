#' @title Kursuskalender
#' @description kursuskalender() opens the KUB Datalab course calendar in a browser
#' @details details here
#' @rdname kursuskalender
#' @export

kursuskalender <- function(){
  url <- "https://kubkalender.kb.dk/calendar/datalab"
  browseURL(url)
}

#' @title scale_colour_ucph
#' @rdname kursuskalender
#' @export
coursecalendar <- kursuskalender

