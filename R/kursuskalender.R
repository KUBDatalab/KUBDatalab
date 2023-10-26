#' @title Kursuskalender
#' @description kursuskalender() opens the KUB Datalab course calendar in a browser
#' @details details here
#' @rdname kursuskalender
#' @importFrom utils browseURL
#' @export

kursuskalender <- function(){
  url <- "https://kubkalender.kb.dk/calendar/datalab"
  browseURL(url)
}

#' @title Course calendar
#' @rdname kursuskalender
#' @export
coursecalendar <- kursuskalender

