#' Add a tour
#'
#' This widget creates a tour on a web page.
#'
#' @param steps list of steps for the tour
#' @param options options for the tour
#' @param button boolean indicating if a tour button should be displayed
#' @param width width of the tour button
#' @param height height of the tour button
#' @import htmlwidgets
#' @export
fdTour <- function(steps, options, button = NULL, width = 30, height = 10) {

  # forward options using x
  x = list(
   steps = steps, options = options, button = button
  )
  x = Filter(Negate(is.null), x)

  # create widget
  htmlwidgets::createWidget(
    name = 'fdTour',
    x,
    width = width,
    height = height,
    package = 'flightdeck'
  )
}

fdTour_html = function(id, style, class, ...){
  htmltools::tags$a(id = id, href='#',
    class = paste(class, 'btn btn-success'),
    tags$style("
      .introjs-helperNumberLayer{
        border: none;
        background: steelblue;
        box-shadow: none;
        border-radius: 0;
      }
    ")
  )
}
