#' Create an interactive plot using plotly.
#'
#' This function is a lightweight wrapper around plotly.js. For complete
#' documentation, please refer to \url{https://plot.ly/javascript/reference/}.
#'
#' @import htmlwidgets
#' @export
#' @example inst/examples/fdPlotly.R
fdPlotly <- function(data, layout = NULL, config = NULL, width = NULL,
    height = NULL) {
  # forward options using x
  x = list(
    data = data, layout = layout, config = config
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'plotlyLite',
    x,
    width = width,
    height = height,
    sizingPolicy = htmlwidgets::sizingPolicy(
      browser.fill = TRUE, defaultWidth = "100%", defaultHeight = 400
    ),
    package = 'flightdeck'
  )
}


