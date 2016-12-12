#' A light wrapper around plotly.js
#'
#' This function is a lightweight wrapper around plotly.js. For complete
#' documentation, please refer to \url{https://plot.ly/javascript/reference/}
#'
#' @import htmlwidgets
#' @examples
#'  d = data.frame(
#'    x = 1:5,
#'    y = (1:5)*2
#'  )
#'  plotlyLite(data = list(d))
#' @export
plotlyLite <- function(data, layout = NULL, config = NULL, width = NULL,
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
