#' Displaying a histogram along with summary statistics
#'
#' @param x a numeric vector.
#' @param digits number of digits to display in the summary statistics table.
#' @param plotTitle title of the histogram.
#' @param ... extra arguments (not used currently).
#' @export
#' @examples
#' library(flightdeck)
#' if (interactive()){
#'   runif(1000) %>% 
#'     fdPanelHistogram(plotTitle = 'Histogram of Residuals') %>%
#'     fdPreview
#' }
fdPanelHistogram <- function(x, digits = 4, plotTitle = 'Histogram', 
    ...){
  xSummary <- data.frame(
    Statistic = c("Minimum", "1st Quartile", "Median", "Mean",
      "3rd Quartile", "Maximum"),
    Value = format(summary(x), digits = digits)
  )
  xHistogram <- fdPlotly(
    data = list(list(
      x = unname(x),
      type = 'histogram'
    )),
    list(
      margin = list(t = 40), 
      bargap = 0.05,
      title = plotTitle,
      ...
    ),
    list(displaylogo = FALSE, displayModeBar = FALSE),
    height = 325
  )
  div(class = 'fd-panel-residuals',
      fdColumn(8, xHistogram),
      fdColumn(4, fdSimpleTable(xSummary))
  )
}
