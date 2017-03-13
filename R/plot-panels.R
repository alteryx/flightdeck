#' Display a histogram and table of summary statistics
#'
#' @param x a numeric vector.
#' @param digits number of digits to display in the summary statistics table.
#' @param plotTitle title of the histogram.
#' @param statNames named vector of statistics.
#' @param tabColNames named vector of table column names.
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
    statNames = c(minimum = "Minimum", q1 = "1st Quartile", 
      median = "Median", mean = "Mean", q3 = "3rd Quartile", maximum = "Maximum"
    ), 
    tabColNames = c(stat = 'Statistic', val = 'Value'),
    ...
){
  x <- as.vector(x)
  xSummary <- data.frame(
    stat = statNames[c("minimum", "q1", "median", "mean", "q3", "maximum")],
    val = format(summary(x), digits = digits)
  )
  names(xSummary) <- tabColNames[c('stat', 'val')]
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

makePanel <- function(...){
  div(..., `data-widget-type` = 'panel')
}
