#' Interactive Conditional Density Plot
#' 
#' 
#' @param x a numeric vector
#' @param y a "factor" interpreted to be the dependenty variable
#' @param title optional title for the plot
#' @param ... additional arguments to pass to \code{\link[graphics]{cdplot}}
#' @example inst/examples/fdPlotConditionalDensity.R
#' @export
fdPlotConditionalDensity <- function(x, y, title  = NULL, ...){
  dots <- list(...)
  cdcPlotData <- cdplot(x, y, ..., plot = F)
  xPoints <- pretty(range(x), n = 100)
  traces = lapply(names(cdcPlotData), function(d){
    list(x = xPoints, y = cdcPlotData[[d]](xPoints), 
         fill = 'tozeroy', type = 'scatter', name = d
    )
  })
  layout = list(
    xaxis = list(title = dots$xlab),
    yaxis = list(range = c(0, 1)),
    title = title
  )
  fdPlotly(traces, layout)
}