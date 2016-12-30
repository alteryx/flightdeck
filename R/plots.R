#' Interactive Conditional Density Plot
#' 
#' 
#' @param x a numeric vector
#' @param y a "factor" interpreted to be the dependenty variable
#' @param title optional title for the plot
#' @param showlegend boolean indicating if a legend should be displayed.
#' @param ... additional arguments to pass to \code{\link[graphics]{cdplot}}
#' @example inst/examples/fdPlotConditionalDensity.R
#' @export
# TODO
# 1. add annotations to indicate y variable levels
fdPlotConditionalDensity <- function(x, y, title  = NULL, showlegend = TRUE, ...){
  dots <- list(...)
  cdcPlotData <- cdplot(x, y, ..., plot = F)
  xPoints <- pretty(range(x), n = 100)
  traces = lapply(names(cdcPlotData), function(d){
    list(x = xPoints, y = cdcPlotData[[d]](xPoints), 
      fill = 'tozeroy', type = 'scatter', name = d,
      showlegend = showlegend
    )
  })
  layout = list(
    plot_bgcolor = "#f6f6f6",
    margin = list(
      t = 30,
      l = 40,
      r = 30,
      b = 40
    ),
    xaxis = list(title = dots$xlab),
    yaxis = list(range = c(0, 1)),
    title = title
  )
  config <- list(displaylogo = FALSE, displayModeBar = FALSE)
  fdPlotly(traces, layout, config, width = '100%')
}

#' Interactive Confusion Matrix
#' 
#' @param x confusion matrix with row and column names representing classes. It 
#'   is expected that the rownames represent actual classes while column names
#'   represent the predicted classes
#' @param class html class to style the confusion matrix table
#' @param digits number of digits to display in the tooltip
#' @export
#' @example inst/examples/fdPlotConfusionMatrix.R 
fdPlotConfusionMatrix <- function(x, class = 'table table-bordered', digits = 3){
  if (is.table(x)) {
    x <- as.data.frame.matrix(x) 
  } else if (is.matrix(x)){
    x <- as.data.frame(x)
  }
  makeTooltip <- function(x, title, pct, phrase){
    tpl <- '
    <div data-toggle="tooltip" title="%s"data-value=%s data-phrase="%s">
    %s
    </div>
    '
    sprintf(tpl, title, pct, phrase, x)
  }
  d2 <- x
  for (i in 1:NROW(d2)){
    for (j in 1:NCOL(d2)){
      phrase <- if (i == j) 'correctly' else 'incorrectly'
      pct <- x[i, j]/sum(x[i,])
      title <- sprintf('%s %% of %s are %s classified as %s',
        format(pct*100, digits = digits), rownames(x)[i], phrase, colnames(x)[j]
      )
      background = if (i == j) {
        paste0('rgba(44, 160, 44, ', pct , ' )')
      } else {
        paste0('rgba(214, 39, 40,  ', pct, ' )')
      }
      d2[i,j] <- makeTooltip(
        paste0(x[i, j], ' (', format(pct*100, digits = digits), '%)'),
        title, 
        pct, 
        phrase
      )
    }
  }
  d2 <- cbind(Actual = rownames(d2), d2)
  fdSimpleTable(d2, class = paste(class, 'fd-confusion-matrix'))
}
