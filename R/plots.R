#' Interactive Conditional Density Plot
#' 
#' 
#' @param x a numeric vector
#' @param y a "factor" interpreted to be the dependenty variable
#' @param title optional title for the plot
#' @param ... additional arguments to pass to \code{\link[graphics]{cdplot}}
#' @example inst/examples/fdPlotConditionalDensity.R
#' @export
# TODO
# 1. add annotations to indicate y variable levels
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

#' Plot a confusion matrix
#' 
#' @param x confusion matrix with row and column names representing classes
#' @param class html class to style the confusion matrix table
#' @export
#' @example inst/examples/fdPlotConfusionMatrix.R 
fdPlotConfusionMatrix <- function(x, class = 'table table-bordered'){
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
      pct <- d[i, j]/sum(d[i,])
      title <- sprintf('%s %% of %s are %s classified as %s',
                       format(pct*100, 2), rownames(d)[i], phrase, colnames(d)[j]             
      )
      background = if (i == j) {
        paste0('rgba(44, 160, 44, ', pct , ' )')
      } else {
        paste0('rgba(214, 39, 40,  ', pct, ' )')
      }
      d2[i,j] <- makeTooltip(
        paste0(d[i, j], ' (', format(pct*100, 2), '%)'),
        title, 
        pct, 
        phrase
      )
    }
  }
  d2 <- cbind(Actual = rownames(d2), d2)
  fdSimpleTable(d2, class = paste(class, 'fd-confusion-matrix'))
}
