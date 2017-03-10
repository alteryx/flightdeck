#' Interactive Mismatch Matrix
#' 
#' 
#' @param x matrix or table of actual vs. predicted values.
#' @param class html class for table.
#' @param barColor color for bars.
#' @param digits number of digits to display.
#' @param columnNames custom column names for the table.
#' @export
#' @example inst/examples/fdPlotMismatchMatrix.R
fdPlotMismatchMatrix <- function(x, 
    class = 'table table-bordered',
    barColor = '#e15759', digits = 3,
    columnNames = c(actual = 'Actual', predicted = 'Predicted', 
      frequency = 'Frequency', percentage = 'Percentage', 
      cumulative_pct = 'Cumulative Pct'
    )
){
  d <- as.data.frame(x)
  d <- d[(d[[1]] != d[[2]]) & (d[[3]] != 0),]
  d$pct <- d$Freq/sum(d$Freq)*100
  d$pct <- format(d$pct, digits = digits)
  d <- plyr::arrange(d, plyr::desc(pct))
  d$cumpct <- cumsum(d$pct)
  d$cumpct <- format(d$cumpct, digits = digits)
  names(d) <- unname(columnNames)
  table1 <- DT::datatable(
    d,
    rownames = FALSE,
    extensions = c('Buttons', 'Responsive'),
    options = list(),
    style = 'bootstrap',
    width = '100%',
    height = if (NROW(d) > 10) 550 else (250 + NROW(d)*20),
    class = c('stripe', 'hover', 'cell-border'),
    escape = FALSE
  ) %>%
    DT::formatStyle(columnNames['percentage'],
      background = styleColorBarReverse(c(0, 100), barColor),
      backgroundSize = '98% 88%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>%
    DT::formatStyle(columnNames['cumulative_pct'],
      background = styleColorBarReverse(c(0, 100), barColor),
      backgroundSize = '98% 88%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )
  table1
}

styleColorBarReverse <- function (data, color, angle = 90){
  rg = range(data, na.rm = TRUE, finite = TRUE)
  r1 = rg[1]
  r2 = rg[2]
  r = r2 - r1
  htmlwidgets::JS(sprintf("isNaN(parseFloat(value)) || value <= %s ? '' : 'linear-gradient(%sdeg, %s ' + value/%s * 100 + '%%, transparent ' +  value/%s * 100 + '%%)'", 
  r1, angle,  color, r2, r, r2, r))
}



