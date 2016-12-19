#' Progress Bar
progressBar <- function(pct, status = 'success', showText = TRUE){
  div(class = 'progress',
    div(
      class = sprintf('progress-bar progress-bar-%s', status),
      role = 'progressbar',
      style = sprintf("width: %s%%", pct),
      if (showText) span(class = 'progress-bar-number', paste(round(pct, 1), '%'))
    ),
    tags$style('
      .progress-bar-number{padding-left: 5px;}
      .progress-bar{text-align: left;}
    ')
  )
}
