#' Display performance metrics for a regression model
#'
#' @param actual vector of actual values.
#' @param predicted vector of predicted values.
#' @param metrics vector of metrics to compute. See \code{\link{MLmetrics}} for
#'   a complete list of metrics that can be used.
#' @import plyr MLmetrics
#' @export
#' @example inst/examples/fdPanelRegressionMetrics.R
fdPanelRegressionMetrics <- function(actual, predicted,
    metrics = c("MAE", "MAPE", "MedianAPE", "RMSE", "RMSLE", "RAE", "R2_Score")){
  metricsDF = computeRegressionMetrics(actual, predicted, metrics)
  fdPanelMetrics(metricsDF)
}

#' Compute metrics to evaluate performance of a regression model
#' 
#' @inheritParams fdPanelRegressionMetrics
#' @rdname fdPanelRegressionMetrics
#' @export
computeRegressionMetrics <- function(actual, predicted,
   metrics = c("MAE", "MAPE", "MedianAPE", "RMSE", "RMSLE", "RAE", "R2_Score")){
  d <- plyr::ldply(metrics, function(f){
    fn <- getFromNamespace(f, 'MLmetrics')
    value = fn(y_pred = predicted, y_true = actual)
    data.frame(Abbreviation = f, Value = value)
  })
  defn <- read.csv(
    pkgFile('definitions/regression_metrics.csv')
  )
  merge(defn, d)
}

#' Dispaly scatterplot of actual vs. predicted values for a regression model.
#' 
#' @param actual vector of actual values
#' @param predicted vector of predicted values
#' @export
#' @family regression
#' @example inst/examples/fdPanelRegressionScatterplot.R
fdPanelRegressionScatterplot <- function(actual, predicted){
  actual <- unname(as.vector(actual))
  predicted <- unname(as.vector(predicted))
  data = list(
    list(
      x = actual,
      y = predicted,
      type = 'scatter',
      mode = 'markers',
      hoverinfo = "x+y",
      name = 'Data',
      marker = list(size = 5, opacity = 0.5),
      showlegend = F
    ),
    list(
      x = range(actual),
      y = range(predicted),
      type = 'scatter',
      mode = 'line',
      marker = list(size = 10, opacity = 0.5),
      showlegend = F
    )
    
  )
  
  layout = list(
    plot_bgcolor = "#f6f6f6",
    margin = list(
      t = 10,
      l = 40,
      r = 30,
      b = 30
    ),
    xaxis = list(title = 'Actual'),
    yaxis = list(title = 'Predicted')
  )
  
  config <- list(displaylogo = FALSE, displayModeBar = FALSE)
  
  fdPlotly(data, layout, config)
}

#' Display model residuals for a linear regression model
#'
#' @param mod lm model object with a residuals method
#' @param digits number of digits to display in the summary statistics table
#' @param plotTitle title of the histogram.
#' @param ... additional arguments. not used currently.
#' @export
#' @examples
#' library(flightdeck)
#' mod <- lm(mpg ~ ., data = mtcars)
#' if (interactive()){
#'   mod %>% 
#'     fdPanelRegressionResiduals(plotTitle = 'Histogram of Residuals') %>%
#'     fdPreview
#' }
fdPanelRegressionResiduals <- function(mod, digits = 4, 
    plotTitle = 'Histogram', ...){
  res <- residuals(mod)
  fdPanelHistogram(res)
}


#' Display a panel of metrics
#' 
#' @param x a data frame containing metrics to display.
#' @export
fdPanelMetrics <- function(x){
  l <- plyr::alply(x, 1, function(d){
    fdStat(d$Abbreviation, d$Value, note = d$Metric, showBar = d$Scaled == "Yes")
  })
  do.call(tagList, l)
}

