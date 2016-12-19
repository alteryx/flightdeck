#' Compute metrics to evaluate performance of a regression model.
#'
#' @param actual vector of actual values.
#' @param predicted vector of predicted values.
#' @param metrics vector of metrics to compute. See \code{\link{MLmetrics}} for
#'   a complete list of metrics that can be used
#' @import plyr MLmetrics
#' @export
#' @examples 
#' mod <- lm(mpg ~ ., data = mtcars)
#' pred <- predict(mod)
#' computeRegressionMetrics(mtcars$mpg, pred)
# TODOS: 
# 1. add an extra argument to let user customize definitions file.
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

#' Display regression metrics in a panel
#'
#' @inheritParams computeRegressionMetrics
#' @import plyr
#' @export
#' @examples
#' data(cars)
#' reg <- lm(log(dist) ~ log(speed), data = cars)
#' fdRegressionMetricsTable(
#'   actual = cars$dist,
#'   predicted = exp(reg$fitted.values)
#' ) %>%
#' fdPreview("Regression Metrics")
fdRegressionMetricsTable <- function(actual, predicted,
    metrics = c("MAE", "MAPE", "MedianAPE", "RMSE", "RMSLE", "RAE", "R2_Score")){
  metricsDF = computeRegressionMetrics(actual, predicted, metrics)
  l <- plyr::alply(metricsDF, 1, function(d){
    percentBars <- c('MAPE', 'R2_Score')
    fdStat(d$Abbreviation, d$Value, note = d$Metric,
      showBar = d$Abbreviation %in% percentBars
    )
  })
  do.call(tagList, l)
}

#' @export
fdMetricsPanel <- function(x){
  l <- plyr::alply(x, 1, function(d){
    fdStat(d$Abbreviation, d$Value, note = d$Metric, showBar = d$Scaled == "Yes")
  })
  do.call(tagList, l)
}

#' Scatterplot of actual vs. predicted values for a regression model
#' 
#' @param actual vector of actual values
#' @param predicted vector of predicted values
#' @export
#' @family regression
#' @examples 
#' fit <- lm(mpg ~ ., data = mtcars)
#' actual <- mtcars$mpg
#' predicted <- predict(fit)
#' fdRegressionScatterplot(actual, predicted) %>% fdPreview
fdRegressionScatterplot <- function(actual, predicted){
  actual <- unname(actual)
  predicted <- unname(predicted)
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
  
  plotlyLite(data, layout, config)
}

