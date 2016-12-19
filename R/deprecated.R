#' @export
fdRegressionScatterplot <- function(actual, predicted){
  .Deprecated('fdPanelRegressionScatterplot')
  fdPanelRegressionScatterplot(actual, predicted)
}

#' @export
fdRegressionMetricsTable <- function(actual, predicted, 
    metrics = c("MAE", "MAPE", "MedianAPE", "RMSE", "RMSLE", "RAE", "R2_Score")){
  .Deprecated('fdRegressionMetricsTable')
  fdPanelRegressionMetrics(actual, predicted)
}

#' @export
plotlyLite <- function(data, layout = NULL, config = NULL, width = NULL, 
    height = NULL){
  .Deprecated('fdPlotly')
  fdPlotly(data, layout, config, width, height)
}

#' @export
fdCoefTable <- function(mod, digits = 3, barColor = 'steelblue'){
  .Deprecated('fdPanelCoefficients')
  fdPanelCoefficients(mod, digits = digits, barColor = barColor)
}

#' @export
fdVariableImportanceTable <- function(mod, digits = 3, barColor = 'steelblue'){
  .Deprecated('fdPanelImportance')
  fdPanelImportance(mod, digits = digits, barColor = barColor)
}
