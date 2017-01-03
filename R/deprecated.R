fdRegressionScatterplot <- function(actual, predicted){
  .Deprecated('fdPanelRegressionScatterplot')
  fdPanelRegressionScatterplot(actual, predicted)
}

fdRegressionMetricsTable <- function(actual, predicted, 
    metrics = c("MAE", "MAPE", "MedianAPE", "RMSE", "RMSLE", "RAE", "R2_Score")){
  .Deprecated('fdRegressionMetricsTable')
  fdPanelRegressionMetrics(actual, predicted)
}

plotlyLite <- function(data, layout = NULL, config = NULL, width = NULL, 
    height = NULL){
  .Deprecated('fdPlotly')
  fdPlotly(data, layout, config, width, height)
}

fdCoefTable <- function(mod, digits = 3, barColor = 'steelblue'){
  .Deprecated('fdPanelCoefficients')
  fdPanelCoefficients(mod, digits = digits, barColor = barColor)
}

fdVariableImportanceTable <- function(mod, digits = 3, barColor = 'steelblue'){
  .Deprecated('fdPanelImportance')
  fdPanelImportance(mod, digits = digits, barColor = barColor)
}
