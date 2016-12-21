#' Display a panel of diagnostic plots for regression models
#' 
#' @param mod a regression model object
#' @export
#' @examples 
#' mod <- lm(mpg ~ ., data = mtcars)
#' fdPanelRegressionDiagnostics(mod) %>% fdPreview(wrap = 'none')
fdPanelRegressionDiagnostics <- function(mod){
  plots = fdPlotsRegressionDiagnostics(mod)
  fdRowBox(width = 12, title = 'Regression Diagnostics Plots',
    fdRow(
      fdColumn(6, plots$residuals_vs_fitted),
      fdColumn(6, plots$scale_location)
    ),
    fdRow(
      fdColumn(6, plots$qq_plot),
      fdColumn(6, plots$residuals_vs_leverage)
    )
  ) 
}
#' @inheritParams fdPanelRegressionDiagnostics
#' @rdname fdPanelRegressionDiagnostics
#' @export
fdPlotsRegressionDiagnostics <- function(mod){
  d <- fdFortifyLm(mod)
  
  qq_plot <- fdQQPlotLm(d)
  
  residuals_vs_fitted <- fdScatterLoess('Fitted_Values', 'Residuals', d, 
    title = 'Residuals vs. Fitted Values'                                      
  )
  residuals_vs_leverage <- fdScatterLoess('Leverage', 'Standardized_Residuals', d,
    title = 'Residuals vs. Leverage'
  )
  scale_location <- fdScatterLoess('Fitted_Values', 'Root_Residuals', d,
    title = 'Scale-Location'                                 
  )
  list(
    residuals_vs_fitted = residuals_vs_fitted,
    scale_location = scale_location,
    qq_plot = qq_plot,
    residuals_vs_leverage = residuals_vs_leverage
  )
}



# Create a scatter with loess plot from data using \code{\link{fdPlotly}}
fdScatterLoess <- function(x, y, data, ...){
  loessFit <- loess.smooth(data[[x]], data[[y]])
  data_ <- list(
    list(
      x = data[[x]], 
      y = data[[y]], 
      type = "scatter", 
      mode = "markers", 
      hoverinfo = "x+y",
      name = "Data",
      marker = list(size = 10, opacity = 0.5), 
      showlegend = F
    ),
    list(
      x = loessFit$x, 
      y = loessFit$y, 
      type = "scatter", 
      mode = "line",
      name = "Smooth",
      line = list(width = 2),
      showlegend = F
    )
  )
  
  layout_ = list(
    #title = "Scale Location",
    plot_bgcolor = "#f6f6f6",
    margin = list(
      t = 60,
      l = 40,
      r = 30,
      b = 60
    ),
    xaxis = list(title = x),
    yaxis = list(title = y),
    ...
  )
  config = list(displaylogo = FALSE, displayModeBar = FALSE)
  fdPlotly(data_, layout_, config, height = 325)
}

# Compute useful numbers for regression diagnostics plots
fdFortifyLm <- function(mod){
  d <- ggplot2::fortify(mod)
  data.frame(
    Fitted_Values = d$.fitted,
    Residuals = d$.resid,
    Standardized_Residuals = d$.stdresid,
    Root_Residuals = sqrt(abs(d$.stdresid)),
    Theoretical_Quantiles = qqnorm(d$.resid, plot.it = FALSE)$x,
    Leverage = d$.hat
  )
}

# Create a QQ Plot from the results of \code{\link{fdFortifyLm}}
fdQQPlotLm <- function(x){
  data_ <- list(
    list(
      x = x$Theoretical_Quantiles,
      y = x$Standardized_Residuals,
      type = 'scatter',
      mode = 'markers',
      hoverinfo = "x+y",
      name = 'Data',
      marker = list(size = 10, opacity = 0.5),
      showlegend = F
    ),
    list(
      x = x$Theoretical_Quantiles, 
      y = x$Theoretical_Quantiles, 
      type = "scatter", 
      mode = "line", 
      name = "",
      line = list(width = 2),
      showlegend = FALSE
    )
  )
  
  layout_ <- list(
    title = 'QQ Plot',
    plot_bgcolor = "#f6f6f6",
    margin = list(
      t = 60,
      l = 40,
      r = 30,
      b = 60
    ),
    xaxis = list(title = 'Theoretical Quantiles'),
    yaxis = list(title = 'Standardize Residuals')
  )
  config = list(displaylogo = FALSE, displayModeBar = FALSE)
  fdPlotly(data_, layout_, config, height = 325)
}
