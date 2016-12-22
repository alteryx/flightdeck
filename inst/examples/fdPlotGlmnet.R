library(glmnet)
library(flightdeck)
data(QuickStartExample)
fit <- glmnet(x, y)

if (interactive()){
  #' Interctive profiles plot
  fdPlotGlmnet(fit)
  
  # Use lambda as xvar
  fdPlotGlmnet(fit, xvar = 'lambda')
  
  # Don't show legend
  fdPlotGlmnet(fit, showlegend = FALSE)
}
