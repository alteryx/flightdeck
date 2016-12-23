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
  
  # Putting everything in a tabset panel
  fdTabsetPanel(selected = 'L1 Norm',
    fdTabPanel('L1 Norm', fdPlotGlmnet(fit, xvar = 'norm')),
    fdTabPanel('Lambda', fdPlotGlmnet(fit, xvar = 'lambda')),
    fdTabPanel('Deviance', fdPlotGlmnet(fit, xvar = 'dev'))
  ) %>%
    fdColumn(width = 12) %>%
    fdPreview(wrap = 'row')
}

