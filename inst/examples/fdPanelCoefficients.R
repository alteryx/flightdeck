library(flightdeck)
data(diamonds, package = 'ggplot2')
if (interactive()){
  lm(price ~ ., data = diamonds) %>%
    fdPanelCoefficients %>%
    fdPreview  
}

