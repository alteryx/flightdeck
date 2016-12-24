library(flightdeck)

#' Example 1: Linear Regression Model
data(diamonds, package = 'ggplot2')
panel1 <- lm(price ~ ., data = diamonds) %>%
  fdPanelCoefficients %>%
  fdPreview  

if (interactive()) {
  panel1
}

#' Example 2: GlmNet Model
library(glmnet)
data(QuickStartExample)
panel2 <- glmnet(x, y) %>%
  fdPanelCoefficients(s = 0.3) %>%
  fdPreview

if (interactive()){
  panel2
}
