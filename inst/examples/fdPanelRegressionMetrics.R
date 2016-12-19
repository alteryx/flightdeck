library(flightdeck)
data(cars)
reg <- lm(log(dist) ~ log(speed), data = cars)
actual <- cars$dist
predicted <- exp(reg$fitted.values)
fdPanelRegressionMetrics(actual, predicted) %>%
  fdPreview("Regression Metrics")
