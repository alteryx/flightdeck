library(MLmetrics)
library(flightdeck)
data(cars)
reg <- lm(log(dist) ~ log(speed), data = cars)

fdRegressionMetricsTable(
  actual = cars$dist,
  predicted = exp(reg$fitted.values)
) %>%
  fdPreview




