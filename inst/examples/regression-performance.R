library(flightdeck)
fit <- lm(mpg ~ ., data = mtcars)
actual <- mtcars$mpg
predicted <- predict(fit)
fdBox(width = 12, title = 'Regression Model Performance',
  fdColumn(8, fdRegressionScatterplot(actual, predicted)),
  fdColumn(4, fdRegressionMetricsTable(actual, predicted))
) %>%
  fdPreview(wrapBox = F)
