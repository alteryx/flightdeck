library(flightdeck)
fit <- lm(mpg ~ ., data = mtcars)
actual <- mtcars$mpg
predicted <- predict(fit)
regPerformance <- 'This is a note on how to interpret this panel'
title <- span('Regression Model Performance', 
  fdIcon('info-circled', lib = 'entypo') %>%
    fdModal(regPerformance, title = 'Model Performance')
)
fdBox(width = 12, title = title,
  fdColumn(8, fdRegressionScatterplot(actual, predicted)),
  fdColumn(4, fdRegressionMetricsTable(actual, predicted))
) %>%
  fdPreview(wrapBox = F)
