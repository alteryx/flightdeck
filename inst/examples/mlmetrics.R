library(MLmetrics)
data(cars)
reg <- lm(log(dist) ~ log(speed), data = cars)
actual <- cars$dist
predicted <- exp(reg$fitted.values)
if (interactive()){
  fdRegressionMetricsTable(actual, predicted) %>%
    fdPreview("Regression Performance Metrics")
}


tf <- tempfile(fileext = '.html')
computeRegressionMetrics(actual, predicted) %>%
  fdMetricsPanel() %>%
  fdPreview("Regression Performance Metrics...") %>%
  save_html(file = tf) %>%
  webshot::webshot('~/Desktop/foo.png', selector = '.box')

data(ptitanic, package = 'rpart.plot')
fit <- rpart::rpart(survived ~ ., data = ptitanic)
actual <- ptitanic$survived
predicted <- predict(fit, type = 'class')
computeClassificationMetrics(actual, predicted) %>%
  fdMetricsPanel %>%
  fdPreview("Classification Performance Metrics")

fdClassificationPerformance(actual, predicted) %>%
  fdPreview

library(webshot)

computeClassificationMetricsByClass(actual, predicted) %>%
  fdClassificationPerformance2() %>%
  fdSimpleTable('table') %>%
  fdPreview("Classification Performance Measures") %>%
  webshot('~/Desktop/classificationPerformance.png')
