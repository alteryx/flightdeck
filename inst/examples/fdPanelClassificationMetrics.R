library(flightdeck)
library(rpart)
data(ptitanic, package = 'rpart.plot')
fit <- rpart(survived ~ ., data = ptitanic)
actual <- ptitanic$survived
predicted <- predict(fit, type = 'class')

computeClassificationMetrics(actual = actual, predicted = predicted)

if (interactive()){
  fdPanelClassificationMetrics(actual = actual, predicted = predicted) %>%
    fdPreview("Classification Metrics") 
}


