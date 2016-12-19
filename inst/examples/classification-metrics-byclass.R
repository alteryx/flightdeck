library(rpart)
fit <- rpart(Species ~ ., data = iris)
actual <- iris$Species
predicted <- predict(fit, type = 'class')

computeClassificationMetricsByClass(actual = actual, predicted = predicted)

if (interactive()){
  fdPanelClassificationMetricsByClass(actual = actual, predicted = predicted) %>%
    fdPreview("Classification Metrics") 
}

