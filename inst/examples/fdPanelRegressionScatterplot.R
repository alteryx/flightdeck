library(flightdeck)
fit <- lm(mpg ~ ., data = mtcars)
actual <- mtcars$mpg
predicted <- predict(fit)
if (interactive()){
  fdPanelRegressionScatterplot(actual, predicted) %>% 
    fdPreview
}
