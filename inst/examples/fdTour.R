library(flightdeck)
library(htmltools)

#' Example 1
fit <- rpart::rpart(Species ~ ., data = iris)
actual <- iris$Species
predicted <- predict(fit, type = 'class')

## Clicking on the Alteryx title will trigger an interactive tour of the page.
tour = fdTour(list(
  list(intro = 'Decision Tree'),
  list(
    intro = "This panel displays class-wise performance metrics.",
    element = htmlwidgets::JS("document.querySelector('.box')"),
    numberPosition = "top-right",
    tooltipPosition = "right"
  )
), list(), '.logo-lg', width = 90, height = 30)


fdPanelClassificationMetrics(actual, predicted) %>%
  fdPreviewBoard(title = 'Classification Performance', tour, wrap = 'rowbox')
