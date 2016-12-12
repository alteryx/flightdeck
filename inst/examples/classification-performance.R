library(flightdeck)

#' Example 1
fit <- rpart::rpart(Species ~ ., data = iris)
pred <- predict(fit, type = 'class')

tour = fdTour(list(
  list(intro = 'Decision Tree'),
  list(
    intro = "This panel displays class-wise performance metrics.",
    element = htmlwidgets::JS("document.querySelector('.box')"),
    numberPosition = "top-right",
    tooltipPosition = "right"
  )
), list(), '.logo-lg', width = 90, height = 30)


fdClassificationPerformance(
  actual = iris$Species,
  pred = predict(fit, type = 'class')
) %>%
  fdPreview(title = 'Classification Performance', wrapBox = T, tour)


#' Example 2
data(cars)
logreg <- glm(formula = vs ~ hp + wt, family = binomial(link = "logit"),
  data = mtcars)
pred <- ifelse(logreg$fitted.values < 0.5, 0, 1)

fdClassificationPerformance(mtcars$vs, pred) %>%
  fdPreview(
    fdTitleWithPopover(
      'Classification Performance Metrics',
      note = 'This is a help note'
    )
  )




