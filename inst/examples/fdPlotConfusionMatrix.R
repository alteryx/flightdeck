library(flightdeck)
library(rpart)
mod <- rpart(Species ~ ., data = iris)
outcome <- as.character(attr(mod, 'ylevels')[mod$y])
pred <- predict(mod, type = "class")

# Example 1: Table
ctable <- table(outcome, pred)

if (interactive()){
  fdRowBox(width = 6,
    fdPlotConfusionMatrix(ctable),
    title = 'Confusion Matrix',
    footer = paste(
      'Numbers in parantheses indicate % of observations', 
      'correctly classified. Hover for more details'
    ) 
  ) %>% 
  fdPreview(wrap = 'none')
}

# Example 2: Matrix
cmat2 <- matrix(data = c(63, 3, 4, 33), nrow = 2, ncol = 2,
  dimnames = list(
    c('Actual Positive', 'Actual Negative'),
    c('Predicted Positive', 'Predicted Negative')
  )
)

fdPlotConfusionMatrix(cmat2) %>%
  fdPreview



