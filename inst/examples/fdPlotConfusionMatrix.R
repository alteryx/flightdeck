library(flightdeck)
library(rpart)
mod <- rpart(Species ~ ., data = iris)
outcome <- as.character(attr(mod, 'ylevels')[mod$y])
pred <- predict(mod, type = "class")
cmat <- as.data.frame.matrix(table(outcome, pred))

if (interactive()){
  fdRowBox(width = 6,
    fdPlotConfusionMatrix(cmat),
    title = 'Confusion Matrix',
    footer = paste(
      'Numbers in parantheses indicate % of observations', 
      'correctly classified. Hover for more details'
    ) 
  ) %>% 
  fdPreview(wrap = 'none')
}


