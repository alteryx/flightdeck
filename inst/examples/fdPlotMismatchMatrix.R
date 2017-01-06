library(rpart)
mod <- rpart(Species ~ ., data = iris)
predicted <- predict(mod, type = 'class')
actual <- iris$Species

if (interactive()){
  confusion <- table(actual, predicted)
  fdBox(title = 'Classification Mismatch',
    fdPlotMismatchMatrix(confusion),
    footer = 'This table shows the top misclassification pairs',
    width = 12
  ) %>%
    fdPreview(wrap = 'row')
}


# library(mlbench)
# mod <- rpart(Class ~ ., data = Soybean)
# predicted <- predict(mod, type = 'class')
# actual <- Soybean$Class
# confusion <- table(actual, predicted)
# 
# htmltools::tagList(
#   fdRowBox(width = 12, title = 'Confusion Matrix', 
#     fdPlotConfusionMatrix(confusion), 
#     extraBoxClass = 'table-responsive'),
#   fdRowBox(width = 12, title = 'Mismatch Matrix', 
#     fdPlotClassificationMismatch(confusion),
#     footer = 'This table shows pairs of actual and predicted values sorted by their frequency of mismatches'
#   )
# ) %>%
#   fdPreview(wrap = 'none')
