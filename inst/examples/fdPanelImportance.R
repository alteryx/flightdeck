library(rpart)
data(ptitanic, package = 'rpart.plot')
if (interactive()){
  rpart(survived ~ ., data = ptitanic) %>%
    fdPanelImportance %>%
    fdPreview
}
