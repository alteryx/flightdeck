library(ROCR)
data(ROCR.simple)
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels)
recall <- performance(pred, "rec")
precision <- performance(pred, "prec")
fscore <- performance(pred, "f")

d <- data.frame(
  threshold = recall@x.values[[1]],
  recall = recall@y.values[[1]],
  precision = precision@y.values[[1]],
  fscore = fscore@y.values[[1]]
)

d2 <- reshape2::melt(d, id = 'threshold')

library(ggplot2)
d <- ggplot(d2, aes(x = threshold, y = value)) +
  geom_line(aes(group = variable, color = variable))

library(plotly)
ggplotly(d)
