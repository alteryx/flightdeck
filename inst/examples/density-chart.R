# Density Charts for Classification
library(rpart)
m <- rpart(Species ~ ., data = iris)
p <- apply(predict(m), 1, max)
d <- data.frame(x = p, g = iris$Species)


library(ggplot2)
ggplot(d, aes(x = x, group = g, fill = g)) +
  geom_density(alpha = 0.3)
