library(flightdeck)
# Example 1: NASA space shuttle o-ring failures
fail <- factor(c(2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1,
  1, 2, 1, 1, 1, 1, 1),
  levels = 1:2, labels = c("no", "yes")
)
temperature <- c(
  53, 57, 58, 63, 66, 67, 67, 67, 68, 69, 70, 70,
  70, 70, 72, 73, 75, 75, 76, 76, 78, 79, 81
)
# Conditional Density Plot
# cdplot(fail ~ temperature)
if (interactive()){
  fdPlotConditionalDensity(temperature, fail)
}

# Example 2: Iris
if (interactive()){
  fdPlotConditionalDensity(iris$Petal.Length, iris$Species, xlab = 'Petal.Length')
}
