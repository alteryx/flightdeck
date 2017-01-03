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

# Example 3A: Panel of Plots
dat <- iris
dat$Species <- as.factor(dat$Species)
eachPlotWidth = 6
numericColumns <- names(dat)[sapply(dat, is.numeric)]
# Facets in a Single Panel
cdPlots <- lapply(numericColumns, function(x){
  plt <- fdPlotConditionalDensity(x = dat[[x]], y = dat[['Species']], 
    xlab = x, showlegend = FALSE
  )
  fdColumn(plt, width = eachPlotWidth)
})

fdRow(cdPlots) %>% 
  fdPreviewBoard(title = 'Conditional Density Plots', wrap = 'box')

# Tabbed Panel
cdPlots <- lapply(numericColumns, function(x){
  plt <-  fdPlotConditionalDensity(x = dat[[x]], y = dat[['Species']], 
    xlab = x, showlegend = FALSE
  )
  fdTabPanel(plt, title = x)
})

fdTabsetPanel(selected = makeHtmlId('Sepal.Length'), .list = cdPlots) %>%
  fdColumn(width = 12) %>%
  fdPreview(wrap = 'row')
