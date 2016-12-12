library(flightdeck)
fdColors <- fdPalette()
coefTabColor = fdColors$olivegreen

library(htmltools)
library(DT)
data(diamonds, package = 'ggplot2')
mod <- lm(price ~ ., data = diamonds)


d3 <- fdCoefTable(mod, barColor = coefTabColor)

mod <- lm(mpg ~ ., data = mtcars)

page1 <- fdPage(id='page1', style='display:show;',
  fdRowBox(title = 'Variable Coefficients', d3, width = 12),
  fdRowBox(title = 'Variable Importance', d3, width = 12)
)

notes <- c(
  'Proportion of values predicted positive, that were actually positive',
  'Proportion of values actually positive, that were predicted positive',
  'Harmonic mean of Recall and Precision',
  'Proportion of correct predictions in the data'
)

metricsNoBar <- tagList(
  fdStat("Precision", 0.10, note = notes[1], showBar = F),
  fdStat("Recall", 0.90, note = notes[2], showBar = F),
  fdStat("F1-Score", 0.95, note = notes[3], showBar = F),
  fdStat("Accuracy", 0.80, note = notes[4], showBar = F)
)

metricsBar <- tagList(
  fdStat("Precision", 0.10, note = notes[1]),
  fdStat("Recall", 0.90, note = notes[2]),
  fdStat("F1-Score", 0.95, note = notes[3]),
  fdStat("Accuracy", 0.80, note = notes[4])
)


page2 <- fdPage(id='page2', style='display:none;',
  fdRow(
    fdBox(width = 12, title = 'Performance',
      fdColumn(6, metricsNoBar),
      fdColumn(6, metricsBar)
    ),
    fdResidualsPanel(mod, digits = 4)
  )
)

library(rpart)
data(ptitanic, package = 'rpart.plot')
fit1 = rpart(survived ~ ., data = ptitanic)
page3 <- fdPage(id='page3', style='display:none;',
  fdRowBox(width = 12, title = 'Decision Tree',
    AlteryxRviz::renderTree(
      fit1, colpal = unname(fdColors[c('ocean_blue','smoke_grey')])
    )
  )
)

d2 <- fdBoard(
  fdHeader(title = 'Decision Trees', miniTitle = 'DT'),
  fdSidebar(
    fdSidebarMenu(
      fdMenuItem("Interpretation", icon = fdIcon("table"), pageName = 'page1',
        fdMenuSubItem('Test', pageName = 'page1')
      ),
      fdMenuItem("Performance", icon = fdIcon("bar-chart"), pageName = 'page2'),
      fdMenuItem("Model Information", icon = fdIcon("eye"), pageName = 'page3')
    )
  ),
  fdBody(page1, page2, page3)
)

html_print(d2, background = NULL)
