library(flightdeck)
if (interactive()){
  fdTabsetPanel(selected = 'Tab2',
    fdTabPanel('Tab1', h3("This is tab 1")),
    fdTabPanel('Tab2', h3("This is tab 2"))
  ) %>%
    fdColumn(width = 12) %>%
    fdPreview(wrap = 'row')
}



library(flightdeck)
d <- fdTabContent(
  fdTabContentItem(fdPlotGlmnet(fit, xvar = 'norm'), name = 'activity', active = TRUE),
  fdTabContentItem(fdPlotGlmnet(fit, xvar = 'lambda'), name = 'timeline'),
  fdTabContentItem(fdPlotGlmnet(fit, xvar = 'dev'), name = 'settings')
)

