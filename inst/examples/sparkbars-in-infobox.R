viewSource <- tags$a(fdIcon('code'), 'Source Code') %>%
  fdModal('Hello', title = 'Adjusted R-Squared') %>%
  tags$li()

fdBoard(
  fdHeader(viewSource), fdSidebar(), fdBody()
)

#' Sparkbars in InfoBox
sparkBar <- div(class = 'sparkbar', 
  sparkline::sparkline(rpois(5, 20), type = 'bar', barColor = 'white', 
    barWidth = 60/5, 
    height = 40
  ),
  tags$style('.sparkbar canvas{vertical-align:baseline !important;')
)
fdInfoBox('Adjusted R-Squared', 20, icon = sparkBar) %>%
  fdPreview(wrapBox = F)

tagList(
  fdColumn(width = c(xs = 2), span(class = 'info-box-icon bg-aqua', sparkBar)),
  fdColumn(width = c(xs = 2), span(class = 'info-box-icon bg-green', sparkBar))
) %>%
  fdPreview(wrapBox = T)