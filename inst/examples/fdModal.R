library(flightdeck)
library(htmltools)

# Tooltlip
myIcon <- fdIcon("info-sign", lib = 'glyphicon') 
tooltipData <- list(
  message = 'This plot shows if residuals are normally distributed.', 
  title = 'Residuals', 
  placement = 'right'
)

box1 <- fdBox(title = span("Tooltip", myIcon %>% fdTooltip(.list = tooltipData))) 
box2 <-  fdBox(title = span("Popover", myIcon %>% fdPopover(.list = tooltipData)))
box3 <- fdBox(title = span("Modal", myIcon %>% fdModal(.list = tooltipData)))
myBoxes <- tagList(box1, box2, box3) 
if (interactive()){
  fdPreviewBoard(myBoxes, 'Modal, Tooltip and Popovers', wrapBox = F)
}
  


# Example 2
title <- span('Hello', 
  fdIcon('info-circle') %>%
    fdPopover(
      message = 'Welcome to flighdeck! The best dashboarding package ever.', 
      title = 'Hello'
    )
)
if (interactive()){
  fdBox(
    p("This is a really cool thing..."),
    title = title,
    footer = tags$small('Click on the info sign to view help.'),
    solidHeader = FALSE,
    status = 'danger'
  ) %>%
    fdPreview(wrapBox = F)
}
