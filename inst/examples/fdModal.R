library(flightdeck)
library(htmltools)

# Tooltlip
myIcon <- fdIcon("info-sign", lib = 'glyphicon') 
tooltipData <- list(message = 'This plot shows if residuals are normally distributed. Do residuals follow a straight line well or do they deviate severely? It’s good if residuals are lined well on the straight dashed line.', title = 'Residuals', placement = 'right')

box1 <- fdBox(title = span("Tooltip", myIcon %>% fdTooltip(.list = tooltipData))) 
box2 <-  fdBox(title = span("Popover", myIcon %>% fdPopover(.list = tooltipData)))
box3 <- fdBox(title = span("Modal", myIcon %>% fdModal(.list = tooltipData)))
tagList(box1, box2, box3) %>%
  fdPreviewBoard('Modal, Tooltip and Popovers', wrapBox = F)

