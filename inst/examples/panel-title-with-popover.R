library(flightdeck)
library(htmltools)
title <- fdTitleWithPopover('Hello', note = 'This is to say hello')
fdBox(
    p("This is a really cool thing..."),
    title = title,
    footer = tags$small('Click on the info sign '),
    solidHeader = FALSE,
    status = 'danger'
  ) %>%
  fdPreview(wrapBox = F)
