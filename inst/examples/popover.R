library(flightdeck)
library(htmltools)
title <- span('Hello', 
  fdIcon('info-circle') %>%
    fdPopover(
      message = 'Welcome to flighdeck! The best dashboarding package ever.', 
      title = 'Hello'
    )
)
fdBox(
    p("This is a really cool thing..."),
    title = title,
    footer = tags$small('Click on the info sign '),
    solidHeader = FALSE,
    status = 'danger'
  ) %>%
  fdPreview(wrapBox = F)
