library(flightdeck)
library(htmltools)

#' Example 1: Empty Dashboard
myBoard1 <- fdBoard(
  fdHeader(),
  fdSidebar(),
  fdBody()
)

if (interactive()){
  myBoard1 
}

#' Example 2: Hello World!
myBoard2 <- fdBoard(
  fdHeader(title = 'My Board'),
  fdSidebar(),
  fdBody(
    h3("Hello World!")
  )
) 

if (interactive()){
  myBoard2 
}

#' Example 3: Fixed Header. The header stays on the page as you scroll down.
myBoard3 <- fdBoard(
  fdHeader(title = 'Test'),
  fdSidebar(),
  fdBody(div(style = 'height:900px;')),
  fixed = TRUE
)

if (interactive()){
  myBoard3 
}

#' Example 4: Long Titles
myBoard4 <- fdBoard(
  fdHeader(
    title = 'This is a really really really long title', 
    titleWidth = 300
  ),
  fdSidebar(sidebarWidth = 300),
  fdBody()
)


