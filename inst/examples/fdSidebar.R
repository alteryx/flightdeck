library(flightdeck)
library(htmltools)

# Set up menu for dashboard sidebar
mySidebar <- fdSidebarMenu(
  fdMenuItem("Page1", icon = fdIcon("table"), pageName = 'page1'),
  fdMenuItem("Page2", icon = fdIcon("bar-chart"), pageName = 'page2')
)

# Set up pages for dashboad body
myPages <- list(
  fdPage(p('Page 1'), id = 'page1', display = TRUE),
  fdPage(p('Page 2'), id = 'page2')
)

## Render the dashboard only in interactive R sessions
if (interactive()) {
  fdBoard(
    fdHeader(title = 'My Dashboard'),
    fdSidebar(fdSidebarMenu(mySidebar)),
    fdBody(myPages)
  )
}
