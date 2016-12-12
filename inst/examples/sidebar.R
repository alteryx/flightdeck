library(flightdeck)
fdBoard(
  fdHeader(title='Sidebar'),
  fdSidebar(
    fdSidebarMenu(
      fdMenuItem("Page1",icon=fdIcon("table"),
       fdMenuSubItem('Page1A',pageName='page1')
      ),
      fdMenuItem("Page2",icon=fdIcon("bar-chart"),pageName='page2')
    ) 
  ),
  fdBody(
    fdPage(p('Page 1'),id='page1', display = TRUE),
    fdPage(p('Page 2'),id='page2')
  )
)