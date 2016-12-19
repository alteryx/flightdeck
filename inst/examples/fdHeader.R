library(htmltools)
myMenu <- fdDropdownMenu(type = "messages",
  fdMessage(
    from = "Sales Dept",
    message = "Sales are steady this month."
  ),
  fdMessage(
    from = "New User",
    message = "How do I register?",
    icon = fdIcon("question"),
    time = "13:45"
  ),
  fdMessage(
    from = "Support",
    message = "The new server is ready.",
    icon = fdIcon("life-ring"),
    time = "2014-12-01"
  )
)


myNotifications <- fdDropdownMenu(type = "notifications",
  fdNotification(
    text = "5 new users today",
    icon = fdIcon("users")
  ),
  fdNotification(
    text = "12 items delivered",
    icon = fdIcon("truck"),
    status = "success"
  ),
  fdNotification(
    text = "Server load at 86%",
    icon = fdIcon("exclamation-triangle"),
    status = "warning"
  )
)
myHeader <- tagList(myMenu, myNotifications)

if (interactive()){
  fdBoard(
    fdHeader(title = 'Header Menu', myHeader), 
    fdSidebar(), 
    fdBody()
  )
}
