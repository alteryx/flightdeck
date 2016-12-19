myMenu <- fdDropdownMenu(type = "messages",
  messageItem(
    from = "Sales Dept",
    message = "Sales are steady this month."
  ),
  messageItem(
    from = "New User",
    message = "How do I register?",
    icon = fdIcon("question"),
    time = "13:45"
  ),
  messageItem(
    from = "Support",
    message = "The new server is ready.",
    icon = fdIcon("life-ring"),
    time = "2014-12-01"
  )
)


myNotifications <- fdDropdownMenu(type = "notifications",
  fdNotificationItem(
    text = "5 new users today",
    fdIcon("users")
  ),
  fdNotificationItem(
    text = "12 items delivered",
    fdIcon("truck"),
    status = "success"
  ),
  fdNotificationItem(
    text = "Server load at 86%",
    icon = fdIcon("exclamation-triangle"),
    status = "warning"
  )
)

fdBoard(
  fdHeader(myMenu, myNotifications), fdSidebar(), fdBody()
)
