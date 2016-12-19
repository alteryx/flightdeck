#' Example 1
if (interactive()){
  fdBoard(
    fdHeader(),
    fdSidebar(),
    fdBody()
  ) 
}

#' Example 2
if (interactive()){
  fdBoard(
    fdHeader(title = 'My Board'),
    fdSidebar(),
    fdBody(
      h3("Hello World!")
    )
  ) 
}
