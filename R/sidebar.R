#' Create the sidebar of the dashboard.
#'
#'
#' This creates a sidebar for the dashboard
#' @param ... Items to put in the sidebar.
#' @export
#' @seealso \code{\link{fdSidebarMenu}}
#' @example inst/examples/fdSidebar.R
fdSidebar <- function(..., sidebarWidth = NULL) {
  tags$aside(class = "main-sidebar fd-main-sidebar",
    addCustomCssForSidebarWidth(sidebarWidth),
    tags$section(class = "sidebar fd-sidebar", list(...))
  )
}

#' Create a menu in the sidebar
#' 
#' 
#' This creates a sidebar menu which mainly consists of
#' \code{\link{fdMenuItem}} and  \code{\link{fdMenuSubItem}}
#' @export
#' @param ... Items to put in the sidebar. It can include \code{\link{fdMenuSubItem}}
#' @family sidebar
fdSidebarMenu <- function(...){
  tags$ul(class = "sidebar-menu fd-sidebar-menu", ...)
}


#' @param text Text to show for the menu item.
#' @param icon Icon to display against the text.
#' @param pageName Id of the page that this menu item will activate
#' @param href Link address.
#' @export
#' @family sidebar
#' @rdname fdSidebarMenu
fdMenuItem <- function(text, ..., icon = NULL, pageName = NULL, href = "#") {
  subItems <- list(...)
  # If no subitems, return a pretty simple tag object
  if (length(subItems) == 0) {
    return(
      tags$li(
        a(href = href,
          `data-page` = pageName,
          icon,
          span(text)
        )
      )
    )
  }
  tags$li(class = "treeview",
    a(href = href,
      icon,
      span(text),
      fdIcon("angle-left", class = "pull-right")
    ),
    do.call(tags$ul, c(class = "treeview-menu", subItems))
  )
}


#' @export
#' @family sidebar
#' @rdname fdSidebarMenu
fdMenuSubItem <- function(text, href = NULL, pageName = NULL, 
    icon = fdIcon("angle-double-right")){
  tags$li(
    a(href = href, `data-page` = pageName,
      icon,
      text
    )
  )
}
