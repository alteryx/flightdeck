#' Create the header of the dashboard
#'
#' @param ... Elements to in the dashboard header.
#' @param title The title to display in the header bar.
#' @param miniTitle The title to display in the header bar when the sidebar is
#'   collapsed.
#' @param titleWidth The width of the title in pixels.
#' @param .list An optional list containing elements to put in the header. These
#'   are the identical to the \code{...} arguments, but provided in a list
#'   format. This is useful when the elements going into the header are
#'   programatically generated using \code{lapply} calls
#' @export
#' @example inst/examples/fdHeader.R
fdHeader <- function(..., title = NULL, miniTitle = NULL, titleWidth = NULL, 
    .list = NULL) {
  items <- c(list(...), .list)
  tags$header(class = "main-header",
    addCustomCssForTitle(titleWidth),
    tags$a(href="#", class='logo',
      if (!is.null(miniTitle)) span(class = "logo-mini", miniTitle),
      span(class = "logo-lg", title)
    ),
    tags$nav(class = "navbar navbar-static-top", role = "navigation",
      # Embed hidden icon so that we get the font-awesome dependency
      span(fdIcon("bars"), style = "display:none;"),
      # Sidebar toggle button
      a(href="#", class="sidebar-toggle", `data-toggle`="offcanvas",
        role="button", span(class="sr-only", "Toggle navigation")
      ),
      div(class = "navbar-custom-menu",
        tags$ul(class = "nav navbar-nav",
          items
        )
      )
    )
  )
}

#' Create a dropdown menu
#'
#' @param type The type of menu. Should be one of "messages", "notifications",
#'   "tasks".
#' @param badgeStatus The status of the badge which displays the number of items
#'   in the menu. This determines the badge's color. A value of \code{NULL}
#'   means to not display a badge.
#' @param ... Items to put in the menu. Typically, message menus should contain
#'   \code{\link{fdMessage}}s, notification menus should contain
#'   \code{\link{fdNotification}}s.
#' @param icon An icon to display in the header. By default, the icon is
#'   automatically selected depending on \code{type}, but it can be overriden
#'   with this argument.
#' @param .list An optional list containing items to put in the menu Same as the
#'   \code{...} arguments, but in list format. This can be useful when working
#'   with programmatically generated items.
#'
#' @seealso \code{\link{fdHeader}} for example usage.
#'
#' @export
#' 
fdDropdownMenu <- function(...,
  type = c("messages", "notifications", "tasks"),
  badgeStatus = "primary", icon = NULL, .list = NULL){
  type <- match.arg(type)
  # if (!is.null(badgeStatus)) validateStatus(badgeStatus)
  items <- c(list(...), .list)

  # Make sure the items are li tags
  # lapply(items, tagAssert, type = "li")

  dropdownClass <- paste0("dropdown ", type, "-menu")

  if (is.null(icon)) {
    icon <- switch(type,
      messages = fdIcon("envelope-o"),
      notifications = fdIcon("warning"),
      tasks = fdIcon("tasks")
    )
  }

  numItems <- length(items)
  if (is.null(badgeStatus)) {
    badge <- NULL
  } else {
    badge <- span(class = paste0("label label-", badgeStatus), numItems)
  }

  tags$li(class = dropdownClass,
    a(href = "#", class = "dropdown-toggle", `data-toggle` = "dropdown",
      icon,
      badge
    ),
    tags$ul(class = "dropdown-menu",
      tags$li(class = "header", paste("You have", numItems, type)),
      tags$li(
        tags$ul(class = "menu",
          items
        )
      )
      # TODO: This would need to be added to the outer ul
      # tags$li(class = "footer", a(href="#", "View all"))
    )
  )

}

#' Create a message
#'
#' @param from Who the message is from.
#' @param message Text of the message.
#' @param icon An icon tag, created by \code{\link{fdIcon}}.
#' @param time String representing the time the message was sent. Any string may
#'   be used. For example, it could be a relative date/time like "5 minutes",
#'   "today", or "12:30pm yesterday", or an absolute time, like "2014-12-01 13:45".
#'   If NULL, no time will be displayed.
#' @param href An optional URL to link to.
#'
#' @family menu items
#' @seealso \code{\link{fdHeader}} for example usage.
#' @export
#' @examples
#' myMenu <- fdDropdownMenu(type = "messages",
#'   fdMessage(
#'     from = "Sales Dept",
#'     message = "Sales are steady this month."
#'   ),
#'   fdMessage(
#'     from = "New User",
#'     message = "How do I register?",
#'     icon = fdIcon("question"),
#'     time = "13:45"
#'   ),
#'   fdMessage(
#'     from = "Support",
#'     message = "The new server is ready.",
#'     icon = fdIcon("life-ring"),
#'     time = "2014-12-01"
#'   )
#' )
#' fdBoard(fdHeader(myMenu), fdSidebar(), fdBody())
fdMessage <- function(from, message, icon = fdIcon("user"), time = NULL,
  href = NULL){
  # tagAssert(icon, type = "i")
  if (is.null(href)) href <- "#"
  
  tags$li(
    a(href = href,
      icon,
      h4(
        from,
        if (!is.null(time)) tags$small(fdIcon("clock-o"), time)
      ),
      p(message)
    )
  )
}

#' Create a notification
#'
#' @param text The notification text.
#' @param icon An icon tag, created by \code{\link{fdIcon}}.
#' @param status The status of the item This determines the item's background
#'   color.
#' @param href An optional URL to link to.
#'
#' @family menu items
#' @export
fdNotification <- function(text, icon = fdIcon("warning"), status = "success", 
    href = NULL){
  # tagAssert(icon, type = "i")
  # validateStatus(status)
  if (is.null(href)) href <- "#"
  
  # Add the status as another HTML class to the icon
  icon <- tagAppendAttributes(icon, class = paste0("text-", status))
  
  tags$li(
    a(href = href, icon, text)
  )
}

addCustomCssForTitle <- function(width){
  titleWidth <- htmltools::validateCssUnit(width)
  custom_css <- NULL
  cssTemplate <- "@media (min-width: 768px) {
     .main-header > .navbar {
      margin-left: _WIDTH_;
    }
    .main-header .logo {
      width: _WIDTH_;
    }
   }"
  if (!is.null(titleWidth)) {
    custom_css <- tags$head(
      tags$style(HTML(
        gsub("_WIDTH_", titleWidth, cssTemplate, fixed = TRUE)
      ))
    )
  }
  return(custom_css)
}

addCustomCssForSidebarWidth <- function(width){
  sidebarWidth <- htmltools::validateCssUnit(width)
  custom_css <- NULL
  cssTemplate <- "   
      @media (min-width: 768px) {
        .content-wrapper,
        .right-side,
        .main-footer {
          margin-left: _WIDTH_;
        }
        .main-sidebar,
        .left-side {
          width: _WIDTH_;
        }
      }
      @media (max-width: 767px) {
        .sidebar-open .content-wrapper,
        .sidebar-open .right-side,
        .sidebar-open .main-footer {
          -webkit-transform: translate(_WIDTH_, 0);
          -ms-transform: translate(_WIDTH_, 0);
          -o-transform: translate(_WIDTH_, 0);
          transform: translate(_WIDTH_, 0);
        }
      }
      @media (max-width: 767px) {
        .main-sidebar,
        .left-side {
          -webkit-transform: translate(-_WIDTH_, 0);
          -ms-transform: translate(-_WIDTH_, 0);
          -o-transform: translate(-_WIDTH_, 0);
          transform: translate(-_WIDTH_, 0);
        }
      }
      @media (min-width: 768px) {
        .sidebar-collapse .main-sidebar,
        .sidebar-collapse .left-side {
          -webkit-transform: translate(-_WIDTH_, 0);
          -ms-transform: translate(-_WIDTH_, 0);
          -o-transform: translate(-_WIDTH_, 0);
          transform: translate(-_WIDTH_, 0);
        }
      }
    ')"
  if (!is.null(sidebarWidth)) {
    custom_css <- tags$head(
      tags$style(HTML(
        gsub("_WIDTH_", sidebarWidth, cssTemplate, fixed = TRUE)
      ))
    )
  }
  return(custom_css)
}
