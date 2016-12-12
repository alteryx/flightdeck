#' Dashboard header
#'
#' @param ... Items to put in the header.
#' @param title The title to show in the header bar.
#' @param miniTitle The title to show in the header bar when sidebar is collapsed.
#' @param .list An optional list containing items to put in the header. Same as
#'   the \code{...} arguments, but in list format.
#' @export
fdHeader <- function(..., title = NULL, miniTitle = NULL, .list = NULL) {
  items <- c(list(...), .list)
  tags$header(class = "main-header",
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
