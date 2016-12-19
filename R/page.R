#' Dashboard
#'
#' This creates a dashboard.
#' @param header A header created by \code{fdHeader}.
#' @param sidebar A sidebar created by \code{fdSidebar}.
#' @param body A body created by \code{fdBody}.
#' @param theme A string indicating theme to use for the dashboard.
#' @export
#' @family dashboard
#' @import htmltools
#' @examples 
#' library(flightdeck)
#' fdBoard(
#'   fdHeader(title = 'My Board'),
#'   fdSidebar(),
#'   fdBody()
#' )
fdBoard <- function(header, sidebar, body, theme = 'skin-blue-light'){
  content <- div(class = "wrapper", header, sidebar, body)
  dashboard <- tags$body(class=paste('hold-transition sidebar-mini', theme), content)
  addDeps(dashboard, theme) %>% browsable
}


#' Dashboard Page
#' 
#' This function creates a single page.
#' @param ... items to put on the page.
#' @param id id attribute for the page div.
#' @param display boolean indicating if the page is to be displayed by default.
#' @export
fdPage <- function(..., id, display = FALSE){
  cl <- c('page', 'fd-page')
  htmltools::div(..., 
    class = paste(cl, collapse = " "), 
    id = id,
    style = paste0('display:', if (display) 'block' else 'none', ';')
  )
}