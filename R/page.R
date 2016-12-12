#' Dashboard
#'
#' This creates a dashboard.
#' @param header A header created by \code{fdHeader}.
#' @param sidebar A sidebar created by \code{fdSidebar}.
#' @param body A body created by \code{fdBody}.
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
fdBoard <- function(header, sidebar, body){
  content <- div(class = "wrapper", header, sidebar, body)
  dashboard <- tags$body(class='hold-transition skin-blue sidebar-mini', content)
  addDeps(dashboard) %>% browsable
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