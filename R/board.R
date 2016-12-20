#' Create a dashboard
#'
#' This function creates a dashboard.
#' @param header A header created by \code{fdHeader}.
#' @param sidebar A sidebar created by \code{fdSidebar}.
#' @param body A body created by \code{fdBody}.
#' @param theme A string indicating theme to use for the dashboard.
#' @param fixed A boolean indicating if the dashboard should use a sticky header
#' @export
#' @family dashboard
#' @import htmltools
#' @example inst/examples/fdBoard.R
fdBoard <- function(header, sidebar, body, theme = 'skin-blue-light', 
    fixed = FALSE){
  content <- div(class = "wrapper", header, sidebar, body)
  class <- paste('hold-transition sidebar-mini', theme)
  if (fixed) class <- paste(class, 'fixed')
  dashboard <- tags$body(class = class, content)
  addDeps(dashboard, theme) %>% browsable
}


#' Create a page of the dashboard
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

#' Create the body of the dashboard.
#'
#' @export
fdBody <- function(...){
  div(class = "content-wrapper",
      tags$section(class = "content", ...)
  )
}
