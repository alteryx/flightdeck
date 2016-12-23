#' Make a tabset panel
#' 
#' @param ... elements to put in. see details.
#' @param selected id of the active tab
#' @export
fdTabsetPanel <- function(..., selected = NULL){
  tabContent <- list(...)
  tabNav <- lapply(tabContent, function(tp){
    fdTabNavItem(
      tp$attribs$`data-title`, 
      id = tp$attribs$`data-id`,
      active = (tp$attribs$`data-id` == selected)
    )
  })
  tabContent <- lapply(tabContent, function(d){
    if (d$attribs$`data-id` == selected) {
      d$attribs$class <- paste(d$attribs$class, 'active')
    }
    return(d)
  })
  tags$div(class = 'nav-tabs-custom',
    do.call(fdTabNav, tabNav),
    do.call(fdTabContent, tabContent)
  )
}

#' Make a tab panel
#' 
#' @inheritParams fdTabsetPanel
#' @rdname fdTabsetPanel
#' @param ... elements to put in the panel
#' @export
fdTabPanel <- function(title, ..., id = title){
  if (is.null(id)){
    stop('A tab pane needs to have a name.')
  }
  tags$div(..., `data-title` = title, `data-id` = id,
    id = paste0('fd-tab-', id),
    class = "tab-pane fd-tab-pane"
  )
}

#' @inheritParams fdTabsetPanel
#' @rdname fdTabsetPanel
#' @export
fdTabNav <- function(...){
  tags$ul(class = 'nav nav-tabs', ...)
}

#' @inheritParams fdTabsetPanel
#' @rdname fdTabsetPanel
#' @export
fdTabContent <- function(...){
  tags$div(class = 'tab-content', ...)
}

#' @inheritParams fdTabsetPanel
#' @rdname fdTabsetPanel
#' @param name unique name to identify tab
#' @param active boolean indicating if tab is active
#' @export
fdTabNavItem <- function(..., id = NULL, active = FALSE){
  if (is.null(id)){
    stop('A tab pane needs to have a name.')
  }
  tags$li(class = if (active) "active" else "", tags$a(...,
     href = paste0("#fd-tab-", id), 
    `data-toggle` = "tab",
    `aria-expanded` = "true"   
  ))
}


#' @inheritParams fdTabsetPanel
#' @rdname fdTabsetPanel
#' @export
fdTabContentItem <- function(..., id = NULL, active = FALSE){
  if (is.null(id)){
    stop('A tab pane needs to have a name.')
  }
  active = if (active) "active" else ""
  tags$div(...,
    class = paste("tab-pane fd-tab-pane", active),
    id = paste0('fd-tab-', id)
  )
}
