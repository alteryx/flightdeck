#' Create a modal, popover or tooltip
#' 
#' 
#' This functions lets the user add a modal, popover or tooltip to a html
#' element that will be displayed on click or hover.
#' @param x html element
#' @param message message to dispaly in modal
#' @param title title to display in modal
#' @param ... additional arguments to pass to bootbox/bootstrap. See details.
#' @param .list options as a list. makes it easier with programmatic generation.
#' @import commonmark jsonlite
#' @export
#' @example inst/examples/fdModal.R
fdModal <- function(x, message, title = NULL, ..., .list = NULL){
  scr <- "$('%s').on('click', function(e){bootbox.alert(%s)})"
  if (is.null(x$attribs$id)) x$attribs$id <- paste0('modal-', rpois(1, 10000))
  x$attribs$class <- paste(x$attribs$class, ' fd-clickable')
  payload <- if (is.null(.list)){
    list(
      message = commonmark::markdown_html(message),
      title = title,
      ...
    )
  } else {
    .list$message = commonmark::markdown_html(.list$message)
    .list
  }
  tagList(x, tags$script(HTML(sprintf(scr, 
    paste0("#", x$attribs$id),
    jsonlite::toJSON(payload, auto_unbox = TRUE)
  ))))
}

#' @inheritParams fdModal
#' @rdname fdModal 
#' @export
fdPopover <- function(x, message, title = NULL, ..., .list = NULL){
  scr <- "$('%s').popover(%s)"
  if (is.null(x$attribs$id)) x$attribs$id <- paste0('modal-', rpois(1, 10000))
  x$attribs$class <- paste(x$attribs$class, ' fd-clickable')
  payload <- if (is.null(.list)){
    list(
      content = commonmark::markdown_html(message),
      title = title,
      html = TRUE,
      ...
    )
  } else {
    .list$content <- commonmark::markdown_html(.list$message)
    .list$message <- NULL
    .list$html <- TRUE
    .list
  }
  tagList(x, tags$script(HTML(sprintf(scr, 
    paste0("#", x$attribs$id),
    jsonlite::toJSON(payload, auto_unbox = TRUE)
  ))))
}

#' @inheritParams fdModal
#' @rdname fdModal 
#' @export
fdTooltip <- function(x, message, title = NULL, ..., .list = NULL){
  scr <- "$('%s').tooltip(%s)"
  if (is.null(x$attribs$id)) x$attribs$id <- paste0('modal-', rpois(1, 10000))
  payload <- if (is.null(.list)){
    list(
      content = commonmark::markdown_html(message),
      title = title,
      html = TRUE,
      ...
    )
  } else {
    .list$content <- commonmark::markdown_html(.list$message)
    .list$message <- NULL
    .list$html <- TRUE
    .list
  }
  tagList(x, tags$script(HTML(sprintf(scr, 
    paste0("#", x$attribs$id),
    jsonlite::toJSON(payload, auto_unbox = TRUE)
  ))))
}
