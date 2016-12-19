#' Preview a widget in the dashboard.
#'
#'
#' This function helps preview a widget in the dashboard with or without the
#' header and sidebar.
#' @param widget widget to preview
#' @param title title to display
#' @param wrapBox boolean indicating if the widget is to be wrapped in an
#'   \code{\link{fdRowBox}}
#' @param ... additional elements to pass to \code{\link{fdBody}}.
#' @export
#' @export
#' @examples 
#' library(flightdeck)
#' fdSimpleTable(mtcars) %>% fdPreview(title = 'Simple Table')
#' fdSimpleTable(mtcars) %>% fdPreviewBoard(title = 'Simple Table')
fdPreview <- function(widget, title = deparse(substitute(widget)), 
    wrapBox = TRUE, ...){
  if (title[1] == ".") title = "Preview"
  html <- fdBoard(div(), div(),
    fdBody(
      fdRow(
        if (wrapBox){
          fdBox(width = 12, title = title, widget, solidHeader = TRUE)
        } else {
          widget
        }
      ),
      ...,
      flightdeck:::activatePopover(),
      tags$style(".content-wrapper{margin-left: 0px;}")
    )
  )
  browsable(html)
}

#' @rdname fdPreview
#' @inheritParams fdPreview
#' @export
fdPreviewBoard <- function(widget, title = deparse(substitute(widget)),  
    wrapBox = TRUE, ...){
  if (title[1] == ".") title = "Preview"
  html <- fdBoard(
    fdHeader(title = 'Alteryx', miniTitle = 'A'),
    fdSidebar(
      fdSidebarMenu(
        fdMenuItem("Page", icon = fdIcon("th"), tabName = 'preview')
      )
    ),
    fdBody(
      fdRow(
        if (wrapBox){
          fdBox(width = 12, title = title, widget, solidHeader = TRUE)
        } else {
          widget
        }
      ),
      ...,
      activatePopover(),
      tags$style("
        .dt-buttons.btn-group {float: left;}
        .dataTables_filter{margin-bottom: 10px;}")
    )
  )
  html
  if (isTRUE(getOption('knitr.in.progress'))){
    html
  } else {
    htmltools::html_print(html, background = NULL)
  }
}


activatePopover <- function(){
  tags$script("
    $(document).ready(function(){
      $('[data-toggle=popover]').popover()
    })
  ")
}
