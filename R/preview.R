#' Preview a widget in the dashboard.
#'
#'
#' This function helps preview a widget in the dashboard with or without the
#' header and sidebar.
#' @param widget widget to preview
#' @param title title to display
#' @param wrap string indicating if the widget is to be wrapped in a 'row',
#'   'box', 'rowbox' or 'none'
#'   \code{\link{fdRowBox}}
#' @param wrapBox boolean indicating if we should set wrap = 'rowbox'
#' @param ... additional elements to pass to \code{\link{fdBody}}.
#' @export
#' @export
#' @examples 
#' library(flightdeck)
#' fdSimpleTable(mtcars) %>% fdPreview(title = 'Simple Table')
#' fdSimpleTable(mtcars) %>% fdPreviewBoard(title = 'Simple Table')
fdPreview <- function(widget, title = deparse(substitute(widget)), 
    wrap = 'rowbox', ..., wrapBox = NULL){
  if (title[1] == ".") title = "Preview"
  if (!is.null(wrapBox) && !wrapBox) wrap = 'row'
  if (wrap == 'rowbox' || wrap == 'box'){
    widget <- fdBox(width = 12, title = title, widget, solidHeader = TRUE)
  }
  if (wrap == 'rowbox' || wrap == 'row') {
    widget <- fdRow(widget)
  }
  html <- fdBoard(div(), div(),
    fdBody(widget, ...,
      activatePopover(),
      tags$style(".content-wrapper{margin-left: 0px;}")
    )
  )
  browsable(html)
}

#' @rdname fdPreview
#' @inheritParams fdPreview
#' @export
fdPreviewBoard <- function(widget, title = deparse(substitute(widget)), 
    wrap = 'rowbox', ...,  wrapBox = NULL){
  if (title[1] == ".") title = "Preview"
  if (!is.null(wrapBox) && !wrapBox) wrap <- 'row'
  if (wrap == 'rowbox' || wrap == 'box'){
    widget <- fdBox(width = 12, title = title, widget, solidHeader = TRUE)
  }
  if (wrap == 'rowbox' || wrap == 'row') {
    widget <- fdRow(widget)
  }
  html <- fdBoard(
    fdHeader(title = 'Alteryx', miniTitle = 'A'),
    fdSidebar(
      fdSidebarMenu(
        fdMenuItem("Page", icon = fdIcon("th"), tabName = 'preview')
      )
    ),
    fdBody(widget, ...,
      activatePopover(),
      tags$style("
        .dt-buttons.btn-group {float: left;}
        .dataTables_filter{margin-bottom: 10px;}")
    )
  )
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
