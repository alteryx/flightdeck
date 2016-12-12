fdPreview2 <- function(widget, title = deparse(substitute(widget)), wrapBox = TRUE, ...){
  if (title == ".") title = "Preview"
  html <- fdBoard(
    fdHeader(title = 'Alteryx', miniTitle = 'A'),
    fdSidebar(
      fdSidebarMenu(
        fdMenuItem("Page", icon = fdIcon("th"), tabName = 'preview')
      ),
      disable = TRUE
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

#' Preview a widget in the dashboard.
#'
#' @param widget widget to preview
#' @param title title to display
#' @export
#' @export
#' @examples 
#' library(flightdeck)
#' fdSimpleTable(mtcars) %>% fdPreview(title = 'Simple Table')
fdPreview <- function(widget, title = deparse(substitute(widget)), wrapBox = TRUE, ...){
  if (title == ".") title = "Preview"
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


activatePopover <- function(){
  tags$script("
    $(document).ready(function(){
      $('[data-toggle=popover]').popover()
    })
  ")
}
