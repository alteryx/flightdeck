#' Dashboard body
#'
#' @export
fdBody <- function(...){
  div(class = "content-wrapper",
    tags$section(class = "content", ...)
  )
}
