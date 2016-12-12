#' Create a column
#'
#' @param width the grid width of the column
#' @param ... elements to include within the column
#' @param offset the number of columns to offset this column
#' @param id id for the column
#'
#' @export
fdColumn <- function(width, ..., offset = NULL,
    id = paste0('column-', round(runif(1)*10000))){
  if (length(width) == 1){
    if (is.null(names(width))) width = c(md = width) else width = width
  } else if (is.null(names(width))){
    names(width) <- c('xs', 'sm', 'md', 'lg')[seq_along(width)]
  }
  if (!is.null(offset)){
    if (length(offset) == 1){
      offset = c(md = offset)
    } else if (is.null(names(offset))){
      names(offset) <- c('xs', 'sm', 'md', 'lg')[seq_along(offset)]
    }
  }
  col_class = paste('col', names(width), width, sep = '-')
  if (!is.null(offset)){
    off_class = paste0('col-', names(offset), '-offset-', offset)
    col_class = c(col_class, off_class)
  }
  div(class = paste(col_class, collapse = " "), id = id, ...)
}

#' Create a row
#'
#' @param ... elements to include within a row
#' @export
fdRow <- function(...){
  div(class = "row", ...)
}
