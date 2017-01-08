#' Pipe functions
#'
#' Use the pipe function, \code{\%>\%} to turn function composition
#' into a series of imperative statements.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs A visualisation and a function to apply to it
NULL


pkgFile <- function(...){
  system.file(..., package = 'flightdeck')
}

addStar <- function(x){
  paste(rep('&starf;', x), collapse = "")
}

makeConfidenceStars <- function(x){
  cut(
    x,
    c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
    c(addStar(3), addStar(2), addStar(1), "&#8226;", "")
  )
}

#' Make a valid html id from a string
#' 
#' @param x string to convert into a valid html id.
#' @export
makeHtmlId <- function(x){
  x <- gsub(".", "-", make.names(x), fixed = TRUE)
  x <- gsub("_", "-", x, fixed = TRUE)
  tolower(x)
}

inAlteryx <- function(){
  exists("AlteryxDataOutput", .GlobalEnv)
}

stylesheet <- function(x){
  paste0(x, if (inAlteryx()) '-selfcontained' else '', '.min.css')
}
