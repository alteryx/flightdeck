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


# Tries very hard to compute a sensible R squared, else returns NaN.
#
# @param y_pred one numeric vector
# @param y_true another numeric vector of the same length
# @author Todd Morley
R2_Score <- function(y_pred, y_true){
  if(
    !inherits(x = y_pred, what = 'numeric') ||
    !inherits(x = y_true, what = 'numeric')
  ){
    stop(
      msg = paste(
        "An object other than a numeric vector was passed",
        "AlteryxPredictive::rSquared().  Please contact Alteryx Support. "
      )
    )
  }
  if(length(y_pred) != length(y_true)){
    stop(
      msg = paste(
        "The vectors passed to AlteryxPredictive::rSquared() were of",
        "unequal length.  Please contact Alteryx Support. "
      )
    )
  }
  r_squared <- NULL
  try(
    expr = r_squared <-
      cov(y_pred, y_true)^2 /
      (var(y_pred) * var(y_true)),
    silent = TRUE
  )
  if(
    is.null(r_squared) ||
    is.nan(r_squared) ||
    r_squared < 0.0 ||
    r_squared > 1.0
  ){
    try(
      expr = r_squared <-
        exp(
          2 * log(cov(y_pred, y_true)) -
            log(var(y_pred)) -
            log(var(y_true))
        ),
      silent = TRUE
    )
  }
  if(
    is.null(r_squared) ||
    is.nan(r_squared) ||
    r_squared < 0.0 ||
    r_squared > 1.0
  ){
    r_squared <- NaN
  }
  return(r_squared)
}

