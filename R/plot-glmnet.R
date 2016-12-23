#' Interactive coefficient profile plot for glmnet objects.
#' @param x a glmnet model object
#' @param xvar What is on the X-axis. "norm" plots against the L1-norm of the
#'   coefficients, "lambda" against the log-lambda sequence, and "dev" against
#'   the percent deviance explained.
#' @param title plot title.
#' @param ... extra arguments passed to every series being plotted
#' @export
#' @example inst/examples/fdPlotGlmnet.R
fdPlotGlmnet <- function(x, xvar = c("norm", "lambda", "dev"), title = NULL, ...){
  d <- extractCoefGlmnet(x)
  xvar <- match.arg(xvar)
  xaxisTitle <- switch(xvar,
    norm = 'L1 Norm',
    lambda = 'Log Lambda',
    dev = 'Fraction Deviance Explained'
  )
  traces <- plyr::llply(names(d)[-c(1:3)], function(y){
    makeTrace(xvar, y, y, d, mode = 'lines',
     type = 'scatter', ...)
  })
  layout = list(
    plot_bgcolor = "#f6f6f6",
    margin = list(
      t = 10,
      l = 40,
      r = 30,
      b = 30
    ),
    title = if (!is.null(title)) title,
    xaxis = list(title = xaxisTitle),
    yaxis = list(title = 'Coefficients')
  )
  config <- list(displaylogo = FALSE, displayModeBar = FALSE)
  fdPlotly(unname(traces), layout, config)
}

#' Interactive plot for cv.glmnet object.
#' 
#' This plots MSE vs Log Lambda
#' @param x fitted "cv.glmnet" object
#' @param sign.lambda Either plot against log(lambda) (default) or its negative
#'   if sign.lambda=-1
#' @param title plot title.
#' @param ... additional arguments. not currently used
#' @export
#' @example inst/examples/fdPlotCvGlmnet.R
fdPlotCvGlmnet <- function(x, sign.lambda = NULL){
  d <- data.frame(
    x = if (is.null(sign.lambda)) log(x$lambda) else -log(x$lambda), 
    y = x$cvm, 
    e = x$cvup - x$cvm
  )
  trace <- makeErrorBarTrace('x', 'y', 'e', data = d)
  layout = list(
    plot_bgcolor = "#f6f6f6",
    margin = list(
      t = 10,
      l = 40,
      r = 30,
      b = 30
    ),
    xaxis = list(title = 'Log Lambda'),
    yaxis = list(title = 'Mean Squared Error'),
    title = if (!is.null(title)) title
  )
  config <- list(displaylogo = FALSE, displayModeBar = FALSE)
  fdPlotly(list(trace), layout, config)
}

# Extract Glmnet Coefficient Profile
# This function reuses code from \code{\link[glmnet]{plotCoef}}
extractCoefGlmnet <- function (fit, ...){
  beta <- fit$beta
  lambda <- fit$lambda
  df <- fit$df
  dev <- fit$dev.ratio 
  which = nonzeroCoef(beta)
  nwhich = length(which)
  switch(nwhich + 1, `0` = {
    warning("No plot produced since all coefficients zero")
    return()
  }, `1` = warning(
    "1 or less nonzero coefficients; glmnet plot is not meaningful"
  ))
  beta = as.matrix(beta[which, , drop = FALSE])
  d <- as.data.frame(t(beta))
  d2 <- cbind(
    `norm` = apply(abs(beta), 2, sum),
    `lambda` = log(lambda),
    `dev` = dev,
    d
  )
  return(d2)
}

# Utility function to make a plotly trace
makeTrace <- function(x, y, name, data, ...){
  list(
    x = data[[x]],
    y = data[[y]],
    name = name,
    ...
  )
}

#' Make an error bar trace
makeErrorBarTrace <- function(x, y, e, data){
  makeTrace(x = x, y = y, name = y, data = data,
    error_y = list(type = 'data', array = data[[e]], visible = TRUE),
    type = 'scatter'
  )
}
