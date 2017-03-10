#' Interactive coefficient profile plot for glmnet objects.
#' @param x a glmnet model object
#' @param xvar What is on the X-axis. "norm" plots against the L1-norm of the
#'   coefficients, "lambda" against the log-lambda sequence, and "dev" against
#'   the percent deviance explained.
#' @param title plot title.
#' @param xaxisTitles custom x axis titles for each \code{xvar}.
#' @param yaxisTitle custom y axis title.
#' @param ... extra arguments passed to every series being plotted
#' @export
#' @example inst/examples/fdPlotGlmnet.R
fdPlotGlmnet <- function(x, xvar = c("norm", "lambda", "dev"), title = NULL, 
    xaxisTitles = c(norm = 'L1 Norm', 
      lambda = 'Log Lambda', dev = 'Fraction Deviance Explained'
    ), 
    yaxisTitle = 'Coefficients', 
    ...
){
  d <- extractCoefGlmnet(x)
  xvar <- match.arg(xvar)
  xaxisTitle <- xaxisTitles[[xvar]]
  traces <- plyr::llply(names(d)[-c(1:3)], function(y){
    makeTrace(xvar, y, y, d, mode = 'lines',
     type = 'scatter', ...)
  })
  layout = list(
    plot_bgcolor = "#f6f6f6",
    margin = list(
      t = if (is.null(title)) 10 else 40,
      l = 40,
      r = 30,
      b = 30
    ),
    title = if (!is.null(title)) title,
    xaxis = list(title = xaxisTitle),
    yaxis = list(title = yaxisTitle)
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
#' @param xaxisTitle custom x axis title
#' @param yaxisTitle custom y axis title.
#' @param ... additional arguments. not currently used
#' @export
#' @example inst/examples/fdPlotCvGlmnet.R
fdPlotCvGlmnet <- function(x, sign.lambda = NULL, title = NULL, 
   xaxisTitle = 'Log Lambda',
   yaxisTitle = 'Coefficients',
   ...
){
  d <- data.frame(
    x = if (is.null(sign.lambda)) log(x$lambda) else -log(x$lambda), 
    y = x$cvm, 
    e = x$cvup - x$cvm
  )
  trace <- makeErrorBarTrace('x', 'y', 'e', data = d)
  layout = list(
    plot_bgcolor = "#f6f6f6",
    margin = list(
      t = if (is.null(title)) 10 else 40,
      l = 40,
      r = 30,
      b = 30
    ),
    xaxis = list(title = xaxisTitle),
    yaxis = list(title = yaxisTitle),
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

# This function reuses code from \code{\link[glmnet]{plotCoef}}
nonzeroCoef = function (beta, bystep = FALSE) 
{
  ### bystep = FALSE means which variables were ever nonzero
  ### bystep = TRUE means which variables are nonzero for each step
  nr=nrow(beta)
  if (nr == 1) {#degenerate case
    if (bystep) 
      apply(beta, 2, function(x) if (abs(x) > 0) 
        1
        else NULL)
    else {
      if (any(abs(beta) > 0)) 
        1
      else NULL
    }
  }
  else {
    beta=abs(beta)>0 # this is sparse
    which=seq(nr)
    ones=rep(1,ncol(beta))
    nz=as.vector((beta%*%ones)>0)
    which=which[nz]
    if (bystep) {
      if(length(which)>0){
        beta=as.matrix(beta[which,,drop=FALSE])
        nzel = function(x, which) if (any(x)) 
          which[x]
        else NULL
        which=apply(beta, 2, nzel, which)
        if(!is.list(which))which=data.frame(which)# apply can return a matrix!!
        which
      }
      else{
        dn=dimnames(beta)[[2]]
        which=vector("list",length(dn))
        names(which)=dn
        which
      }
      
    }
    else which
  }
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

# Make an error bar trace
makeErrorBarTrace <- function(x, y, e, data){
  makeTrace(x = x, y = y, name = y, data = data,
    error_y = list(type = 'data', array = data[[e]], visible = TRUE),
    type = 'scatter'
  )
}
