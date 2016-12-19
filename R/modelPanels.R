#' Panel displaying model residuals
#'
#' @param mod model object
#' @export
#' @examples
#' mod <- lm(mpg ~ ., data = mtcars)
#' mod %>% fdResidualsPanel %>% fdPreview
fdResidualsPanel <- function(mod, digits = 4){
  res <- residuals(mod)
  residualSummary <- data.frame(
    Statistic = c("Minimum", "1st Quartile", "Median", "Mean",
      "3rd Quartile", "Maximum"
    ),
    Value = format(summary(mod$residuals), digits = digits)
  )
  residualHistogram <- plotlyLite(
    data = list(list(
      x = unname(res),
      type = 'histogram'
    )),
    list(margin = list(t = 40), bargap = 0.05),
    list(displaylogo = FALSE, displayModeBar = FALSE),
    height = 325
  )
  div(class = 'fd-panel-residuals',
    fdColumn(8, residualHistogram),
    fdColumn(4, fdSimpleTable(residualSummary))
  )
}
