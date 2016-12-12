library(flightdeck)
mod <- lm(mpg ~ ., data = mtcars)
residualHistogram <- plotlyLite(
  data = list(list(
    x = unname(residuals(mod)),
    type = 'histogram'
  )),
  list(margin = list(t = 40), bargap = 0.05),
  list(displaylogo = FALSE, displayModeBar = FALSE),
  height = 325
)

fdPreview(residualHistogram)

mod %>%
  fdResidualsPanel %>%
  fdPreview(title = 'Residuals')
