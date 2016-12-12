library(flightdeck)
fit <- lm(mpg ~ ., data = mtcars)

# Extract fitted values from lm() object
Fitted.Values <-  fitted(fit)

# Extract residuals from lm() object
Residuals <-  resid(fit)

# Extract standardized residuals from lm() object
Standardized.Residuals <- MASS::stdres(fit)

# Extract fitted values for lm() object
Theoretical.Quantiles <- qqnorm(Residuals, plot.it = F)$x

# Square root of abs(residuals)
Root.Residuals <- sqrt(abs(Standardized.Residuals))

# Calculate Leverage
Leverage <- lm.influence(fit)$hat

# Create data frame
# Will be used as input to plot_ly

regMat <- data.frame(
  Fitted.Values,
  Residuals,
  Standardized.Residuals,
  Theoretical.Quantiles,
  Root.Residuals,
  Leverage
)


LOESS1 <- loess.smooth(Fitted.Values, Residuals)


dat = list(
  list(
    x = regMat$Fitted.Values,
    y = regMat$Residuals,
    type = 'scatter',
    mode = 'markers',
    hoverinfo = "x+y",
    name = 'Data',
    marker = list(size = 10, opacity = 0.5),
    showlegend = F
  ),
  list(
    x = LOESS1$x,
    y = LOESS1$y,
    type = "scatter",
    mode = "line",
    name = "Smooth",
    line = list(width = 2),
    showlegend = F
  )
)

# regMat = list(
#   Fitted.Values = runif(100000),
#   Residuals = runif(100000)/10000
# )

dat2 = list(
  list(
    x = regMat$Fitted.Values,
    y = regMat$Fitted.Values + regMat$Residuals,
    type = 'scatter',
    mode = 'markers',
    hoverinfo = "x+y",
    name = 'Data',
    marker = list(size = 10, opacity = 0.5),
    showlegend = F
  ),
  list(
    x = range(regMat$Fitted.Values),
    y = range(regMat$Fitted.Values),
    type = 'scatter',
    mode = 'line',
    marker = list(size = 10, opacity = 0.5),
    showlegend = F
  )

)

layout = list(
  #title = "Scale Location",
  plot_bgcolor = "#f6f6f6",
  margin = list(
    t = 10,
    l = 30,
    r = 30,
    b = 30
  ),
  xaxis = list(title = 'Actual'),
  yaxis = list(title = 'Predicted')
)

config <- list(displaylogo = FALSE, displayModeBar = FALSE)

plt1 <- plotlyLite(dat2, layout, config)
plt1
