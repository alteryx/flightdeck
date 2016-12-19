# Example 1: Scatterplot
dat <- list(
  x = runif(100),
  y = runif(100),
  mode = 'markers',
  type = 'scatter'
)
layout <- list(margin = list(t = 40))
config <- list(displaylogo = FALSE, displayModeBar = FALSE)

if (interactive()){
  fdPlotly(
    data = list(dat),
    layout = layout,
    config = config,
    height = 325
  )  
}


# Example 2: Line and Scatter Plot
# https://plot.ly/javascript/line-and-scatter/
trace1 = list(
  x =  c(1, 2, 3, 4),
  y =  c(10, 15, 13, 17),
  mode =  'markers',
  type =  'scatter'
)

trace2 = list(
  x =  c(2, 3, 4, 5),
  y =  c(16, 5, 11, 9),
  mode =  'lines',
  type =  'scatter'
)

trace3 = list(
  x =  c(1, 2, 3, 4),
  y =  c(12, 9, 15, 12),
  mode =  'lines+markers',
  type =  'scatter'
)

if (interactive()){
  fdPlotly(
    data = list(trace1, trace2, trace3)
  )
}
