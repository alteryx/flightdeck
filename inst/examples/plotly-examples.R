templateDir <- system.file('templates', package = 'pkgdown')

AlteryxRhelper:::copy_dir(templateDir, 'inst/pkgdown/templates')


plotlyLite(
  data = list(list(
    x = runif(100),
    y = runif(100),
    mode = 'markers',
    type = 'scatter'
  )),
  list(margin = list(t = 40), bargap = 0.05),
  list(displaylogo = FALSE, displayModeBar = FALSE),
  height = 325
)

trace1 = list(
  x = 1:4,
  y = c(10, 15, 13, 17),
  mode = 'markers',
  type = 'scatter'
)

trace1 = list(
  x =  c(1, 2, 3, 4),
  y =  c(10, 15, 13, 17),
  mode =  'markers',
  type =  'scatter'
);

trace2 = list(
  x =  c(2, 3, 4, 5),
  y =  c(16, 5, 11, 9),
  mode =  'lines',
  type =  'scatter'
);

trace3 = list(
  x =  c(1, 2, 3, 4),
  y =  c(12, 9, 15, 12),
  mode =  'lines+markers',
  type =  'scatter'
);

