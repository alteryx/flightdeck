library(ROCR)
data(ROCR.simple)
pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred,"tpr","fpr")
d <- data.frame(
  x = signif(perf@x.values[[1]]*100, 2),
  y = signif(perf@y.values[[1]]*100, 2)
)
value = list(names(d)[names(d) != 'x'])

plot2 = AlteryxRviz::c3(
  legend = list(
    show = if (NCOL(d) == 2) FALSE else 'bottom'
    #inset = list(anchor = 'bottom-right', x = 0, y = 120)
  ),
  padding = list(right = 60),
  data = list(
    json = d,
    keys = list(
      x = 'x',
      value =  value
    ),
    types = as.list(setNames(rep('line', length(value) - 1), value[-1])),
    colors = list(
      Baseline = 'gray'
    ),
    onclick = htmlwidgets::JS("function(d){console.log(JSON.stringify(d))}")
  ),
  axis = list(
    x = list(tick = list(
      values = seq(0, 100, 20)
    )),
    y = list(
      tick = list(outer = FALSE, values = seq(0, 100, 20)),
      max = 100, min = 0,
      padding = list(top = 0, bottom = 0)
    )
  ),
  grid = list(y = list(show = TRUE)),
  width = '100%',
  height = 300,
  point = list(
    show = FALSE
  )
)
plot2
