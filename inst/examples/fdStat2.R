library(flightdeck)
fdStat2 <- function(name, value, color = 'green', note = name, pct = value*100,
    showBar = TRUE, digits = 4){
  div(class = 'progress-group',
    tags$div(class = 'progress-metrics',
      span(class = 'progress-text', name),
      span(class = 'progress-number', 
        if (showBar){
          tags$b(sprintf("%.1f %%", value*100))
        } else {
          tags$b(format(mean(value), digits = digits))
        },
        sparkline::sparkline(rpois(5, 20), type = 'bar', width = 40)
      ),
      div(class = 'progress-subtext', tags$small(note))
    ),
    if (showBar){
      div(class = 'progress sm',
        div(class = paste0('progress-bar progress-bar-', color),
          style = sprintf('width: %s%%', pct)
        )
      )
    } else {
      div(style='margin-bottom: 15px;')
    }
  )
}

notes <- c(
  'Proportion of values predicted positive, that were actually positive',
  'Proportion of values actually positive, that were predicted positive',
  'Harmonic mean of Recall and Precision',
  'Proportion of correct predictions in the data'
)

metricsNoBar <- tagList(
  fdStat2("Precision", runif(5), note = notes[1], showBar = F),
  fdStat2("Recall", 0.90, note = notes[2], showBar = F),
  fdStat2("F1-Score", 0.95, note = notes[3], showBar = F),
  fdStat2("Accuracy", 0.80, note = notes[4], showBar = F)
)

metricsNoBar %>% fdPreview
