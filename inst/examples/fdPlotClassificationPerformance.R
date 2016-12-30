library(flightdeck)
library(ROCR)
data(ROCR.simple)
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred, 'tpr', 'fpr')


rocChart <- fdPlotClassificationPerformance(
  performance(pred, "tpr", "fpr"), digits = 2
)
precisionRecallChart <- fdPlotClassificationPerformance(
  performance(pred, "prec", "rec"), digits = 2
)

# Example 1
fdTabsetPanel(selected = makeHtmlId('ROC Chart'),
  fdTabPanel('ROC Chart', rocChart),
  fdTabPanel('Precision vs. Recall', precisionRecallChart)
) %>%
  fdColumn(width = 12) %>%
  fdPreview(wrap = 'row')
