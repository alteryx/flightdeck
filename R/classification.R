#' Panel displaying classification model metrics
#' 
#' 
#' @param actual vector of actual values
#' @param predicted vector of predicted values
#' @param metrics vector of metrics to compute. See \code{\link{MLmetrics}} for list
#'   of potential metrics
#' @import plyr
#' @export
#' @example inst/examples/classification-metrics.R
#' @family classification
fdPanelClassificationMetrics <- function(actual, predicted,
    metrics = c("Accuracy", "Recall", "Precision", "F1_Score")){
  metricsDF = computeClassificationMetrics(actual, predicted, metrics)
  l <- plyr::alply(metricsDF, 1, function(d){
    percentBars <- c('MAPE', 'R2_Score')
    fdStat(d$Abbreviation, d$Value, note = d$Metric)
  })
  do.call(tagList, l)
}

#' Panel displaying classification model metrics by class
#' 
#' @param actual vector of actual classes.
#' @param predicted vector of predicted classes.
#' @param cutoffs vector of cutoffs to use for deciding bar colors.
#' @param vertical boolean indicating if panel should display measures in columns.
#' @export
#' @example inst/examples/classification-metrics.R
#' @family classification
fdPanelClassificationMetricsByClass <- function(actual, predicted, vertical = TRUE,  
    cutoffs = c(0, 0.6, 0.75, 1)){
  d <- computeClassificationMetricsByClass(actual, predicted, vertical = vertical)
  getStatus <- function(x, cutoffs){
    cut(x, cutoffs, c('danger', 'warning', 'success'))
  }
  getBar <- function(y){
    sapply(y, function(x){
      if (is.na(x)) {
        ""
      } else {
        x <- as.numeric(x)
        as.character(progressBar(x*100, getStatus(x, cutoffs)))
      }
    })
  }
  numericCols <- sapply(d, is.numeric)
  d[,numericCols] <- apply(d[,numericCols], 2, getBar)
  fdSimpleTable(d)
}


#' @rdname fdPanelClassificationMetrics
#' @inheritParams fdPanelClassificationMetrics
#' @export
computeClassificationMetrics <- function(actual, predicted,
   metrics = c("Precision", "Recall", "F1_Score", "Accuracy")){
  d <- plyr::ldply(metrics, function(f){
    fn = getFromNamespace(f, 'MLmetrics')
    value = fn(y_pred = predicted, y_true = actual)
    data.frame(Abbreviation = f, Value = value)
  })
  defn <- read.csv(
   pkgFile('definitions/classification_metrics.csv')
  )
  merge(defn, d)
}

#' @rdname fdPanelClassificationMetricsByClass
#' @inheritParams fdPanelClassificationMetricsByClass
#' @export
computeClassificationMetricsByClass <- function(actual, predicted, vertical = TRUE){
  cm <- as.matrix(xtabs(~ actual + predicted))

  n = sum(cm) # number of instances
  nc = nrow(cm) # number of classes
  diag = diag(cm) # number of correctly classified instances per class
  rowsums = apply(cm, 1, sum) # number of instances per class
  colsums = apply(cm, 2, sum) # number of predictions per class
  p = rowsums / n # distribution of instances over the actual classes
  q = colsums / n # distribution of instances over the predicted classes


  precision = diag / colsums
  recall = diag / rowsums
  f1 = 2 * precision * recall / (precision + recall)

  d1 <- as.data.frame(do.call(cbind, list(precision, recall, f1)))
  colnames(d1) <- c('Precision', 'Recall', 'F1_Score')

  # Macro Averages

  macroMetrics = c(
    Precision = mean(precision),
    Recall = mean(recall),
    F1 = mean(f1),
    Accuracy = sum(diag) / n
  )

  # Micro Averages

  oneVsAll <- lapply(1 : nc, function(i){
    v = c(cm[i,i], rowsums[i] - cm[i,i], colsums[i] - cm[i,i],
          n-rowsums[i] - colsums[i] + cm[i,i]
    );
    return(matrix(v, nrow = 2, byrow = T))
  })

  s <- Reduce("+", oneVsAll)

  microMetrics <- c(
    Precision = (diag(s) / apply(s,1, sum))[1],
    Recall = (diag(s) / apply(s, 2, sum))[1]
  )

  microMetrics['F1_Score'] =  2 * microMetrics['Precision'] * microMetrics['Recall'] / (microMetrics['Precision'] + microMetrics['Recall'])

  microMetrics['Accuracy'] = sum(diag(s)) / sum(s)
  d1$Accuracy = NA
  d1 <- rbind(d1, microAverage = microMetrics, macroAverage = macroMetrics)
  d1 <- cbind(Class = rownames(d1), d1)
  if (vertical){
    d2 <- as.data.frame(t(d1[,-1]), stringsAsFactors = F)
    d2 <- d2[c('Accuracy', 'F1_Score', 'Precision', 'Recall'),]
    d2 <- cbind(Measure = rownames(d2), d2, stringsAsFactors = F)
    rownames(d2) <- NULL
    d2[,-1] <- apply(d2[,-1], 2, as.numeric)
    return(d2)
  } else {
    return(d1)
  }
}
