#' Compute performance metrics by class for classification problems
#'
#' @param actual vector of actual classes
#' @param predicted vector of predicted classes
#' @export
#' @examples
#' library(rpart)
#' fit <- rpart(Species ~ ., data = iris)
#' pred <- predict(fit, type = 'class')
#' computeClassificationMetricsByClass(iris$Species, pred)
computeClassificationMetricsByClass <- function(actual, predicted){
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
  d1$Accuracy = ""
  d1 <- rbind(d1, microAverage = microMetrics, macroAverage = macroMetrics)
  cbind(Class = rownames(d1), d1)
}

#' Classification Performance By Class
#'
#' @inheritParams computeClassificationMetricsByClass
#' @param cutoffs vector of breaks
#' @export
#' @examples
#' data(ptitanic, package = 'rpart.plot')
#' fit1 = rpart::rpart(survived ~ ., data = ptitanic)
#' fdClassificationPerformance(
#'   actual = ptitanic$survived,
#'   pred = predict(fit1, type = 'class')
#' ) %>%
#'   fdPreview('Classification Performance Metrics')
computeClassificationPerformanceByClass <- function(actual, predicted,
    cutoffs = c(0, 0.6, 0.75, 1)){
  getStatus <- function(x, cutoffs){
    cut(x, cutoffs, c('danger', 'warning', 'success'))
  }
  getBar <- function(x){
    if (x == "") {
      ""
    } else {
      x <- as.numeric(x)
      as.character(progressBar(x*100, getStatus(x, cutoffs)))
    }
  }
  d2 <- computeClassificationMetricsByClass(actual, predicted)
  d2$F1_Score = sapply(d2$F1_Score, getBar)
  d2$Precision = sapply(d2$Precision, getBar)
  d2$Recall = sapply(d2$Recall, getBar)
  d2$Accuracy = sapply(d2$Accuracy, getBar)
  return(d2)
}

#' @rdname computeClassificationMetrics
#' @inheritParams computeClassificationPerformanceByClass
#' @export
fdClassificationPerformance <- function(actual, predicted,  
    cutoffs = c(0, 0.6, 0.75, 1)){
  computeClassificationPerformanceByClass(actual, predicted, cutoffs) %>%
    fdSimpleTable
}

#' Compute metrics to evaluate performance of a classification model.
#'
#' @param actual vector of actual values
#' @param predicted vector of predicted values
#' @param metrics vector of metrics to compute. See \code{\link{MLmetrics}} for list
#'   of potential metrics
#' @import plyr
#' @export
#' @examples
#' data(ptitanic, package = 'rpart.plot')
#' fit <- rpart(survived ~ ., data = ptitanic)
#' computeClassificationMetrics(
#'   actual = ptitanic$survived,
#'   predicted = predict(fit, type = 'class')
#' )
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

#' Display classification metrics in a panel
#'
#' @inheritParams computeClassificationMetrics
#' @import plyr
#' @export
#' @examples
#' library(rpart)
#' data(ptitanic, package = 'rpart.plot')
#' fit <- rpart(survived ~ ., data = ptitanic)
#' fdClassificationMetricsTable(
#'   actual = ptitanic$survived,
#'   predicted = predict(fit1, type = 'class')
#' ) %>%
#' fdPreview("Classification Metrics")
fdClassificationMetricsTable <- function(actual, predicted,
    metrics = c("Accuracy", "Recall", "Precision", "F1_Score")){
  metricsDF = computeClassificationMetrics(actual, predicted, metrics)
  l <- plyr::alply(metricsDF, 1, function(d){
    percentBars <- c('MAPE', 'R2_Score')
    fdStat(d$Abbreviation, d$Value, note = d$Metric)
  })
  do.call(tagList, l)
}
