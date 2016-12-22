library(glmnet)
library(flightdeck)
data(QuickStartExample)
cvfit <- cv.glmnet(x, y)

if (interactive()){
  #' Interctive profiles plot
  fdPlotCvGlmnet(cvfit)
}
