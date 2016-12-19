library(htmltools)
mod <- lm(mpg ~ ., data = mtcars)

# Sidebar
sidebar <- fdSidebarMenu(
  fdMenuItem('Summary', icon = fdIcon('th'), pageName = 'page1')
)

# Body
row1 <- fdRow(
  fdInfoBox('Adjusted R-squared', '80.7%', 
    color = 'blue', icon = fdIcon('chart-line', lib = 'entypo')),
  fdInfoBox('F-Statistic', 13.93,  'on 10 and 21 degrees of freedom', 
    color = 'green', icon = fdIcon('star', lib = 'entypo')),
  fdInfoBox('Residual Standard Error', 2.65, 'on 21 degrees of freedom', 
    color = 'purple', icon = fdIcon('flash', lib = 'entypo'))
)

row2 <- fdRow(fdBox(fdCoefTable(mod), width = 12))

page1 <- fdPage(row1, row2, id = 'page1', display = TRUE)

fdBoard(
  fdHeader(title = 'Linear Regression'),
  fdSidebar(sidebar),
  fdBody(page1)
)

#' Example 2
definitions <- list(
  adjRsquare = "The adjusted R-squared compares the explanatory power of regression models that contain different numbers of predictors.

Suppose you compare a five-predictor model with a higher R-squared to a one-predictor model. Does the five predictor model have a higher R-squared because it’s better? Or is the R-squared higher because it has more predictors? Simply compare the adjusted R-squared values to find out!

The adjusted R-squared is a modified version of R-squared that has been adjusted for the number of predictors in the model. The adjusted R-squared increases only if the new term improves the model more than would be expected by chance. It decreases when a predictor improves the model by less than expected by chance. The adjusted R-squared can be negative, but it’s usually not.  It is always lower than the R-squared."
)

iconWithModal <-  fdIcon('info-circled', lib = 'entypo') %>%
  fdModal(definitions$adjRsquare, title = 'Adjusted R-Squared')
adjRsquare <- span("Adjusted R-squared", iconWithModal)

row1 <- fdRow(
  fdInfoBox(adjRsquare, '80.7%', 
    color = 'blue', icon = fdIcon('chart-line', lib = 'entypo')),
  fdInfoBox('F-Statistic', 13.93,  'on 10 and 21 degrees of freedom', 
    color = 'green', icon = fdIcon('star', lib = 'entypo')),
  fdInfoBox('Residual Standard Error', 2.65, 'on 21 degrees of freedom', 
    color = 'purple', icon = fdIcon('flash', lib = 'entypo'))
)

page2 <- fdPage(row1, row2, id = 'page1', display = TRUE)

fdBoard(
  fdHeader(title = 'Linear Regression'),
  fdSidebar(sidebar),
  fdBody(page2)
)
