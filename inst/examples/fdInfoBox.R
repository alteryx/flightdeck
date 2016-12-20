library(flightdeck)
library(htmltools)

# Example 1
tagList(
  fdInfoBox('Adjusted R-squared', '86.9%', 
    color = 'blue', icon = fdIcon('chart-line', lib = 'entypo')),
  fdInfoBox('F-Statistic', 13.93,  'on 10 and 21 degrees of freedom', 
    color = 'green', icon = fdIcon('star', lib = 'entypo')),
  fdInfoBox('Residual Standard Error', 2.65, 'on 21 degrees of freedom', 
    color = 'purple', icon = fdIcon('flash', lib = 'entypo'))
) %>%
  fdPreview(wrapBox = F)


# Example 2: Interactive help embedded in stat box.
# The message supports markdown.
msg <- "The adjusted R-squared compares the explanatory power of regression models that contain different numbers of predictors.

Suppose you compare a five-predictor model with a higher R-squared to a one-predictor model. Does the five predictor model have a higher R-squared because it’s better? Or is the R-squared higher because it has more predictors? Simply compare the adjusted R-squared values to find out!

The adjusted R-squared is a modified version of R-squared that has been adjusted for the number of predictors in the model. The adjusted R-squared increases only if the new term improves the model more than would be expected by chance. It decreases when a predictor improves the model by less than expected by chance. The adjusted R-squared can be negative, but it’s usually not.  It is always lower than the R-squared."



adjRsquared <- span("Adjusted R-squared",
  fdIcon('info-circled', lib = 'entypo') %>%
    fdModal(msg, title = 'Adjusted R-Squared')
)

tagList(
  fdInfoBox(adjRsquared, '86.9%', 
    color = 'blue', icon = fdIcon('chart-line', lib = 'entypo')),
  fdInfoBox('F-Statistic', 13.93,  'on 10 and 21 degrees of freedom', 
    color = 'green', icon = fdIcon('star', lib = 'entypo')),
  fdInfoBox('Residual Standard Error', 2.65, 'on 21 degrees of freedom', 
    color = 'purple', icon = fdIcon('flash', lib = 'entypo'))
) %>%
  fdPreview(wrapBox = F)
