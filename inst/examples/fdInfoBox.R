library(flightdeck)
library(htmltools)

# Example 1

if (interactive()){
  tagList(
    fdInfoBox('Adjusted R-squared', '86.9%', 
      color = 'blue', icon = fdIcon('chart-line', lib = 'entypo')),
    fdInfoBox('F-Statistic', 13.93,  'on 10 and 21 degrees of freedom', 
      color = 'green', icon = fdIcon('star', lib = 'entypo')),
    fdInfoBox('Residual Standard Error', 2.65, 'on 21 degrees of freedom', 
      color = 'purple', icon = fdIcon('flash', lib = 'entypo'))
  ) %>%
    fdPreview(wrapBox = F)
}


# Example 2: Interactive help embedded in stat box.
# The message supports markdown.
msg <- "The adjusted R-squared measures the explanatory power of regression models."



adjRsquared <- span("Adjusted R-squared",
  fdIcon('info-circled', lib = 'entypo') %>%
    fdModal(msg, title = 'Adjusted R-Squared')
)

if (interactive()){
  tagList(
    fdInfoBox(adjRsquared, '86.9%', 
      color = 'blue', icon = fdIcon('chart-line', lib = 'entypo')),
    fdInfoBox('F-Statistic', 13.93,  'on 10 and 21 degrees of freedom', 
      color = 'green', icon = fdIcon('star', lib = 'entypo')),
    fdInfoBox('Residual Standard Error', 2.65, 'on 21 degrees of freedom', 
      color = 'purple', icon = fdIcon('flash', lib = 'entypo'))
  ) %>%
    fdPreview(wrapBox = F)
}
