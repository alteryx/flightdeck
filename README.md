## Interactive Dashboards for Predictive Models

[![Travis-CI Build Status](https://travis-ci.org/alteryx/flightdeck.svg?branch=master)](https://travis-ci.org/alteryx/flightdeck)

__FlightDeck__ is an R package designed to make it easy to create interactive dashboards for reporting outputs of predictive models.

## Installation

__FlighDeck__ is not currently available from CRAN, and is under acive development. You can install a stable binary from https://alteryx.github.io/drat or the development version from github.

```r
# Install stable binary...
install.packages("flightdeck", 
  repos = c(getOption('repos'), alteryx = 'https://alteryx.github.io/drat')
)

# ... or development version from github using devtools.
devtools::install_github('alteryx/flightdeck')
```

## Usage

Here is a single page dashboard generated using flightdeck.

<div class='figure'>
  <img src='http://i.imgur.com/A8mK1oH.png' width='90%' />
</div>


```r
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

row2 <- fdRow(fdBox(fdPanelCoefficients(mod), width = 12))

page1 <- fdPage(row1, row2, id = 'page1', display = TRUE)

fdBoard(
  fdHeader(title = 'Linear Regression'),
  fdSidebar(sidebar),
  fdBody(page1)
)
```

<style>
.figure img {
  max-width: 100%;
  box-shadow: 1px 2px 20px 0px rgba(0,0,0,0.25);
  border: 1px solid #ccc;
  display: block;
  margin-left: auto;
  margin-right: auto;
  margin-bottom: 30px;
  margin-top: 30px;
}
.figure .caption {
  display: none;
}
</style>
