#' Create a simple html table
#'
#'
#' @param x data frame to be displayed as a html table
#' @export
#' @examples
#' fdSimpleTable(mtcars) %>% fdPreview
fdSimpleTable <- function(x, class = 'table table-striped'){
  tags$table(class = class,
    tags$tbody(
      tags$tr(lapply(names(x), tags$th)),
      apply(x, 1, function(d){
        tags$tr(lapply(d, function(x) tags$td(HTML(x))))
      })
    )
  )
}

#' Syntactic sugar to create a html table from a list of columns
#'
#' @param ... columns for the table.
#' @param class css calss for table.
#' @export
fdTable <- function(..., class = 'table table-striped'){
  fdSimpleTable(data.frame(...))
}


#' Displaying regression coefficients
#'
#'
#' @param mod model object
#' @param digits number of digits to display
#' @param barColor bar color
#' @import DT
#' @export
#' @example inst/examples/fdPanelCoefficients.R
fdPanelCoefficients <- function(mod, digits = 3, barColor = 'steelblue'){
  coefTable <- as.data.frame(summary(mod)$coef)
  coefTable$Impact <- abs(coefTable$Estimate)

  add_star <- function(x){
    paste(rep('&starf;', x), collapse = "")
  }
  coefTable$Confidence <-cut(
    coefTable$`Pr(>|t|)`,
    c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
    c(add_star(3), add_star(2), add_star(1), "&#8226;", "")
  )

  coefTable <- cbind(Variable = rownames(coefTable), coefTable)
  coefTable <- coefTable[, c('Variable', 'Estimate', 'Impact',
    'Confidence', "Std. Error", "t value", "Pr(>|t|)"
  )]
  numericCols <- c('Estimate', 'Std. Error', 't value', 'Pr(>|t|)')
  coefTable[,numericCols] <- format(coefTable[,numericCols], digits = digits)
  table1 <- datatable(
    coefTable,
    rownames = FALSE,
    extensions = c('Buttons', 'Responsive'),
    options = list(
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'colvis',
          text = 'Display Advanced Statistics', columns =4:6
        )
      ),
      columnDefs = list(
        list(targets = 4:6, visible = F)
      )
    ),
    style = 'bootstrap',
    width = '100%',
    height = if (NROW(coefTable) > 10) 550 else NULL,
    class = c('stripe', 'hover', 'cell-border'),
    escape = FALSE
  )

  table1 %>%
    formatStyle('Impact',
      background = styleColorBar(range(coefTable$Impact), barColor),
      backgroundSize = '98% 88%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center',
      color = 'transparent'
    )
}

#' Displaying variable importance
#'
#' @inheritParams fdPanelCoefficients
#' @import DT
#' @export
#' @example inst/examples/fdPanelImportance.R
fdPanelImportance <- function(mod, digits = 2, barColor = 'steelblue'){
  coefTable <- data.frame(
    Variable = names(mod$variable.importance),
    Impact = mod$variable.importance/sum(mod$variable.importance)
  )
  coefTable$Importance <- coefTable$Impact
  coefTable <- coefTable[,c('Variable', 'Importance', 'Impact')]
  datatable(
    coefTable,
    options = list(
      dom = 'frtip'
    ),
    rownames = FALSE,
    extensions =  'Responsive',
    style = 'bootstrap',
    width = '100%',
    height = if (NROW(coefTable) > 10) 550 else NROW(coefTable)*75,
    class = c('stripe', 'hover', 'cell-border'),
    escape = F
  ) %>% formatStyle('Impact',
    background = styleColorBar(c(0, 1), barColor),
    backgroundSize = '98% 88%',
    backgroundRepeat = 'no-repeat',
    backgroundPosition = 'center',
    color = 'transparent'
  ) %>% formatPercentage(
    'Importance', digits = digits
  )
}

#' Display a statistic
#'
#' @export
fdStat <- function(name, value, color = 'green', note = name, pct = value*100,
    showBar = TRUE, digits = 4){
  div(class = 'progress-group',
    tags$div(class = 'progress-metrics',
      span(class = 'progress-text', name),
      span(class = 'progress-number', if (showBar){
        tags$b(sprintf("%.1f %%", value*100))
      } else {
        tags$b(format(value, digits = digits))
      }),
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

# Interactive table of regression coefficients
fdTidyTable <- function(coefTable, digits = 3, barColor = 'steelblue'){
  names(coefTable) <- c('Term', 'Estimate', 'Std. Error', 'Statistic', 'P Value')
  coefTable$Impact <- abs(coefTable$Estimate)

  add_star <- function(x){
    paste(rep('&starf;', x), collapse = "")
  }
  coefTable$Confidence <-cut(
    coefTable$`P Value`,
    c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
    c(add_star(3), add_star(2), add_star(1), "&#8226;", "")
  )
  coefTable <- coefTable[, c('Term', 'Estimate', 'Impact',
    'Confidence', "Std. Error", "Statistic", "P Value"
  )]
  numericCols <- c('Estimate', 'Std. Error', 'Statistic', 'P Value')
  coefTable[,numericCols] <- format(coefTable[,numericCols], digits = digits)
  table1 <- datatable(
    coefTable,
    rownames = FALSE,
    extensions = c('Buttons', 'Responsive'),
    options = list(
      dom = 'Bfrtip',
      buttons = list(
        list(
          extend = 'colvis',
          text = 'Display Advanced Statistics', columns =4:6
        )
      ),
      columnDefs = list(
        list(targets = 4:6, visible = F)
      )
    ),
    style = 'bootstrap',
    width = '100%',
    height = if (NROW(coefTable) > 10) 550 else NULL,
    class = c('stripe', 'hover', 'cell-border'),
    escape = FALSE
  )

  table1 %>%
    formatStyle('Impact',
      background = styleColorBar(range(coefTable$Impact), barColor),
      backgroundSize = '98% 88%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center',
      color = 'transparent'
    )
}
