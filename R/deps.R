# Add an html dependency, without overwriting existing ones
# https://github.com/rstudio/shinydashboard/blob/master/R/deps.R
appendDependencies <- function(x, value) {
  if (inherits(value, "html_dependency"))
    value <- list(value)
  old <- attr(x, "html_dependencies", TRUE)
  htmlDependencies(x) <- c(old, value)
  x
}

lib.file <- function(x){
  system.file(file.path('htmlwidgets', 'lib', x), package = 'flightdeck')
}

# Add dashboard dependencies to a tag object
addDeps <- function(x, theme) {
  dashboardDeps <- list(
    htmlDependency("jquery", '2.2.3',
      c(file = lib.file('jquery')),
      script = "jquery-2.2.3.min.js"
    ),
    htmlDependency("bootstrap", '3.3.6',
      c(file = lib.file('bootstrap')),
      script = "js/bootstrap.min.js",
      stylesheet = 'css/bootstrap.min.css'
    ),
    htmlDependency("bootbox", '4.4.0',
      c(file = lib.file('bootbox')),
      script = "bootbox.min.js"
    ),
    htmlDependency("AdminLTE", "2.3.7",
      c(file = lib.file("AdminLTE")),
      script = 'js/app.min.js',
      stylesheet = c('css/AdminLTEAyx.css', sprintf('css/skins/%s.css', theme))
    ),
    htmlDependency("jquery.slimscroll", "2.3.7",
      c(file = lib.file("slimScroll")),
      script = 'jquery.slimscroll.min.js'
    ),
    htmlDependency("flightdeck", "0.1.0",
      c(file = lib.file("flightdeck")),
      script = "flightdeck.js",
      stylesheet = "flightdeck.css"
    )
  )
  appendDependencies(x, dashboardDeps)
}
