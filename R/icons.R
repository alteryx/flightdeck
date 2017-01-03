#' Create a font icon.
#'
#' @param name name of the icon
#' @param class html class attribute to add. See details for an example.
#' @param lib font library to use. See details.
#' @param ... extra items to be passed to the icon.
#' @export
#' @details 
#' This function supports four font libraries at present. A detailed list of fonts
#' can be found on the pages listed below. The name to pass to \code{\link{fdIcon}}
#' should be devoid of any of the suffixes used. For example to use a glyphicon
#' named \code{glyphicon glyphicon-asterisk}, you would use the name
#' \code{asterisk} and set \code{lib} to \code{glyphicon}.
#' 
#' You can use the \code{class} argument to add extra classes to the icon. This
#' is useful when you want to drive behavior. For example an icon marked with
#' the class \code{popover} could display a help note on being clicked.
#' 
#' \itemize{
#'   \item font-awesome \url{http://fontawesome.io/icons/}
#'   \item glyphicon \url{http://fontello.github.io/entypo/demo.html}
#'   \item entypo \url{http://getbootstrap.com/components/}
#' }
fdIcon <- function (name, class = NULL, lib = "font-awesome", ...){
  prefixes <- list(
    `font-awesome` = "fa",
    glyphicon = "glyphicon",
    ionicons = 'ion',
    entypo = 'en'
  )
  prefix <- prefixes[[lib]]
  if (is.null(prefix)) {
    stop("Unknown font library '", lib, "' specified. Must be one of ",
         paste0("\"", names(prefixes), "\"", collapse = ", "))
  }
  iconClass <- ""
  if (!is.null(name))
    iconClass <- paste0(prefix, " ", prefix, "-", name)
  if (!is.null(class))
    iconClass <- paste(iconClass, class)
  iconTag <- tags$i(class = iconClass, ...)
  if (lib == "font-awesome") {
    htmlDependencies(iconTag) <- htmlDependency(
       "FontAwesome", "4.7.0",
       c(file = lib.file('fontawesome')),
       stylesheet = 'css/font-awesome.min.css'
    )
  }
  if (lib == "ionicons"){
    htmlDependencies(iconTag) <- htmlDependency(
      "Ionicons", "2.0.1",
      c(file = lib.file('ionicons')),
      stylesheet = 'css/ionicons.min.css'
    )
  }
  if (lib == "entypo"){
    htmlDependencies(iconTag) <- htmlDependency(
      "Entypo", "2.0.1",
      c(file = lib.file('entypo')),
      stylesheet = 'entypo.css'
    )
  }
  iconTag
}

# Icon with tooltip
fdIconWithPopover <- function(name = 'info-sign', note = "", class = NULL,
   lib = "glyphicon"){
  fdIcon('info-sign', lib = lib,
    class = class,
    `data-content` = note,
    `data-toggle` = "popover",
    `data-html` = TRUE,
     style = "cursor:pointer;font-size:16px;"
  )
}

# Text with tooltip that has a popover
fdTitleWithPopover <- function(text, note = "", name = 'info-sign',
    class = NULL, lib = 'glyphicon', ...){
  tagList(
    tags$span(text),
    fdIconWithPopover(name = name, note = note, class = class, lib = lib),
    ...
  )
}
