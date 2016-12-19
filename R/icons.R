#' Icon
#'
#' @param name name of the icon
#' @param class class to add
#' @param lib font library to use
#' @export
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
