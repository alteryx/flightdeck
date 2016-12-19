#' Create a box
#'
#' Boxes can be used to hold content in the main body of a dashboard.
#'
#' @param title Optional title.
#' @param footer Optional footer text.
#' @param status The status of the item This determines the item's background
#'   color. Valid statuses are listed in \link{validStatuses}.
#' @param solidHeader Should the header be shown with a solid color background?
#' @param background If NULL (the default), the background of the box will be
#'   white. Otherwise, a color string. Valid colors are listed in
#'   \link{validColors}.
#' @param width The width of the box, using the Bootstrap grid system. This is
#'   used for row-based layouts. The overall width of a region is 12, so the
#'   default valueBox width of 4 occupies 1/3 of that width. For column-based
#'   layouts, use \code{NULL} for the width; the width is set by the column that
#'   contains the box.
#' @param height The height of a box, in pixels or other CSS unit. By default
#'   the height scales automatically with the content.
#' @param collapsible If TRUE, display a button in the upper right that allows
#'   the user to collapse the box.
#' @param collapsed If TRUE, start collapsed. This must be used with
#'   \code{collapsible=TRUE}.
#' @param ... Contents of the box.
#'
#' @family boxes
#'
#' @export
fdBox <- function(..., title = NULL, footer = NULL, status = NULL,
    solidHeader = FALSE, background = NULL, width = 6,
    height = NULL, collapsible = FALSE, collapsed = FALSE, extraBoxClass = NULL,
    extraHeader = NULL) {

  boxClass <- "box"
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if (!is.null(status)) {
    #validateStatus(status)
    boxClass <- paste0(boxClass, " box-", status)
  }
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
  }
  if (!is.null(background)) {
    #validateColor(background)
    boxClass <- paste0(boxClass, " bg-", background)
  }

  if (!is.null(extraBoxClass)){
    boxClass <- paste(boxClass, extraBoxClass)
  }
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", validateCssUnit(height))
  }

  titleTag <- NULL
  if (!is.null(title)) {
    titleTag <- h3(class = "box-title", title)
  }

  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- if (!is.null(status)) status else "default"

    collapseIcon <- if (collapsed) "plus" else "minus"

    collapseTag <- div(class = "box-tools pull-right",
      tags$button(class = paste0("btn btn-box-tool"),
        `data-widget` = "collapse",
        icon(collapseIcon)
      )
    )
  }

  headerTag <- NULL
  if (!is.null(titleTag) || !is.null(collapseTag)) {
    headerTag <- div(class = "box-header",
      titleTag,
      collapseTag,
      extraHeader
    )
  }

  div(class = if (!is.null(width)) paste0("col-sm-", width),
    div(class = boxClass,
      style = if (!is.null(style)) style,
      headerTag,
      div(class = "box-body", ...),
      if (!is.null(footer)) div(class = "box-footer", footer)
    )
  )
}

#' Syntactic sugar for Box in a Row
#'
#' @inheritParams fdBox
#' @rdname fdBox
#' @export
#' @family boxes
fdRowBox <- function(..., title = NULL, footer = NULL, status = NULL,
   solidHeader = FALSE, background = NULL, width = 6,
   height = NULL, collapsible = FALSE, collapsed = FALSE, extraBoxClass = NULL){
  fdRow(fdBox(..., title = title, footer = footer, status = status,
    solidHeader = solidHeader, background = background, width = width,
    height = height, collapsible = collapsible, collapsed = collapsed,
    extraBoxClass = extraBoxClass
  ))
}

#' Create a value box.
#'
#' A value box displays a value (usually a number) in large text, with a smaller
#' subtitle beneath, and a large icon on the right side. Value boxes are meant
#' to be placed in the main body of a dashboard.
#'
#' @inheritParams fdBox
#' @param value The value to display in the box. Usually a number or short text.
#' @param subtitle Subtitle text.
#' @param icon An icon tag, created by \code{\link[shiny]{icon}}.
#' @param color A color for the box. Valid colors are listed in
#'   \link{validColors}.
#' @param href An optional URL to link to.
#'
#' @family boxes
#' @seealso \code{\link{fdBox}} for usage examples.
#'
#' @export
fdValueBox <- function(value, subtitle, icon = NULL, color = "aqua", width = 4,
    href = NULL){
  # validateColor(color)
  # if (!is.null(icon)) tagAssert(icon, type = "i")

  boxContent <- div(class = paste0("small-box bg-", color),
    div(class = "inner",
      h3(value),
      p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon", icon)
  )

  if (!is.null(href))
    boxContent <- a(href = href, boxContent)

  div(class = if (!is.null(width)) paste0("col-sm-", width),
    boxContent
  )
}

#' Create an info box.
#'
#' An info box displays a large icon on the left side, and a title, value
#' (usually a number), and an optional smaller subtitle on the right side. Info
#' boxes are meant to be placed in the main body of a dashboard.
#'
#' @inheritParams fdBox
#' @param title Title text.
#' @param value The value to display in the box. Usually a number or short text.
#' @param subtitle Subtitle text (optional).
#' @param icon An icon tag, created by \code{\link[shiny]{icon}}.
#' @param color A color for the box. Valid colors are listed in
#'   \link{validColors}.
#' @param fill If \code{FALSE} (the default), use a white background for the
#'   content, and the \code{color} argument for the background of the icon. If
#'   \code{TRUE}, use the \code{color} argument for the background of the
#'   content; the icon will use the same color with a slightly darkened
#'   background.
#' @param href An optional URL to link to.
#'
#' @family boxes
#' @seealso \code{\link{fdBox}} for usage examples.
#'
#' @export
#' @example inst/examples/fdInfoBox.R
fdInfoBox <- function(title, value = NULL, subtitle = NULL,
  icon = fdIcon("bar-chart"), color = "aqua", width = 4, href = NULL,
  fill = FALSE) {

  # validateColor(color)
  # tagAssert(icon, type = "i")

  colorClass <- paste0("bg-", color)

  boxContent <- div(
    class = "info-box",
    class = if (fill) colorClass,
    span(
      class = "info-box-icon",
      class = if (!fill) colorClass,
      icon
    ),
    div(class = "info-box-content",
      span(class = "info-box-text", title),
      if (!is.null(value)) span(class = "info-box-number", value),
      if (!is.null(subtitle)) p(subtitle)
    )
  )

  if (!is.null(href))
    boxContent <- a(href = href, boxContent)

  div(class = if (!is.null(width)) paste0("col-sm-", width),
    boxContent
  )
}

