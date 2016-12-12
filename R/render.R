#' Render dashboard in Alteryx Composer
#'
#' @param x an object of class htmlwidget or shiny.tag
#' @param libdir directory to copy js/css dependencies to
#' @param nOutput node to pipe output back into Alteryx
#' @param debug only for use in development
#' @import htmltools htmlwidgets
#' @export
fdRender <- function(x, libdir = NULL, nOutput = 3, debug = FALSE,
    selfcontained = TRUE){
  if (!inAlteryx() && !debug){
    return(htmltools::browsable(x))
  }
  if (!is.null(libdir)){
    rendered <- renderTags(x)
    deps <- lapply(rendered$dependencies, function(dep) {
      dep <- copyDependencyToDir(dep, libdir, FALSE)
      dep <- makeDependencyRelative(dep, libdir, FALSE)
      dep
    })
    scripts = unlist(lapply(deps, function(x){
      normalizePath(file.path(libdir, x$src, x$script), winslash = '/')
    }))

    styles = unlist(lapply(deps, function(x){
      normalizePath(file.path(libdir, x$src, x$style), winslash = '/')
    }))
  } else {
    options(htmlwidgets.copybindingdir = FALSE)
    #unlockBinding("getDependency", env = asNamespace("htmlwidgets"))
    #assign("getDependency", getDependency2,
    #  envir = asNamespace("htmlwidgets")
    #)
    rendered <- renderTags(x)
    deps = rendered$dependencies
    scripts = unlist(lapply(deps, function(x){
      normalizePath(file.path(x$src$file, x$script), winslash = '/')
    }))

    styles = unlist(lapply(deps, function(x){
      normalizePath(file.path(x$src$file, x$style), winslash = '/')
    }))
  }
  if (!selfcontained){
    html_deps = sprintf(
      "<htmldependency src='%s'></htmldependency>",
      c(scripts, styles)
    )
    html_content = sprintf(
      '<htmlcontent><![CDATA[ %s ]]></htmlcontent>',
      rendered$html
    )
  } else {
    html_deps <- NULL
    js <- lapply(scripts, function(x){paste(readLines(x, warn = F), collapse = '\n')})
    #js2 <- do.call(function(...){paste(..., collapse = '\n')}, js)
    #js3 <- paste(c("<div><script type='text/javascript' charset='UTF-8'>", utils::URLencode(js2), "</script></div>"), collapse = '\n')

    js2 <- paste("<div><script type='text/javascript' charset='UTF-8'>",      js, "</script></div>")

    js3 <- do.call(function(...){paste(..., collapse = '\n')}, as.list(js2))

    # js2 <- lapply(js, function(x){
    #   tags$script(src = paste0(
    #     "data:application/x-javascript;base64,",
    #     base64encode(x)
    #   ))
    # })
    #
    # js3 <- do.call(function(...)paste(..., collapse = '\n'), js2)
    css <- lapply(styles, function(x){paste(readLines(x, warn = F), collapse = '\n')})
    css2 <- do.call(function(...){paste(..., collapse = '\n')}, css)
    css3 <- paste(c("<style>", css2, "</style>"), collapse = '\n')
    tpl <- '<htmlpassthrough><![CDATA[ \n <div>%s\n  %s\n %s\n</div>\n ]]></htmlpassthrough>'
    #tpl2 <- '<div>%s\n  %s\n %s\n</div>'
    tpl2 <- '<htmlcontent><![CDATA[ %s\n  %s\n %s\n ]]></htmlcontent>'
    html_content = enc2utf8(sprintf(
      tpl2,
      js3,
      rendered$html,
      css3
    ))
  }
  output = c(
    html = paste(c(html_deps, html_content), collapse = '\n')
  )
  if (inAlteryx()){
    f <- get("write.Alteryx", envir = asNamespace("AlteryxRDataX"))
    f(output, nOutput)
  } else {
    output[['html']]
  }
}

inAlteryx <- function(){
  'package:AlteryxRDataX' %in% search()
}

#' @import yaml
getDependency2 <- function(name, package = name){
  config = sprintf("htmlwidgets/%s.yaml", name)
  jsfile = sprintf("htmlwidgets/%s.js", name)

  config = yaml::yaml.load_file(
    system.file(config, package = package)
  )
  widgetDep <- lapply(config$dependencies, function(l){
    l$src = system.file(l$src, package = package)
    do.call(htmltools::htmlDependency, l)
  })

  # Create a dependency that will cause the jsfile and only the jsfile (rather
  # than all of its filesystem siblings) to be copied
  #bindingDir <- tempfile("widgetbinding")
  #dir.create(bindingDir, mode = "0700")
  #file.copy(system.file(jsfile, package = package), bindingDir)
  bindingDir = system.file('htmlwidgets', package = package)
  bindingDep <- htmltools::htmlDependency(paste0(name, "-binding"),
    packageVersion(package),
    bindingDir,
    script = basename(jsfile)
  )

  c(
    list(htmltools::htmlDependency("htmlwidgets",
      packageVersion("htmlwidgets"),
      src = system.file("www", package="htmlwidgets"),
      script = "htmlwidgets.js"
    )),
    widgetDep,
    list(bindingDep)
  )
}
