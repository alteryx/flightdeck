library(flightdeck)
library(htmltools)
pal <- fdPalette()

displayColor <- function(col){
  style= paste(
    'text-align: center; line-height: 120px; height: 120px; width: 100%; background:',
    pal[[col]]
  )
  fdColumn(c(xs = 3), div(col, style = style), style = 'margin-bottom: 10px;')
}

myColors <- do.call(tagList, lapply(names(pal), displayColor))
fdPreview(myColors, title = "Alteryx Color Palette")
