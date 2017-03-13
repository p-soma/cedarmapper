# cedarGraph.R
#' @import htmlwidgets
#' @export
cedarGraph <- function(LinksDF, NodesDF, width ="100%", height = "250") {
  options = list()

  # create widget
  htmlwidgets::createWidget(
    name = "cedarGraph",
    x = list(links = LinksDF, nodes = NodesDF, options = options),
    width = width,
    height = height,
    #sizingPolicy(padding = 0, browser.fill = FALSE),
                 # browser.defaultWidth = '100%', browser.defaultHeight = '500'),
    package = "cedar"
  )
}

# output for ui.R, calls shinyWidgetOutput to wrap the html
#' @export
cedarGraphOutput <- function(outputId, width = "100%", height = "250") {
  shinyWidgetOutput(outputId, "cedarGraph", width, height,
                    package = "cedar")
}

# output for server.R; runs the expr as function in reactive context;
# then
#' @export
renderCedarGraph <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  shinyRenderWidget(expr, cedarGraphOutput, env, quoted = TRUE)
}
