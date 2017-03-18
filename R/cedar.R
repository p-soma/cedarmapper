#' cedar: A package for exploring data using the mapper topological analysis 
#' described in Topological Methods for the Analysis of High Dimensional Data Sets and 3D Object Recognition (Singh et al 2007)
#'
#' The cedar package provides an object for storing results of the mapper pipeline, 
#' a collection of filter functions or lenses for dimension reduction, 
#' and a Shiny application to display the resulting graph

#' @docType package
#' @name cedar

#' @export
runCedar <- function() {
  appDir <- system.file("cedar",  package = "cedar")
  if (appDir == "") {
    stop("Could not find cedar app directory. Try re-installing `cedar`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, launch.browser = TRUE, display.mode = "normal")
}