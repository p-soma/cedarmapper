# lenseFunctions.R
# CEDAR project
# collection of lense functions for use, all accept a data.frame with named rows, and return
# a named vector of the same length

# lense registry is used for user selection of lenses
# the other method is to use args(fun) => 
assign("lense.registry", list(), envir = .GlobalEnv)



#' Mapper lense using one or more colummns of data, e.g projection by coordinate(s)
#' @family lenses
#' @param coordinate string name of column, or vector of column names for n-dimensional 
#' @returns vector of same length as data, with rownames preserved
#' @export
lense.projection = function(gm,coordinate=NULL ){
  # returns a vector variable, defaults to the first column
  if (is.null(coordinate)) { coordinate= names(gm$d)[1] }
  
  if (! coordinate %in% colnames(gm$d)){
    return(NULL)
  }
  
  # get single column
  L <- gm$d[,coordinate]
  # assign rownames
  if (is.null(dim(L))){
    # single dimension vector
    names(L) <- rownames(gm$d)
  } else {
    # multidimension vector use rownames
    rownames(L) <- rownames(gm$d)  
  }
  return(L)
}
lense.registry[["Projection"]] <- list("function" = lense.projection, params="coordinate", desc="First Principle Component") 


#' Mapper lense using first principle component
#' @family lenses
#' @param none
#' @export
lense.pca <- function(gm) {
  L = prcomp(d, retx=TRUE, center=TRUE, scale. = TRUE)
  names(L) <- rownames(gm$d)
  return(L)
}
lense.registry[["PCA"]] <- list("function" = lense.pca, params=NULL, desc="First principle Component") 




#' Mapper lense calculating the Mahalanobis distance'
#' @param gm GraphMapper object
#' @export
lense.mahalanobis <- function(gm) {
  L=mahalanobis( scale(gm$d, center=TRUE,scale=TRUE ), center=colMeans(gm$d), cov=cov(gm$d))
  names(L) <- rownames(gm$d)
  return(L)
}
lense.registry[["Mahalanobis Distance"]] <- list("function" = lense.mahalanobis, params=NULL, desc="Mahalanobis Distance") 



#' Eccentrity value of each row of data
#' @family lenses
#' @param n 1 or 2 exponent and divisor
#' @export
lense.eccentricity <- function(gm, n=1){ # n = 1 or 2
  d = getdistance(gm)
  L = apply(as.matrix(d**n),1,mean)^(1/n)
  names(L) <- rownames(gm$d)
  return(L)
}
lense.registry[["Eccentricity"]] <- list("function" = lense.eccentricity, params=NULL, desc="Eccentricity") 




#' Mapper lense using Topological density  of each point
#' @family lenses
#' @export
lense.density <- function(gm, sigma=1.0){
  d = getdistance(gm)
  d.exp = exp((-1 * (d^2)) / (2 * sigma^2))
  L = (apply(as.matrix(d.exp),1,mean))
  names(L) <- rownames(gm$d)
  return(L)
}
lense.registry[["Eccentricity"]] <- list("function" = lense.mahalanobis, params=NULL, desc="Mahalanobis Distance of each row") 


#' Retrieve or calculate distance matrix of data in graph mapper object
# several lense functions need a distance matrix, 
# this checks if there is a distance matrix, and if not makes one
# this is a stub currently that should save the distance matrix in the environment for re-reading
#' @param gm
#' @export
getdistance <- function(gm) {
  if (is.null(gm[["distance"]])){
    return(dist(gm$d,method="euclidean", upper=FALSE))}
  else {
    return(gm$distance)
  }
}