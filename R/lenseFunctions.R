# lenseFunctions.R
# CEDAR project
# collection of lense functions for use, all accept a data.frame with named rows, and return
# a named vector of the same length

# lense registry is used for user selection of lenses
# the other method is to use args(fun) => 
lense.registry <<- list()

#' @family lenses
#' @export
lense.projection = function(gm,coordinate=NULL ){
  # returns a single variable, defaults to the first column
  if (is.null(coordinate)) { coordinate= names(gm$d)[1] }
  
  if (! coordinate %in% colnames(gm$d)){
    return(NULL)
  }
  
  # get single column
  L <- gm$d[,coordinate]
  # assign rownames
  names(L) <- rownames(gm$d)
  
  return(L)
}

lense.registry[["Projection"]] <<- list("function" = lense.projection, params=coordinate, desc="First principle Component") 

#' @family lenses
#' @param none
#' @export
lense.pca <- function(gm) {
  d.pca = prcomp(d, retx=TRUE, center=TRUE, scale. = TRUE)
  data.frame(L=d.pca$x[,"PC1"], ID=rownames(d), stringsAsFactors = FALSE) 
}
lense.registry[["PCA"]] = list("function" = lense.pca, params=NULL, desc="First principle Component") 

#' @export
lense.mahalanobis <- function(gm) {
  data.frame(L=mahalanobis(scale(d, center=TRUE,scale=TRUE ), center=colMeans(d), cov=cov(d)), stringsAsFactors = FALSE)
}

lense.registry[["Mahalanobis Distance"]] <<- list("function" = lense.mahalanobis, params=NULL, desc="Mahalanobis Distance of each row") 

#' Eccentrity value of each row of data
#' @family lenses
#' @param n 1 or 2 exponent and divisor
#' @export
lense.eccentricity <- function(gm, n=1){ # n = 1 or 2
  d = getdistance(gm)
  d = d**n
  return(apply(as.matrix(d),1,mean)^(1/n))
}

lense.registry[["Eccentricity"]] <<- list("function" = lense.eccentricity, params=NULL, desc="Eccentricity") 

#' @family lenses
#' @export
lense.density <- function(gm, sigma=1.0){
  d = getdistance(gm)
  d.exp = exp((-1 * (d^2)) / (2 * sigma^2))
  lense.vector = (apply(as.matrix(d.exp),1,mean))
  return(data.frame(L=lense.vector, ID=rownames(gm$d), stringsAsFactors = FALSE))
}

lense.registry[["Eccentricity"]] <<- list("function" = lense.mahalanobis, params=NULL, desc="Mahalanobis Distance of each row") 



# several lense functions need a distance matrix, 
# this checks if there is a distance matrix, and if not makes one
# this is a stub currently that should save the distance matrix in the environment for re-reading
#' @family lenses
#' @export
getdistance <- function(gm) {
  if (is.null(gm[["distance"]])){
    return(dist(gm$d,method="euclidean", upper=FALSE))}
  else {
    return(gm$distance)
  }
}