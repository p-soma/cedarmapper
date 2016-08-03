# lenseFunctions.R
# CEDAR project
# collection of lense functions for use, all accept a data.frame with named rows, and return
# a named vector of the same length

#' lense registry, for user selection of lenses
#' @export
lense.table <- function(){
  options(stringsAsFactors = FALSE)
  lenses = data.frame("Name"="Projection", "fun" =  "lense.projection", params="coordinate", desc="Selected Coordinate")

  lenses = data.frame(rbind(lenses,
            data.frame("Name"="PCA",        "fun" =  "lense.pca", params="", desc="First principle Component"))
  )
            
  lenses = data.frame(rbind(lenses,
            data.frame("Name"="Mahalanobis Distance", "fun" =  "lense.mahalanobis", params="", desc="Mahalanobis Distance"))
  )
  
  lenses = data.frame(rbind(lenses,
     data.frame("Name"="Eccentricity", "fun" =  "lense.eccentricity", params="n = 1 or 2", desc="Eccentricity")))
  
  lenses = data.frame(rbind(lenses,
     data.frame("Name"="Density", "fun" =  "lense.density", params="sigma=0.5 or 1", desc="Topological Density of data points")))

  
  rownames(lenses) <- lenses[,"Name"]

  return(lenses)
}

#' Mapper lense using one or more colummns of data, e.g projection by coordinate(s)
#' @family lenses
#' @param coordinate string name of column, or vector of column names for n-dimensional 
#' @returns vector of same length as data, with rownames preserved
#' @export
lense.projection <- function(gm,lenseparam=NULL ){
  coordinate <- lenseparam
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

lense.2dprojection <- function(gm,lenseparam=NULL ){
  coordinates <- lenseparam
  # returns a vector variable, defaults to the first column
  if (is.null(coordinate)) { coordinate= names(gm$d)[1] }
  
  #if (! coordinate %in% colnames(gm$d)){
  #  return(NULL)
  # }
  
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

#' Mapper lense using first principle component
#' @family lenses
#' @param none
#' @export
lense.pca <- function(gm,lenseparam=NULL) {
  pca = prcomp(gm$d, retx=TRUE, center=TRUE, scale. = TRUE)
  L = pca$x[,"PC1"]
  names(L) <- rownames(gm$d)
  return(L)
}


#' Mapper lense calculating the Mahalanobis distance'
#' @param gm GraphMapper object
#' @export
lense.mahalanobis <- function(gm,lenseparam=NULL) {
  L=mahalanobis( scale(gm$d, center=TRUE,scale=TRUE ), center=colMeans(gm$d), cov=cov(gm$d))
  names(L) <- rownames(gm$d)
  return(L)
}



#' Eccentrity value of each row of data
#' @family lenses
#' @param n 1 or 2 exponent and divisor
#' @export
lense.eccentricity <- function(gm, lenseparam=1){ # n = 1 or 2
  n <- lenseparam
  n <- 1
  d <- getdistance(gm)
  L <- apply(as.matrix(d**n),1,mean)^(1/n)
  names(L) <- rownames(gm$d)
  return(L)
}




#' Mapper lense using Topological density  of each point
#' @family lenses
#' @export
lense.density <- function(gm, lenseparam=1.0){
  sigma <- lenseparam
  d <- getdistance(gm)
  d.exp <- exp((-1 * (d^2)) / (2 * sigma^2))
  L <- (apply(as.matrix(d.exp),1,mean))

  # names(L) <- rownames(gm$d)  # this is done in the partition function
  return(L)
}


#' Retrieve or calculate distance matrix of data in graph mapper object
# several lense functions need a distance matrix, 
# this checks if there is a distance matrix, and if not makes one
# this is a stub currently that should save the distance matrix in the environment for re-reading
#' @param gm
#' @export
getdistance <- function(gm) {
  if (is.null(gm[["distance"]])){
    return(dist(scale(gm$d),method="euclidean", upper=FALSE))}
  else {
    return(gm$distance)
  }
}