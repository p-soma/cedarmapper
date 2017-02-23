# lenseFunctions.R
# CEDAR project

#' TODO getdistance() function ALWAYS SCALES !  
#' 
#' 

# collection of lense functions for use, all accept a data.frame with named rows, and return
# a named vector of the same length

#' lense registry, for user selection of lenses
#' @export
lense.table <- function(){
  options(stringsAsFactors = FALSE)
  lenses = data.frame("Name"="Projection", "fun" =  "lense.projection", params="coordinate", desc="Selected Coordinate")

  lenses = data.frame(rbind(lenses,
     data.frame("Name"="PCA1", "fun" =  "lense.pca1", params="", desc="First principle Component"))
  )
  
  lenses = data.frame(rbind(lenses,
     data.frame("Name"="PCA2", "fun" =  "lense.pca1", params="", desc="First principle Component"))
  )
  
  lenses = data.frame(rbind(lenses,
     data.frame("Name"="Eccentricity", "fun" =  "lense.eccentricity", params="a positive integer 1 or 2", desc="Exponent")))
  
  lenses = data.frame(rbind(lenses,
     data.frame("Name"="Density", "fun" =  "lense.density", params="sigma=0.5 or 1", desc="Point-wise Gaussian kernel width")))

  lenses = data.frame(rbind(lenses,
     data.frame("Name"="Constant", "fun" =  "lense.constant", params="", desc=" ")))
  
  rownames(lenses) <- lenses[,"Name"]

  return(lenses)
}

#' Mapper lense that returns a constant.  This not affect partitioning; all items go into the first partition
#' @family lenses 
#' @param none
#' @returns constant 0
#' @export
lense.constant <- function(d,lenseparam = NULL,distmat=NULL){
  c <- 0
  L <- rep(c,times = nrow(d))
  # names(L) <- rownames(d)  # this is done in the partition function
  return(L)
}


#' Mapper lense using one or more colummns of data, e.g projection by coordinate(s)
#' @family lenses
#' @param coordinate string name of column, or vector of column names for n-dimensional 
#' @returns vector of same length as data, with rownames preserved
#' @export
lense.projection <- function(d,lenseparam=NULL,distmat=NULL ){
  coordinate <- lenseparam
  # returns a vector variable, defaults to the first column
  if (is.null(coordinate)) { coordinate= names(d)[1] }
  
#  if (! coordinate %in% colnames(d)){
#    return(NULL)
#  }
  
  # get single column
  L <- d[,coordinate]
  # assign rownames
  if (is.null(dim(L))){
    # single dimension vector
    names(L) <- rownames(d)
  } else {
    # multidimension vector use rownames
    rownames(L) <- rownames(d)  
  }
  return(L)
}


#' Mapper lense using first principle component
#' @family lenses
#' @param none
#' @export
lense.pca1 <- function(d,lenseparam=NULL,distmat=NULL) {
  pca = prcomp(d, retx=TRUE, center=TRUE, scale. = TRUE)
  L = pca$x[,"PC1"]
  return(L)
}

#' Mapper lense using second principle component
#' @family lenses
#' @param none
#' @export
lense.pca2 <- function(d,lenseparam=NULL,distmat=NULL) {
  pca = prcomp(d, retx=TRUE, center=TRUE, scale. = TRUE)
  L = pca$x[,"PC2"]
  return(L)
}



#' Eccentrity value of each row of data
#' @family lenses
#' @param n 1 or 2 exponent and divisor
#' @export
lense.eccentricity <- function(d, lenseparam=1,distmat=NULL){ # n = 1 or 2
  n <- lenseparam
  n <- 1
  if(is.null(distmat)) {
    distmat <- getdistance(d)
  }
  L <- apply(as.matrix(distmat**n),1,mean)^(1/n)
  # names(L) <- rownames(d) # this is done in the partition function
  return(L)
}


#' Mapper lense using Topological density  of each point
#' @family lenses
#' @export
lense.density <- function(d, lenseparam=1.0,distmat=NULL){
  sigma <- lenseparam
  if(is.null(distmat)) {
    distmat <- getdistance(d)
  }
  d.exp <- exp((-1 * (distmat^2)) / (2 * sigma^2))
  L <- (apply(as.matrix(d.exp),1,mean))

  # names(L) <- rownames(d)  # this is done in the partition function
  return(L)
}


#' Retrieve or calculate distance matrix of data in graph mapper object
# several lense functions need a distance matrix, 
# this checks if there is a distance matrix, and if not makes one
# this is a stub currently that should save the distance matrix in the environment for re-reading
#' @param d data.frame
#' @return distance matrix
#' @export
getdistance <- function(d) {
    # to scale, or not to scale, that is the question
    # and the answer is scale here by default, 
    # to use non scaled or different kind of distance matrix, send as a parameter to lense function
    return(dist(scale(d),method="euclidean", upper=FALSE))
}