#' Synthetic data for use with Mapper examples


# generate x,y  points on circle
# example c = circle_data(120,randomize=TRUE); plot(c)
#' @export
circle_data <- function(r=1, n=60, randomize=FALSE) {
  if (randomize){
    angles = (runif(n, -1*r, 1*r)) * pi
  } else {
    # evenly spaced arcs
    angles = (0:(n-1))*2*(pi/n)    
  }
  
  d = data.frame(X=cos(angles)*r, Y=sin(angles)*r)
  # add an ID number - obsoleted; use rownames instead
  #d = data.frame(d,ID=seq.int(n))
  rownames(d) = 1:nrow(d)
  return(d)
}


#' #' create x,y data in shape of reclining Y
#' @export
y_data <- function(n=100){
  r1 = runif(n,-1,0)
  r2 = runif(n,-1,0)
  r3 = runif(n,0,1)
  
  d1 = cbind(x=r1, y=-r1)
  d2 = cbind(x=r2,y=r2)
  d3 = cbind(x=r3,y=0)
  return(data.frame(rbind(d1,d2,d3)))
}

