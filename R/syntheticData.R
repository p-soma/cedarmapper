#' Synthetic data for use with Mapper examples


# generate x,y  points on circle
# example c = circle_data(120,randomize=TRUE); plot(c)
#' @export
circle_data <- function(r=1, n=60, randomize=FALSE) {
  if (randomize){
    angles = rnorm(n,0, sqrt(pi)/2.5)
    # angles = (runif(n, -1*r, 1*r)) * pi
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

squishy_circle <- function(r=1, n=60, randomize=FALSE, squish=100){
  d = as.matrix(circle_data(r, n, randomize)) %*% matrix(c(1,0,0,squish),nrow=2,ncol=2)
  d = data.frame("X" = d[,1], "Y" = d[,2])
  rownames(d) = 1:nrow(d)
  return(d)
 
}

#' create x,y data in shape of reclining Y
#' @export
y_data <- function(n=100){
  r1 = runif(n,-1,0)
  r2 = runif(n,-1,0)
  r3 = runif(n,0,1)
  
  d1 = cbind(x=r1, y=-r1)
  d2 = cbind(x=r2,y=r2)
  d3 = cbind(x=r3,y=0)
  d = data.frame(rbind(d1,d2,d3))
  rownames(d) = 1:nrow(d)
  return(d)
}

#' Spiral 
#' @export
spiral_data <- function(n=200){
  set.seed("1")
  t <- runif(n, min=1, max=6.3) # theta
  d = data.frame( x = c( t*cos(t), -t*cos(t) ), y = c( t*sin(t), -t*sin(t) ) )
  rownames(d) = 1:nrow(d)
  return(d)
}
