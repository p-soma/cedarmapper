# graphutils


#' @export
randGraphData<-function(n=20){
  nses = 1:n
  nodes = data.frame(name=nses, values=rnorm(length(nses)))
  # remember javascript is zero array based, and links reference index of nodes
  links = data.frame(source=sample(nses,n*2,replace=TRUE)-1,
                     target=sample(nses,n*2,replace=TRUE)-1)
  return(list("nodes"=nodes,"links"=links))
}
