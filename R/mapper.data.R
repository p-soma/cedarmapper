#' mapper.data.R
#' functions to extract information about a mapper object
#' 

has.groups <- function(gm){
  if(is.null(gm$groups)) return(FALSE)
  if(length(gm$groups) < 2) return(FALSE)
  return(TRUE)
}

# return a table of the variance for each variable for each group
#' @export
varTable <- function(gm, group_ids = c(1,2)){
  if(! has.groups(gm)) {return(NULL)}
  varFun <- function(varname){
    d1 = groupdata(gm,group_ids[1],varname)
    d2 = groupdata(gm,group_ids[2],varname)
    return(data.frame("var"=varname, "mean group 1"=mean(d1),  "variance group 1" = var(d1), "mean group 2"=mean(d2), "variance group 2" = var(d2)))
  }
  vtable = ldply(gm$selectedCols, varFun)
}

# returns a table of ks results for each variable in gm$d
#' @export
kstable <- function(gm, group_ids = c(1,2)){
  # requires the 'groups' of the gm object be set ahead of time
  # could have more than 2 , allow to select 2 groups 
  # if(!has.groups(gm)) stop("requires groups to run test") # raise exception need some groups!
  
  # inner function for apply
  ksfun <- function(varname) {
    # DEBUG 
    print(varname)
    
    # TODO this is duplicating the data in memory
    d1 = groupdata(gm,group_ids[1],varname)
    d2 = groupdata(gm,group_ids[2],varname)
    # suppress the ks test warnings because we know there will be ties
    kt = suppressWarnings(
      ks.test(d1,d2, alternatives = "two.sided", exact = FALSE) 
    )
    return(data.frame("var"=varname, "pvalue" = kt$p.value, "kstatistic" = kt$statistic))
  }

  ktable = ldply(mapper.numeric_cols(gm), ksfun)
  return(ktable[order(ktable$pvalue),])
}


# returns the data rows for a given group id
groupdata <- function(gm, group_id, varname = NULL){
  # TODO : add error testing
  nodedata(gm, gm$nodes[gm$groups[[group_id]]], varname)
}


####### helpers

is.mapper <- function(o){
  return((class(o)=="mapper"))
}

is.varname <- function(gm, varname){
  return( Reduce("&", (varname %in% names(gm$d))))
}

# this is dangerous practice
# but returns a variable name in the data set no matter what is sent
guaranteedVarname <- function(gm,  varname=NULL){
  if(!is.mapper(gm)) return(NULL)
  
  if (is.null(varname))   return(colnames(gm$d)[1])
  
  if( Reduce("&", (varname %in% colnames(gm$d)))) return(varname)
  
  return(names(gm$d)[1])
}

# return data rows by variable for given node
# a node is a list of data row IDs from gm$d, note a node ID (node 3)
#' @export
nodedata <- function(gm, nodes, varname=NULL){
  # DEBUG
  # print("node data")
  # print(nodes)
  # print(varname)
  
  if(!is.mapper(gm)) return (NULL)
  
  # unlisting potentially overlapping nodes, this works on single node, too
  rowids = unique(unlist(nodes))
  
  # if no variable name sent, return all columns
  if(is.null(varname)){
    return(gm$d[rowids,])
  } 
  else {
    # return only column(s) requested in varname
    # use reduce here to combine TRUES if varname is vector of names c("X", "Y")
    if( Reduce("&", (varname %in% colnames(gm$d)))){
      return(gm$d[rowids,varname])
    } else if( (varname %in% lenseChoices)){
    return(gm$lensevals[rowids,])
    }
  }
  return()
}

# returns one or more columns of data as listed in one or more partitions
#' @export
partitiondata <- function(gm, p, varname = NULL){
  if(!is.mapper(gm)) return (NULL)
  
  # convert list of partitions into single vector of row ids
  p = unique(unlist(p))
  
  # if no varname param, return all columns
  if(is.null(varname)){
    return(gm$d[gm$partitions[[p]], gm$selected_cols])
  } 
  else {
    # use reduce() here to allow a vector of column names
    if (Reduce("&", (varname %in% names(gm$d))))
      return(gm$d[gm$partitions[[p]],varname])
  }
  # bad variable name sent - error condition?
  return(NULL)
}


#' returns bar plot
#' @export 
#' 
factorBarPlot <- function(gm, varname, group_id = 1){
  x_label = varname
  y_label = "Frequency"
  d_group = groupdata(gm,group_id,varname)
  #  return(barplot(table(gm$d[varname]), xlab = x_label, ylab = y_label) )
  return(barplot(table(d_group), xlab = x_label, ylab = y_label) )
}
