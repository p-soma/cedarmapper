# CEDAR application
# global.R  = global settings shared by all instances

library(cedar)
datasets = list()
data(chemdiab)
chemdiab  <- subset(chemdiab, select = -c(cc))

datasets[["Diabetes"]] <- chemdiab
datasets[["Circle"]]   <-circle_data(r=1, n=60)
datasets[["Circle500"]]<-circle_data(r=1, n=500)
datasets[["FuzzyCircle"]] <- circle_data(r=1, n=200, randomize = TRUE)


####### starting values
dataChoices           <- names(datasets)

# mapper 
clusterIndexChoices   <- c( "gap", "all", "alllong", "kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew","friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2",  "beale", "ratkowsky", "ball", "ptbiserial", "frey", "mcclain", "gamma", "gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw")
partitionCountChoices <- c(3:20)
lenseChoices          <- c("single variable"="simple_lense","Kernel Density"="lense.density", "PCA"="lense.pca", "M distance"="lense.distance")
initVariableChoices   <- names(datasets[[1]])
# starting value for dataset d
d                     <- datasets[[1]]