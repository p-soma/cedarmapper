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
 
#  two intertwined spirals
set.seed("1")
t <- runif(200, min=1, max=6.3) # theta
datasets[["Spiral"]] <- data.frame( x = c( t*cos(t), -t*cos(t) ), y = c( t*sin(t), -t*sin(t) ) )


####### starting values
dataChoices           <- names(datasets)

# mapper 
clusterIndexChoices   <- c( "gap", "all", "alllong", "kl", "ch", "hartigan", "ccc", "scott", "marriot", "trcovw", "tracew","friedman", "rubin", "cindex", "db", "silhouette", "duda", "pseudot2",  "beale", "ratkowsky", "ball", "ptbiserial", "frey", "mcclain", "gamma", "gplus", "tau", "dunn", "hubert", "sdindex", "dindex", "sdbw")
partitionCountChoices <- c(3:20)
lenseChoices          <- c("single variable"="simple_lense","Kernel Density"="lense.density", "PCA"="lense.pca", "M distance"="lense.distance")
initVariableChoices   <- names(datasets[[1]])
# starting value for dataset d
d                     <- datasets[[1]]

gm <- graphmapper(dataset=d, lensefun=simple_lense, partition_count=NULL, overlap=NULL, partition_method="single", index_method="gap", lenseparam="rw")

