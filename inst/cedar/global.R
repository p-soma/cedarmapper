# CEDAR application
# global.R  = global settings shared by all instances

library(cedar)
datasets = list()
data(chemdiab)
chemdiab  <- subset(chemdiab, select = -c(cc))

datasets[["Diabetes"]] <- chemdiab
datasets[["Circle"]]   <- circle_data(r=1, n=500)
datasets[["CircleRandomize_Gaussian"]]<- circle_data(r=1, n=500, randomize=TRUE)
datasets[["CircleSquishy"]]<- squishy_circle(r=1, n=500, squish=100)
datasets[["Y data"]] <- y_data(n=100)
datasets[["Spiral"]] <- spiral_data(200)
datasets[["Iris"]] <- read.csv("~/Documents/msu/pa_cmse/mapper_practice/iris.csv")


####### starting values
dataChoices           <- names(datasets)

# mapper 
partitionCountChoices <- c(1:20)
lenses = lense.table()

lenseChoices <- lenses$Name
# names(lenseChoices) <- lenses$Name
# <- c("single variable"="lense.projection","Density"="lense.density", "Eccentricity"="lense.eccentricity", "PCA"="lense.pca", "M distance"="lense.distance")

# f = match.fun(lenses[1,"fun"])

initVariableChoices   <- names(datasets[[1]])
# starting value for dataset d
d                     <- datasets[[1]]

gm <- graphmapper(dataset=d, lensefun=lense.projection, partition_count=NULL, overlap=NULL,  lenseparam="rw")
