# # example histogram equalization for iris
# 
# h <- hist(iris$Sepal.Length, breaks = length(iris$Sepal.Length))
# pmf <- h$counts/sum(h$counts)
# cdf <- cumsum(pmf)
# # cdf * n
# cdfn <- cdf * length(h$counts)
# #  fl_cdfn <- floor(cdfn)
# eq_hst_vals <- rep(cdfn, h$counts)
# eq_hst <- hist(eq_hst_vals, breaks = length(h$counts))
# 
# plot(h)
# plot(eq_hst)
# d <- iris$Sepal.Length
# equalize_hist <- function(d){
#   h <- hist(d, breaks = length(d))#breaks=bin_breaks)
#   cdf <- cumsum(h$counts)
#   slope <- sum(h$counts) / (max(d) - min(d))
#   cdfn <- (cdf / slope) + min(d)
#   eq_hst_vals <- rep(cdfn, h$counts)
#   eq_hst <- hist(eq_hst_vals, breaks=length(d))# breaks=bin_breaks)
#   return(eq_hst)
# }
# 
# names(eq_hst_vals) <-rownames(iris$Sepal.Length)
# eq_hst_vals
# eq <- equalize_hist(iris$Sepal.Length)#, bin_breaks = 50)
# plot(eq)
# h <- hist(iris$Sepal.Length, breaks = length(iris$Sepal.Length))#, breaks = length(iris$Sepal.Length))
# plot(cumsum(h$counts))
# plot(cumsum(eq$counts))
# 
# 
# library(EBImage)
# image = readImage("~/bay.png")
# grayImage <- channel(image,"gray")
# grayScaled = floor(grayImage * 255)
# h1 <- hist(grayScaled, breaks = length(grayScaled))
# h2 <- equalize_hist(grayScaled)
# plot(h1)
# plot(h2)
# h2
