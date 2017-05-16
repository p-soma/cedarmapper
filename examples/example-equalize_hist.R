# # # example histogram equalization for iris
# # 
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
# nbreaks = 10
# equalize_hist <- function(d){
#   h <- hist(d, breaks = length(d))#breaks=bin_breaks)
#   cdf <- cumsum(h$counts)
#   slope <- sum(h$counts) / (max(d) - min(d))
#   cdfn <- (cdf / slope) + min(d)
#   eq_hst_vals <- rep(cdfn, h$counts)
#   eq_hst <- hist(eq_hst_vals, breaks=length(d))# breaks=bin_breaks)
#   return(eq_hst_vals)
# }
# 
# 
# 
# names(eq_hst_vals) <-rownames(iris$Sepal.Length)
# eq_hst_vals
# eq <- equalize_hist_bins(iris$Sepal.Length, nbreaks = 50)
# plot(eq)
# h <- hist(iris$Sepal.Length, breaks = 50)#, breaks = length(iris$Sepal.Length))
# plot(cumsum(h$counts))
# plot(cumsum(eq$counts))
# 
# 
# library(EBImage)
# image = readImage("~/bay.png")
# grayImage <- channel(image,"gray")
# grayScaled = floor(grayImage * 255)
# h1 <- hist(grayScaled, breaks = 50)
# h2 <- equalize_hist(grayScaled, nbreaks = 50)
# plot(h1)
# plot(h2)
# hist(h2)
# h2
# h1$breaks
# h2$breaks
# h1$counts
# h2$counts
# 
# equalize_hist <- function(d,nbreaks){
#   h <- hist(d, breaks = nbreaks)#breaks=bin_breaks)
#   cdf <- cumsum(h$counts)
#   slope <- sum(h$counts) / (max(d) - min(d))
#   cdfn <- (cdf / slope) + min(d)
#   breaks <- c(min(d), cdfn[-length(cdfn)]+ diff(cdfn)/2, max(d))
#   df <- data.frame(h$breaks,breaks)
#   d <- sort(d)
#   equalized_vals <- vector()
#   for (i in 1:(nrow(df)-1)){
#     m = (df[i,2] - df[i+1,2]) / (df[i,1] - df[i+1,1])
#     b = df[i,2] - (m * df[i,1])
#     # print("before")
#     # print(d[d > df[i,1] & d <= df[i+1,1]])
#     nvals <- d[d > df[i,1] & d <= df[i+1,1]]*m + b
#     # print("after")
#     # print(d[d > df[i,1] & d <= df[i+1,1]])
#     equalized_vals <- c(equalized_vals, nvals)
#   }
#   return(equalized_vals)
# }
# test <- equalize_hist_bins(iris$Sepal.Length, nbreaks = 10)
# 
# eq_hst$breaks
# eq_hst$counts
# h$breaks
# h$counts
# h$mids
# eq_hst$mids
# sort(d)
# d <- iris$Sepal.Length
# 
# 
# 
# datasets[["Circle"]]   <- circle_data(r=1, n=500)
# circle <- datasets[["Circle"]]
# d <- circle$X
# nbreaks <- 10
# equalize_hist <- function(d,nbreaks){
#   orig_breaks <- seq(from = min(d), to = max(d), by = (max(d) - min(d))/nbreaks)
#   h <- hist(d, breaks = orig_breaks)
#   cdf <- cumsum(h$counts)
#   slope <- sum(h$counts) / (max(d) - min(d))
#   cdfn <- (cdf / slope) + min(d)
#   
#   breaks <- c(h$breaks[1],cdfn[-length(cdfn)],h$breaks[length(h$breaks)])
#   
#   df <- data.frame(h$breaks,breaks)
# 
#   d <- sort(d)
#   
#   # map old data to new values
#   equalized_vals <- vector()
#   names(equalized_vals) <- names(d)
#   i = 1
#   m = (df[i,2] - df[i+1,2]) / (df[i,1] - df[i+1,1])
#   b = df[i,2] - (m * df[i,1])
#   nvals <- d[d >= df[i,1] & d <= df[i+1,1]]*m + b
#   equalized_vals <- c(equalized_vals, nvals)
#   
#   for (i in 2:(nrow(df)-1)){
#     m = (df[i,2] - df[i+1,2]) / (df[i,1] - df[i+1,1])
#     b = df[i,2] - (m * df[i,1])
#     nvals <- d[d > df[i,1] & d <= df[i+1,1]]*m + b
#     equalized_vals <- c(equalized_vals, nvals)
#   }
#   eq_hst <- hist(equalized_vals, breaks = nbreaks)
#   # plot(h)
#   plot(eq_hst)
# 
# 
#   return(equalized_vals)
# }



x_eq <- equalize_hist(circle$X, nbreaks = 10)
y_eq <- equalize_hist(circle$Y, nbreaks = 10)




dat_sort <- data.frame(x= sort(circle$X), y= sort(circle$Y))





library(ggplot2)
datasets[["Circle"]]   <- circle_data(r=1, n=500)
circle <- datasets[["Circle"]]
d <- circle$X
nbreaks <- 10
equalize_hist <- function(d,nbreaks){
  orig_breaks <- seq(from = min(d), to = max(d), by = (max(d) - min(d))/nbreaks)
  h <- hist(d, breaks = orig_breaks, plot = FALSE)
  cdf <- cumsum(h$counts)
  cdf_len <- length(cdf)
  slope <- cdf[cdf_len] / (max(d) - min(d))
  cdfn <- (cdf / slope) + min(d)
  
  breaks <- c(orig_breaks[1], cdfn[-cdf_len], orig_breaks[nbreaks+1])
  
  breaks_df <- data.frame(orig_breaks,breaks)
  
  # map old data to new values
  d_eq <- d
  i = 1
  m = (breaks_df[i,2] - breaks_df[i+1,2]) / (breaks_df[i,1] - breaks_df[i+1,1])
  b = breaks_df[i,2] - (m * breaks_df[i,1])
  d_eq[d >= breaks_df[i,1] & d <= breaks_df[i+1,1]] <- d[d >= breaks_df[i,1] & d <= breaks_df[i+1,1]]*m + b
  
  for (i in 2:(nrow(breaks_df)-1)){
    m = (breaks_df[i,2] - breaks_df[i+1,2]) / (breaks_df[i,1] - breaks_df[i+1,1])
    b = breaks_df[i,2] - (m * breaks_df[i,1])
    d_eq[d > breaks_df[i,1] & d <= breaks_df[i+1,1]] <- d[d > breaks_df[i,1] & d <= breaks_df[i+1,1]]*m + b
  }
  return(d_eq)
}
library(ggplot2)
# 100% bin count
x_eq_100 <- equalize_hist(circle$X, nbreaks = 500)

# 50% bin count
x_eq_50 <- equalize_hist(circle$X, nbreaks = 250)

# 10% bin count
x_eq_10 <- equalize_hist(circle$X, nbreaks = 50)


# color visualization
dat <- data.frame(x= circle$X, y= circle$Y)

qplot(x,y, data=dat, colour=circle$X, main = "Color by original X vals") + scale_colour_gradient(low="red", high="blue")

qplot(x,y, data=dat, colour=x_eq_100, main = "Color by equalized X vals 100% bin") + scale_colour_gradient(low="red", high="blue")

qplot(x,y, data=dat, colour=x_eq_50, main = "Color by equalized X vals 50% bin") + scale_colour_gradient(low="red", high="blue")

qplot(x,y, data=dat, colour=x_eq_10, main = "Color by equalized X vals 10% bin") + scale_colour_gradient(low="red", high="blue")


# compare cdf of hist counts from original to equalized histogram
# original
plot(cumsum(hist(d)$counts))
# equalized 100% bin
plot(cumsum(hist(x_eq_100)$counts))

# equalized 50% bin
plot(cumsum(hist(x_eq_50)$counts))

# equalized 10% bin
plot(cumsum(hist(x_eq_10)$counts))

# compare histograms of original to equalized values
hist(circle$X, breaks= nbreaks)
hist(x_eq_100, breaks= nbreaks)
hist(x_eq_50, breaks= nbreaks)
hist(x_eq_10, breaks= nbreaks)



# image example
library(EBImage)
image = readImage("~/bay.png")
grayImage <- channel(image,"gray")
grayScaled = floor(grayImage * 255)
h1 <- hist(grayScaled, breaks = 50)
h2 <- equalize_hist(grayScaled, nbreaks = 50)


plot(h1)
hist(h2)


# categorical example 
species <- as.numeric(iris$Species)
species_eq <- equalize_hist(species, nbreaks = 3)
hist(species)
hist(species_eq)

n_points <- 200
perc_breaks <- .2
n_breaks <- ceiling(n_points*perc_breaks)

# bimodal, each mode normally distributed
ex1 <- c(rnorm(n= n_points/2, mean=1, sd=1), rnorm(n = n_points/2, mean= 1000, sd= 1))

hist(ex1, breaks = n_breaks)
ex1_eq <- equalize_hist(ex1, nbreaks=n_breaks)
hist_ex1_eq <- hist(ex1_eq, breaks= n_breaks)


ex1_lower <- ex1[ex1 < 500]
ex1_upper <- ex1[ex1 > 500]
hist(ex1_lower, breaks=100)
hist(ex1_upper, breaks=100)



# unimodal normal distribution
ex2 <- rnorm(100, mean=1, sd=1)
hist(ex2)
ex2_eq <- equalize_hist(ex2, nbreaks=100)
hist_ex2_eq <- hist(ex2_eq, breaks=100)
plot(cumsum(hist_ex2_eq$counts))
ex2_eq


# unimodal with outlier
ex3 <- c(rnorm(n= 100, mean=1, sd=1), 1000)
hist(ex3, breaks = 100)
ex3_eq <- equalize_hist(ex3, nbreaks=100)
hist_ex3_eq <- hist(ex3_eq, breaks= 100)


ex4 <- c(rnorm(n= 100, mean=1000, sd=1), 0)
hist(ex4, breaks = 100)
ex4_eq <- equalize_hist(ex4, nbreaks=100)
hist_ex4_eq <- hist(ex4_eq, breaks= 100)


# approaching infinite binning
ex5 <- rnorm(n= 100, mean=1, sd=1)
hist(ex5, breaks = 100)
ex5_eq <- equalize_hist(ex5, nbreaks=100000)
hist_ex5_eq <- hist(ex5_eq, breaks= 100)

# lower bound binning = 1
ex6 <- rnorm(n= 100, mean=1, sd=1)
hist(ex6, breaks = 100)
ex6_eq <- equalize_hist(ex6, nbreaks=1)
hist_ex6_eq <- hist(ex6_eq, breaks= 100)

# upper bound binning = n
ex7 <- rnorm(n= 100, mean=1, sd=1)
hist(ex7, breaks = 100)
ex7_eq <- equalize_hist(ex7, nbreaks=100)
hist_ex7_eq <- hist(ex7_eq, breaks= 100)



# logarithmic binnings
max_L = log(10, base= 2) + log(length(d), base=2)
eq_level <- m$equalize_bins[[dimension]]
k <- floor(eq_level * max_L)
nbreaks <- 2^k
