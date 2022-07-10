# this script is used to compare the two type cross validation
library(ggplot2)
# import data
data <- read.table("fev.txt", header = T)
# check scatter plot
ggplot(data, aes(x = height, y = fev)) + geom_point()


# Leave-one-out Cross Validation -- using box kernel ---------------
max_h <- max(data$height) - min(data$height)

h_seq <- seq(0.5, max_h, 0.5) 
mse_vector <- rep(0, length(h_seq))
for (i in 1:length(h_seq)){
  h_using <- h_seq[i]
  cv_mse <- rep(NA, length(data$height))
  for (j in 1:length(data$height)){
    y_val_predict <- ksmooth(data$height[-j], data$fev[-j], kernel = "box", bandwidth = h_using, x.points = data$height[j])
    resid <- data$fev[j] - y_val_predict$y
    cv_mse[j] <- resid^2
  }
  mse_vector[i] <- mean(cv_mse, na.rm = T)
}
mse_vector
plot(mse_vector)
h_seq[which.min(mse_vector)]
# band width 3 have the lowest mse

# K-folds Cross Validation -- using box kernel -------------------
h_seq <- seq(0.5, max_h, 0.5) 
mse_vector_k <- rep(0, length(h_seq))
K <- 5
for (i in 1:length(h_seq)){
  h_using <- h_seq[i]
  cv_mse <- rep(NA, K)
  set.seed(111)
  folds <- sample(rep(1:K, length = length(data$height)))
  for (j in 1:K){
    train <- folds != j
    y_val_predict <- ksmooth(data$height[train], data$fev[train], kernel = "box", bandwidth = h_using, x.points = data$height[!train])
    resid <- data$fev[!train][order(data$height[!train])] - y_val_predict$y
    cv_mse[j] <- mean(resid^2)
  }
  mse_vector_k[i] <- mean(cv_mse, na.rm = T)
}
mse_vector_k

plot(mse_vector_k)  
h_seq[which.min(mse_vector_k)]
# the lowest is 0.1693549, corresponding h is 3 


model_loo <- ksmooth(data$height, data$fev, kernel = "box", bandwidth = 3)
resid_loo <- model_loo$y - data$fev[order(data$height)]
mean(resid_loo^2, na.rm = T)

model_kf <- ksmooth(data$height, data$fev, kernel = "box", bandwidth = 3)
resid_kf <- model_kf$y - data$fev[order(data$height)]
mean(resid_kf^2, na.rm = T)

plot(data$height, data$fev, pch = 20, xlab = "Height", ylab = "Fev")
lines(model_loo, col = 2, lwd = 3)
lines(model_kf, col = 4, lwd = 3)
legend(48, 5.5, legend=c("Loocv", "K-fold cv"),
       col=c(2, 4), lty = 1, title = "Validation type")

