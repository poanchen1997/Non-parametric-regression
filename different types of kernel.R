# In this script, I want to make the kernel function by hand, 
# because some of the kernel is not in the package.
# reference : https://en.wikipedia.org/wiki/Kernel_(statistics)

# rectangle kernel
recKernel <- function(x, b){
  K <- ifelse(abs(x/b) <= 1, 1/2, 0)
  return(K)
}

# Box kernel : this is the same with the package ksmooth in "box" kernel
boxKernel <- function(x, b){  # Using this kernel can have the same result with ksmooth
  K <- ifelse(abs(x/b) <= .5, 1, 0)
  return(K)
}

# Triangular Kernel
TriKernel <- function(x, b){
  K <- ifelse(abs(x/b) <= 1, 1 - abs(x/b), 0)
}

# Epanechnikov kernel
EKernel <- function(x, b){
  K <- ifelse(abs(x/b) <= 1, (3/4)*(1-(x/b)^2), 0)
  return(K)
}

# Quartic Kernel
QKernel <- function(x, b){
  K <- ifelse(abs(x/b) <= 1, (15/16)*(1 - (x/b)^2)^2, 0)
  return(K)
}

# Triweight Kernel
TwKernel <- function(x, b){
  K <- ifelse(abs(x/b) <= 1, (35/32)*(1-(x/b)^2)^3, 0)
  return(K)
}

# Tricube Kernel
TcKernel <- function(x, b){
  K <- ifelse(abs(x/b) <= 1, (70/81)*(1-abs(x/b)^3)^3, 0)
  return(K)
}

# Gaussian Kernel (normal kernel)
GKernel <- function(x, b){
  K <- (1/sqrt(2*pi))*exp(-0.5*(x/b)^2)
  return(K)
}

# Cosine Kernel
CsKernel <- function(x, b){
  K <- ifelse(abs(x/b) <= 1, (pi/4)*cos((pi/2)*(x/b)), 0)
  return(K)
}

# Logistic Kernel
LKernel <- function(x, b){
  K <- 1/(exp(x/b) + 2 + exp(-x/b))
  return(K)
}

# Sigmoid function Kernel
SFKernel <- function(x, b){
  K <- (2/pi)*(1/(exp(x/b)+exp(-x/b)))
  return(K)
}

# Silverman kernel
SKernel <- function(x, b){
  K <- 0.5 * exp(-abs(x/b)/sqrt(2)) * sin((abs(x/b)/sqrt(2)) + (pi/4))
  return(K)
}

# ---------------------------------------------------
# test about our kernel
# import data
data <- read.table("fev.txt", header = T)
# check scatter plot
ggplot(data, aes(x = height, y = fev)) + geom_point()

x <- data$height
y <- data$fev
b <- 1.5 #bandwidth
kdeEstimateyX <- seq(min(x),max(x),.05)
ykernel <- NULL
for(xesti in kdeEstimateyX){
  xx <-  xesti - x
  K <-boxKernel(xx,b)  # just change the function here
  Ksum <- sum(K)
  weight <- K/Ksum
  yk <- sum(weight*y)
  xkyk <- c(xesti,yk)
  ykernel <- rbind(ykernel,xkyk)
}
plot(x,y, pch = 20, xlab = "Height", ylab = "Fev")
lines(ykernel[,1],ykernel[,2], col = 2, lwd = 2)

# for testing package Ksmooth with box kernel
model_loo <- ksmooth(data$height, data$fev, kernel = "box", bandwidth = 1.5)
lines(model_loo, col = 2, lwd = 2)