library(tidyverse)
library(e1071)
library(kernlab)
library(stringr)

setwd('~/DA1/Section II/Data')


A <- matrix ( c(2, 2)) # this creates a vector

rm(A) #good to keep in mind if you're dealing wiht large datasets

A <- matrix ( c(2, 2)) # this creates a vector

A <- cbind(A, c(3,1)) # then combine vectors with cbind or

A <- matrix (c(2,3), nrow =1)

A <- rbind(A, c(2,1)) # then combine rows

A <- matrix( c(2, 2, 3, 1), nrow=2, ncol = 2) # or just create the matrix at once.

write.csv(A, file = "A.csv", row.names = FALSE)

A <- read.csv(file = "A.csv", header=TRUE, stringsAsFactors=FALSE)

# read.csv creates a dataframe (covered later)
str(A) #you always need to check on the data types that dataframes create
A <- as.matrix(A)

# s create a couple of vectors and move on with matrix operations
B <- c(3,2) # vector 1
C <- c(0,1) # Vector 2

D <- A #easy to duplicate data structures
D <- A[,1] # or parts - notice that D beccomes a vector
D <- cbind(D, A[,2]) # and back again
D <- t(D) 

E <- A+D # has to be same structure 
E <- B+C 
E <- B-C 

E <- A*D # elements
G <- A %*% D
H <- crossprod(A,D) # equal to t(A) %*% B or A %*% t(B)

# create identity matrix
E <- diag(A)
I <- diag(2)

J <- det(A)
J
#creates an inverse matrix
K <- solve(A)

# check to make sure it's right - should be identity matrix
L <- K%*%A

# singular value decomposition

X <- matrix( c(1, 1, 1, 1, 1, 2, 3, 4), nrow=4, ncol = 2) 

SingVal <- svd(X)
U <- SingVal$u
D <- diag(SingVal$d)
V <- t(SingVal$v)

X2 <- U %*% D %*% V

#  X = U D V'


# another use for solve - solving systems of equations.

# for example:
# 8x_1  + 20x_2 ???56  = 0
# 20x_1 + 60x_2 ???154 = 0 

X <- matrix( c(8, 20, 20, 60), nrow=2, ncol = 2) 
B <- matrix( c(56, 154), nrow=2, ncol = 1) 
solve(X, B)

# it just so happens, these are the two derivatives of the
# sum of squares of the following data

mydata <- read.csv(file="Ex1LS.csv", header=TRUE, sep=",")


X <- cbind(1, mydata$X)
y <- mydata$Y
# we can solve this from the raw data by using a transpose
betaHat <- solve(t(X)%*%X) %*% t(X) %*%y
print(betaHat)


# now solving using SVD

x <- t(X) %*% X
duv <- svd(x)
x.inv <- duv$v %*% diag(1 / duv$d) %*% t(duv$u)
x.pseudo.inv <- x.inv %*% t(X)
w <- x.pseudo.inv %*% y
w

# note we can also use SVD for dimension reduction (like PCA) 
# it's also used in advanced numerical solutions (won't be doing that here)

# exercise for fun

V1 <- data.frame(x=c(0, 3), y=c(0, 4))

p <- ggplot(V1, aes(x=x, y=y))+geom_point(color="black") 
p <- p + geom_segment(aes(x = V1[1,1], y = V1[1,2], xend = V1[2,1], yend =V1[2,2] ), arrow = arrow(length = unit(0.5, "cm")))
p <- p +xlim(0, 20) + ylim(0,20)
p

# create and draw the eigenvector (transpose first)
tV1 <- t(V1)
eV1 <- cbind(eigen(tV1)$values, eigen(tV1)$vectors)
ev2 <- eigen(V1)$values

V2<-data.frame(X=c(0, (eV1[1,1]*eV1[1,2])),Y=c(0, (eV1[1,1]*eV1[2,2])))
p <- p + geom_segment(aes(x = V2[1,1], y = V2[1,2], xend = V2[2,1], yend =V2[2,2] ), col="blue", linetype="dashed", arrow = arrow(length = unit(0.5, "cm")))
p

#create and draw the dot product vector
V3 <- as.matrix(V1)
dpV3 <- V3%*%V3
p <- p + geom_segment(aes(x = dpV3[1,1], y = dpV3[1,2], xend = dpV3[2,1], yend =dpV3[2,2] ), col="red", linetype="dashed", arrow = arrow(length = unit(0.5, "cm")))
p

# draw a right triangle (just for visual reference)

p <- p + geom_segment(aes(x = V1[2,1], y = V1[1,1], xend = V1[2,1], yend =V1[2,2] ), col="blue", linetype="dashed")
p

# get the vector magnitude (eclidian norm)
norm_vec <- function(x)sqrt(sum(x^2)) 
mV1 <- norm_vec(V1)

# Calculte the Direction Vector and Show that the norm = 1
cosX <- V1[2,1]/mV1
cosY <- V1[2,2]/mV1
V4 <- data.frame(X=c(0,cosX), Y=c(0,cosY))
# norm of the direction vector should be 1


p <- p + geom_segment(aes(x = V4[1,1], y = V4[1,2], xend = V4[2,1], yend =V4[2,2] ), col="red", linetype="dashed", arrow = arrow(length = unit(0.5, "cm")))
p

norm_vec(V4)

mV4 <- as.matrix(V4)
norm(mV4, type = '2')


