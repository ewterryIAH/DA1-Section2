library(DBI)
library(tidyverse)
library(odbc)
library(stringr)
library(matrixcalc)
library(reshape2)
library(car)


Advertising <-  dbGetQuery(con2,"
SELECT 
 [TV]
,[Radio]
,[Newspaper]
,[Sales]
FROM [dbo].[Advertising]
")


rfit <- lm(Sales ~ Radio, data = Advertising)
nfit <- lm(Sales ~ Newspaper, data = Advertising)


summary(rfit)
summary(nfit)

mFit <- lm(Sales ~ TV + Radio + Newspaper, data = Advertising)
summary(mFit)

# correlation matrix
cor(Advertising, method = 'pearson', use = 'pairwise')

# Solve and Manually Calculate SE's

vY <- as.matrix(dplyr::select(Advertising, Sales)) # set up y values in matrix                        
mX <- as.matrix(cbind(1, dplyr::select(Advertising, TV, Radio, Newspaper))) # set up x values in matrix
vBeta <- solve(t(mX)%*%mX, t(mX)%*%vY) # solve using normal equations                    
dSigmaSq <- sum((vY - mX%*%vBeta)^2)/(nrow(mX)-ncol(mX)) # estimate the variance  
mVarCovar <- dSigmaSq*chol2inv(chol(t(mX)%*%mX)) # get the var/cov using QR decomposition           
vStdErr <- sqrt(diag(mVarCovar)) # plug in the standard error                          
print(cbind(vBeta, vStdErr))    
summary(mFit)



