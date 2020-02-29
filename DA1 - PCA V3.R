library(DBI)
library(odbc)
library(tidyverse)
library(stringr)
library(ggplot2)
library(matrixcalc)
library(dplyr)
library(kernlab)
library(reshape2)

setwd('C:/Users/ellen/OneDrive/Documents/Fall 2019/DA1/Section2/EDA')

set.seed(7)

# OK, let's look at PCA theory - just for understanding (mc though)

rawdata <- read.csv(file="C:/Users/ellen/OneDrive/Documents/Fall 2019/DA1/Section2/Data/PCAData.csv", header=TRUE, sep=",")
#testInd <- sample(1:150,10)
testInd <- sample(1:150,20)
pcadata <- rawdata[testInd, 3:4]

#get the data and subtract the means
XMean <- sum(pcadata$X)/nrow(pcadata)
YMean <- sum(pcadata$Y)/nrow(pcadata)

pcadata$VarX <- round((pcadata$X - XMean),2)
pcadata$VarY <- round((pcadata$Y - YMean),2)

p <- ggplot(pcadata, aes(x=X2, y=Y2))+geom_point(color="black") 
p

# We can build covariances specifically
pcadata$covXY <- round(pcadata$VarX * pcadata$VarY,2)
totXY <- sum(pcadata$covXY)
covXY <- mean(pcadata$covXY)
pcaCovXY <- cov(pcadata$X, pcadata$Y)

#or just call a cov function
pcaCov <- data.frame(cov(pcadata[1:2]))

# note that both are positive, so we expect X and Y to increase together

pcaEigen <- eigen(pcaCov)
dfPcaEigen <- data.frame(values=pcaEigen$values,pcaEigen$vectors)  

VarMatrix <- pcadata[,3:4]
write.csv(VarMatrix, file = "VarMatrix.csv")
EigenMatrix <- t(dfPcaEigen[1,2:3])
write.csv(EigenMatrix, file = "EigenMatrix.csv")

newMatrix <- VarMatrix*EigenMatrix

# write the covariance matrix for further analysis
#write.csv(pcaCov, file = "PCA Ex 1 Output.csv")

#now let's get the eigens of te covariance matrix
pcaEigen <- eigen(pcaCov[1:2, 1:2])
pcaEigen <- data.frame(values=pcaEigen$values,pcaEigen$vectors)  


write.csv(pcaEigen, file = "PCAEigen.csv")

# get the 2 eigenvectors
V1<-data.frame(X=c(0, pcaEigen[1,2]),Y=c(0, pcaEigen[2,2]))
V1<- rbind(V1, c(V1[2,1]*pcaEigen[1,1],V1[2,2]*pcaEigen[1,1]))
V2<-data.frame(X=c(0, pcaEigen[1,3]),Y=c(0, pcaEigen[2,3]))
V2<- rbind(V2, c(V2[2,1]*pcaEigen[2,1],V2[2,2]*pcaEigen[2,1]))

#plot them out
pcaDataT <- t(pcadata)
#write.csv(pcaDataT, file = "PCADataT.csv")

p <- ggplot(pcadata, aes(x=VarX, y=VarY))+geom_point(color="black") 
p <- p + geom_segment(aes(x = V1[1,1], y = V1[1,2], xend = V1[3,1], yend =V1[3,2] ), arrow = arrow(length = unit(0.5, "cm")))
p <- p + geom_segment(aes(x = V1[1,1], y = V1[1,2], xend = -1*V1[3,1], yend = -1*V1[3,2] ), arrow = arrow(length = unit(0.5, "cm")))
p <- p + geom_segment(aes(x = V2[1,1], y = V2[1,2], xend = 10*V2[3,1], yend = 10*V2[3,2] ), arrow = arrow(length = unit(0.5, "cm")))
p <- p + geom_segment(aes(x = V2[1,1], y = V2[1,2], xend = -10*V2[3,1], yend = -10*V2[3,2] ), arrow = arrow(length = unit(0.5, "cm")))
p

# Note - added coefficients to improve visibility

pcaTheory2  <- rawdata[testInd, 3:4]
pcaTheory2.pca <- prcomp(pcaTheory2, retx=TRUE, center=TRUE, scale.=TRUE) 
pcaTheory2.pca
plot(pcaTheory2.pca)
summary(pcaTheory2.pca)



#------------------ Now let's do PCA on Quote Data ----------------------#



SampleData <- dbGetQuery(con2,"
SELECT [dbo].[Quote].[Quote_ID]
                         ,[dbo].[Customer].[Customer_Description]
                         ,[dbo].[Quote].[Product_ID]
                         ,CAST([dbo].[Quote].[Quote] AS FLOAT) AS Quote
                         ,[dbo].[Competitor].[Competitor_Description] AS Competitor_Description
                         ,CAST([dbo].[Quote].[Competitor_Quote] AS FLOAT) AS Competitor_Quote
                         ,[dbo].[Quote].[Date_Received]
                         ,[dbo].[Quote].[Date_Due]
                         ,[dbo].[Quote].[Date_Submitted]
                         ,[dbo].[Quote].[Date_Required]
                         ,[dbo].[Quote].[ATP]
                         ,[dbo].[Customer].[RSF]
                         ,[dbo].[Quote].[Result] 
                         FROM [dbo].[Quote] 
                         INNER JOIN [dbo].[Customer] 
                         ON [dbo].[Quote].[Customer_ID] = [dbo].[Customer].[Customer_ID] 
                         LEFT JOIN 
                         [dbo].[Competitor] 
                         ON [dbo].[Quote].[Competitor_ID] = [dbo].[Competitor].[Competitor_ID]
                         ")

SampleData$Quote <-as.numeric (SampleData$Quote)
SampleData$Competitor_Quote <-as.numeric (SampleData$Competitor_Quote)
SampleData$RSF <-as.numeric (SampleData$RSF)
SampleData$QuoteDiff <- as.numeric(SampleData$Competitor_Quote - SampleData$Quote)

SampleData$Date_Received <- as.Date(SampleData$Date_Received)
SampleData$Date_Due <- as.Date(SampleData$Date_Due)
SampleData$Date_Submitted <- as.Date(SampleData$Date_Submitted)
SampleData$Date_Required <- as.Date(SampleData$Date_Required)
SampleData$ATP <- as.Date(SampleData$ATP)

SampleData$RFPDiff <- as.numeric(SampleData$Date_Due - SampleData$Date_Submitted)
SampleData$ATPDiff <- as.numeric(SampleData$Date_Required - SampleData$ATP)

SampleData$Competitor_Description <- as.character(SampleData$Competitor_Description)
SampleData$Customer_Description <- as.character(SampleData$Customer_Description)
filter(SampleData, is.na(Competitor_Description))
SampleData$Competitor_Description[is.na(SampleData$Competitor_Description)] <- "Undefined"
filter(SampleData, is.na(Competitor_Description))
filter(SampleData, ATPDiff <0)
arrange(SampleData, ATPDiff, Quote)
dplyr::select(SampleData, Customer_Description, QuoteDiff:ATPDiff) # package conflicts causing problems with select
mutate(SampleData, ATPDiff2 = as.numeric(SampleData$Date_Required - SampleData$ATP))
SampleData$Result <- as.factor(SampleData$Result)


Quote1 <- SampleData[,c("RSF", "QuoteDiff","RFPDiff", "ATPDiff", "Result" )]
Quote1$Result <- as.factor(Quote1$Result)

Quote1.pca <- prcomp(Quote1[-5], retx=TRUE, center=TRUE, scale.=TRUE) 
Quote1.pca
plot(Quote1.pca)
summary(Quote1.pca)


# Let's look at this another way

variable.group <- c(rep(1, 4), rep(2, 4), rep(3, 4),
                    rep(4, 4))

melted <- cbind(variable.group, melt(Quote1.pca$rotation[,1:4]))
Quote1.pca$rotation

barplot <- ggplot(data=melted) +
  geom_bar(aes(x=Var1, y=value, fill=variable.group), stat="identity") +
  facet_wrap(~Var2)
barplot



# ------------------ Advertising Data Now ---------------------#

# note - error in query - do this!

Advertising <- dbGetQuery(con2,"
SELECT                    [Sales]
                          ,[TV]
                          ,[Radio]
                          ,[Newspaper]
                          FROM [dbo].[Advertising]
                          ")


Advertising.pca <- prcomp(Advertising[2:4], retx=TRUE, center=TRUE, scale.=TRUE) 
Advertising.pca
plot(Advertising.pca)
summary(Advertising.pca)
biplot(Advertising.pca, cex=c(.3,1.2))

# this is a really important visualization 
# we're typically looking to reduce dimensions here we hae a lot of interdepedency 

# comparison w lm

PC1m <- lm(Sales ~ TV + Radio + Newspaper, data = Advertising)
summary(PC1m)
PC2m <- lm(Sales ~ TV + Radio, data = Advertising)
summary(PC2m)


# Let's look at this another way

variable.group <- c(rep(1, 3), rep(2, 3), rep(3, 3))

melted <- cbind(variable.group, melt(Advertising.pca$rotation[,1:3]))
Advertising.pca$rotation

barplot <- ggplot(data=melted) +
  geom_bar(aes(x=Var1, y=value, fill=variable.group), stat="identity") +
  facet_wrap(~Var2)
barplot

# notice how PCA paints a very different story than correlation.


# ----------------------------  Kernel PCA ---------------------------- #

# Theory only! You do not have to use this!


# creating a custom kernel

rbfET <- function(x,y) exp(-0.1 * sum((x-y)^2))
class(rbfET) <- "kernel"

# creating a test matrix 

mTst <- data.matrix(rawdata)
mTst

# creating a kernel matrix

k <- kernelMatrix(rbfET, mTst)
dim(k)
k

# creating a principal components matrix of the kernel matrix

kpc1 <- kpca(k, kpar=list(sigma=0.2),features=2)
pcv(kpc1)

plot(rotated(kpc1),col=as.integer(rawdata[-testInd,5]),
     xlab="1st Principal Component",ylab="2nd Principal Component")

emb <- predict(kpc1,k)
points(emb,col=as.integer(rawdata[,5]))


# beautifu!

