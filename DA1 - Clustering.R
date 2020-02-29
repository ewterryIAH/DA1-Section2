library(tidyverse)
library(RODBC)
library(MASS)
library(caret)


custChurn <- read.csv("C:/Users/ellen/Documents/Fall 2018/DA1/Section2/Blackboard/Data/CustomerChurn.csv")

# this is an example from DA2 training a linear discriminant analysis algorithm to 
# predict whether a customer will leave or not
# you don't have to know the lda piece
# just pay attention to the clustering - for intuition

# get rid of all the nas
custChurn <- na.omit(custChurn)

# let's trim outliners 
custChurn <- filter(custChurn, Logins <250)

custChurn$Churn <- as.factor(custChurn$Churn)

# see if we can find a distribution we can grab

ggplot(custChurn, aes(SupportCases, fill = Churn)) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(ylim = c(0, 100), xlim =c(-10,10)) 

ggplot(custChurn, aes(AvgSupPriority, fill = Churn)) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(ylim = c(0, 100), xlim =c(-10,10)) 

p <- ggplot(custChurn, aes(Logins, fill = Churn)) +
  geom_histogram(binwidth = 5) +
  coord_cartesian(ylim = c(0, 20), xlim =c(-100,100)) 
p


library(DMwR)
smoteTrain <- SMOTE(Churn ~ ., custChurn, perc.over = 100, perc.under=300)
prop.table(table(smoteTrain$Churn))


lda.fit <- lda(Churn ~ ., data = smoteTrain) 
lda.pred <- predict(lda.fit, smoteTrain)

testSplit <- .4
totalSampleSize <- nrow(custChurn)
testSampleSize <- round(totalSampleSize*testSplit)
trainSampleSize <- totalSampleSize - testSampleSize
tindexes <- sample(1:nrow(custChurn), testSampleSize)
indexes <- sample(1:nrow(custChurn[-tindexes,]), trainSampleSize)
xTrain <- custChurn[indexes, ]
xTest <- custChurn[tindexes,]

lda.fit <- lda(Churn ~ ., data = xTrain) 
lda.fit

lda.pred <- predict(lda.fit, xTest)
confusionMatrix(lda.pred$class, xTest$Churn)

churnK3 <- kmeans(x = custChurn[,2:5], centers = 2) # set centers = 2 since we know there are 2 classifcations we care about - W and L
custChurn$cluster <- as.factor(churnK3$cluster)

p <- ggplot(custChurn, aes(AvgSupPriority, Logins, color = cluster)) + geom_point()
p


churnK3 <- kmeans(x = xTrain[,2:5], centers = 2) # set centers = 2 since we know there are 2 classifcations we care about - W and L
xTrain$cluster <- as.factor(churnK3$cluster)

p <- ggplot(xTrain, aes(Logins, SupportCases, color = cluster)) + geom_point()
p

# Using the Quote Data Now  


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

  Sales <- SampleData[,c("RSF", "QuoteDiff","RFPDiff", "ATPDiff", "Result" )]
  Sales$Result <- as.factor(Sales$Result)
  mSales <- data.matrix(Sales)
  dfSales <- data.frame(mSales)

set.seed(20) # setting a seed makes it reproducable

salesK3 <- kmeans(x = Sales[,1:4], centers = 4, nstart = 20) # set centers = 2 since we know there are 2 classifcations we care about - W and L
salesK3
salesK3$cluster <- as.factor(salesK3$cluster)
Sales$Cluster <- salesK3$cluster

p <- ggplot(Sales, aes(QuoteDiff, ATPDiff, color = Cluster)) + geom_point()
p
p <- p + facet_grid(RSF ~.)
p





