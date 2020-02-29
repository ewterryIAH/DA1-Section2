library(DBI)
library(odbc)
library(tidyverse)
library(Hmisc)
library(corrplot)
library(kernlab)
library(useful) 
library(stringr) 



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





