library(DBI)
library(tidyverse)
library(stringr)
library(lubridate)
library(corrplot)
library(ggcorrplot)
library(psych)
library(MASS)

# NOTE DATABASE - You'll need to connect to the Accouting database and use another connection (con2 here)


Advertising <- dbGetQuery(con2,"
SELECT 
 [TV]
,[Radio]
,[Newspaper]
,[Sales]
FROM [dbo].[Advertising]
")


Ad <- dplyr::select(Advertising, Sales, TV, Radio, Newspaper)

AdCorP <- cor(Ad, method="pearson")
AdCorS <- cor(Ad, method="spearman")
AdCorK <- cor(Ad, method="kendall")

ggcorrplot(AdCorP, lab = T)
ggcorrplot(AdCorS, lab = T)
ggcorrplot(AdCorK, lab = TRUE)

dfAdCorP <- mutate(data.frame(AdCorP), Method="pearson", Var = row.names(AdCorP))
dfAdCorS <- mutate(data.frame(AdCorP), Method="spearman", Var = row.names(AdCorS))
dfAdCorK <- mutate(data.frame(AdCorK), Method="kendall", Var = row.names(AdCorK))

dfAdCorP <- dplyr::select(dfAdCorP, Method, Sales, Var)
dfAdCorS <- dplyr::select(dfAdCorS, Method, Sales, Var)
dfAdCorK <- dplyr::select(dfAdCorK, Method, Sales, Var)
dfSumCor <- bind_rows(dfAdCorK, dfAdCorP, dfAdCorS)

ggplot(dfSumCor, aes(Var, Sales)) +   
  geom_bar(aes(fill = Method), position = "dodge", stat="identity")


cor(Ad, method="pearson", use="pairwise") 
cor(Ad, method="kendall", use="pairwise") 
cor(Ad, method="spearman", use="pairwise") 

pairs.panels(Ad, method = "pearson")

lMod <- lm(Sales ~ Newspaper, data = Ad)
summary(lMod)

lMod <- lm(Sales ~ TV+Radio+Newspaper, data = Ad)
summary(lMod)

#
# Chi Square - for categorical data
#
# get data from survey of students

Popular <- read.csv(file="C:/Users/ellen/OneDrive/Documents/Fall 2019/DA1/Section2/Data/StudentSurvey.csv", header=TRUE, sep=",")
PopTbl <- table(Popular$District, Popular$Priority)
PopTbl
chisq.test(PopTbl)

# is this significant at the .05 level?

# OK, let's get some more data and walk through another problem

# Using the Sales Data Now  

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
QuoteCor <- dplyr::select(SampleData, Result, RSF, QuoteDiff,RFPDiff, ATPDiff)

mQuoteCor <- data.matrix(QuoteCor)
QuoteCorP <- cor(mQuoteCor, method="pearson")
QuoteCorS <- cor(mQuoteCor, method="spearman")
QuoteCorK <- cor(mQuoteCor, method="kendall")

ggcorrplot(QuoteCorP, lab = TRUE)
ggcorrplot(QuoteCorS, lab = TRUE)
ggcorrplot(QuoteCorK, lab = TRUE)

# ok, now chi square on this data

Quote <- dplyr::select(SampleData, Quote_ID, RSF, Result)
Quote$RSF <- as.character(Quote$RSF)
Quote$Result <- as.character(Quote$Result)
Quote %>% distinct(Result)
Quote$Result[Quote$Result=="l"] <- "L" # fix a data error
# Just for visual:
Quote$RSF[Quote$RSF=="1"] <- "1 - None"
Quote$RSF[Quote$RSF=="2"] <- "2 - Developing"
Quote$RSF[Quote$RSF=="3"] <- "3 - Established"
Quote$RSF[Quote$RSF=="4"] <- "4 - Strong"

tblQuote <- table(Quote$RSF, Quote$Result)
tblQuote

#tblQuote <- tblQuote[,2:3]
#tblQuote
chisq.test(tblQuote)
