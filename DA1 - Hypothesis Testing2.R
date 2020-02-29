library(DBI)
library(tidyverse)
library(lubridate)
library(gridExtra)
library(sn)
library(ggridges)

# 1. From Class_2 Notes

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


Billing <- dbGetQuery(con2,"  
SELECT [ERP].[Billing].[TransID]
      ,[ERP].[Billing].[Date]
      ,[ERP].[Billing].[EmployeeID]
      ,[ERP].[ProjectType].[ProjectType]
      ,[ERP].[Billing].[BilledAmt]
  FROM [ERP].[Billing]
  INNER JOIN [ERP].[Project]
  ON [ERP].[Billing].[ProjectID]=
  [ERP].[Project].[ProjectID]
  INNER JOIN [ERP].[ProjectType]
  ON [ERP].[Project].[TypeID] =
 [ERP].[ProjectType].[TypeID]
 WHERE [ERP].[Billing].[CostCode] < '900' 
 AND [ERP].[Billing].[BilledAmt] > 0
                        ")  


Billing$Date <- ymd(Billing$Date)

p1 <- ggplot(Billing, aes(x = BilledAmt, color = factor(year(Date)))) + 
  geom_density() +
  theme(panel.background = element_rect(fill = "white")) 
p1


# let's draw some samples from the data


n <- 100
plotData <- matrix(nrow = 100, ncol = 2)
for(i in 1:100)
{
  samplen <- sample_n(Billing, n)
  mean <- mean(samplen$BilledAmt)
  plotData[i,] <- c(i, mean) 
}

dfPlotData <- data.frame(SampleID = plotData[,1], Mean = plotData[,2])

# now let's plot out all those means from the samples
p1 <- ggplot(data = dfPlotData, aes(x=Mean)) + geom_density(bw = 50)  +
  xlim(500,1100)
p1








































# ----------- stuff below -------------- #














set.seed(2)

popN <- 10000
popMean <- 10
popSD <- 2
x <- data.frame(x=rnorm(popN, popMean, popSD))
mean(x$x)
summary(x$x)
sd(x$x)

# lets try a demo taking samples from the h0 population


n <- 100
plotData <- matrix(nrow = 100, ncol = 3)
for(i in 1:100)
{
  samplen <- sample_n(x, n)
  mean <- mean(samplen$x)
  sd <- sd(samplen$x)
  plotData[i,] <- c(1, mean, sd) 
}

dfPlotData <- data.frame(plotData)
# now let's plot out all those means from the samples
p1 <- ggplot(data = dfPlotData, aes(x=X2)) + geom_histogram(binwidth = .01)  
p1

# note that the sample means are normally distributed

p2 <- ggplot(data = dfPlotData, aes(x=X2)) + geom_density(bw = .2)  
p2

SampleMean <- mean(dfPlotData$X2)
SampleSD <- mean(dfPlotData$X3)
sd(dfPlotData$X2) # note that the variance of the sample means will always be tighter than the pop. don't do that

# so lets project the pop based on the sample
ProjectedPop <- data.frame( x = rnorm(length(x$x), mean = SampleMean, sd = SampleSD))

p2 <- ggplot(data = ProjectedPop, aes(x=x)) + geom_density(bw = .5)  
p2

# whats the total population under 10
probU10 <- pnorm(10, mean = SampleMean, sd = SampleSD)
TotU10 <- probU10 * count(x)
# so, it's 5k - ck
count(filter(x, x < 10))
# righty rooty


# lets add some real world data

Bill13 <- read_csv("C:/Users/ellen/OneDrive/Documents/Fall 2019/DA1/Section2/Data/Bill13.csv")
BillingSample <- sample_n(Bill13, 100)
Billingmean <- mean(BillingSample$Bill)
Billingsd <- sd(BillingSample$Bill)

filter(BillingSample, Bill < 72)
# we found a 50% error rate in invoices less than $72
# and a 0% error rate with invoices over 72
# What's the projected number of invoices in error
TotalInError <- round(.5 * pnorm(72, mean = Billingmean, sd = Billingsd) * nrow(Bill13),0)

library(sn)
Bill13est <- sn.mple(y = BillingSample$Bill, opt.method = "nlminb")$cp
Bill13est <- cp2dp(Bill13est, family = "SN")

exi <- Bill13est[1]
eomega <- Bill13est[2]
ealpha <- Bill13est[3]


Proj <- data.frame(Bill = rsn(nrow(Bill13), exi, eomega, ealpha))
  
p2 <- ggplot(data = Proj, aes(x  = Bill)) + geom_density(bw = 50)
  theme(panel.background = element_rect(fill = "white")) 
p2

# we have a problem with invoices under 100

# sn estimate
estProb <- psn(100, exi, eomega, ealpha)

# actual
count(filter(Bill13, Bill < 101))/ nrow(Bill13)

# pnorm est
estProb2 <- pnorm(100, Billingmean, Billingsd)

























p2 <- ggplot(data = Bill13, aes(x=Bill)) + geom_density(bw = 50)  
p2

