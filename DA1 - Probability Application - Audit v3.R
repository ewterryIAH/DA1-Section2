library(tidyverse)
library(fitdistrplus)
library(car)
library(sn)
library(stringr)
library(lubridate)


set.seed(1020)

skew <- function(x,e,w,a){
  t <- (x-e)/w
  2/w * dnorm(t) * pnorm(a*t)
}

# set your own location

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


# convert dates
Billing$Date <- ymd(Billing$Date)

# quick look
ggplot(Billing, aes(x = BilledAmt))+ geom_histogram()


# break out the years
Bill12 <- filter(Billing, Date >= "2012-01-01" & Date <= "2012-12-31")
Bill13 <- filter(Billing, Date >= "2013-01-01" & Date <= "2013-12-31")
Bill14 <- filter(Billing, Date >= "2014-01-01" & Date <= "2014-12-31")

# create a simple trend
Bill13$BilledAmt <- Bill13$BilledAmt*1.2
Bill14$BilledAmt <- Bill14$BilledAmt*1.4


# plot 2012 transactions
# more detailed look  - binwidth = 100
p2 <- ggplot(data =Bill12, aes(BilledAmt, ..density..))  + 
  geom_histogram(alpha = .2, fill = 'blue', binwidth = 100) +
  theme(panel.background = element_rect(fill = "white")) 
p2


# lets run a quick CF analysis
descdist(Bill12$BilledAmt, boot = 1000) 
# so obviously the distribution is heavily skewed. 


# use sn.mple to estimate parameters
# just trying to match up 2012 first

Bill12Model <- sn.mple(y = Bill12$BilledAmt, opt.method = "nlminb")$cp
# get values
Bill12Parameters <- cp2dp(Bill12Model, family = "SN")
# put parameters in variables


exi <- Bill12Parameters[1] # location
eomega <- Bill12Parameters[2] # scale
ealpha <- Bill12Parameters[3] # shape

# plot out the density using the skew fuction (just an estimate)

p2 <- p2 + geom_line(aes(x = Bill12$BilledAmt, y = dsn(Bill12$BilledAmt, exi, eomega, ealpha))) +
  theme(panel.background = element_rect(fill = "white")) 
p2

# generate a simulation 

p6 <- ggplot(data = Bill12, aes(x = BilledAmt))  + 
  geom_histogram(alpha = .2, fill = 'blue', binwidth = 500) +
  geom_histogram(aes(x = rsn(nrow(Bill12), exi, eomega, ealpha)), alpha = .2, fill = 'red', binwidth = 500) +
  theme(panel.background = element_rect(fill = "white")) 
p6


# is this any good? let's check

p2 <- ggplot(Bill12, aes(x=sort(BilledAmt), y=sort(rsn(nrow(Bill12), exi, eomega, ealpha)))) + 
  geom_point() + 
  labs(x="Actual",y="Simulated") +
  theme(panel.background = element_rect(fill = "white")) 
p2

# so this is a little skewed. See powerpoints

# -------- to compare the actual to the simulated by bin ----------------------
# just for understanding ##

tst <- data.frame(q5 = sort(Bill12$BilledAmt), q6 = sort(rsn(nrow(Bill12), exi, eomega, ealpha)))

pg <- ggplot_build(p6)
# this creates an R list, which is a little different data structure:(http://www.r-tutor.com/r-introduction/list)
# basically a way to store a bunch of different objects
pgData <- pg$data[[1]]

ActualQs <- tst %>%  count(cut_width(q5, 500, boundary = 50)) %>% rename("bin" = 1, cnt = 2) %>% mutate(cs = cumsum(cnt))
SimQs <- tst %>%  count(cut_width(q6, 500, boundary = 50)) %>% rename("bin" = 1, cnt = 2) %>% mutate(cs = cumsum(cnt))
compQs <- full_join(ActualQs, SimQs, by = "bin") %>% mutate(cntDiff = (cnt.x - cnt.y), csDiff = cumsum(cnt.x - cnt.y))

tst %>% dplyr::count(cut_width(q5, 500, boundary = 50)) 
tst %>% dplyr::count(cut_width(q6, 500, boundary = 50))


## Now, projecting 2012 onto 2013 -------------------------------- ##

# simulate 2013 data using the 2012 parameters with the 2013 volume (# of transactions)  

Bill13$y2 <- rsn(nrow(Bill13), xi=exi , omega=eomega , alpha=ealpha )


p13 <- ggplot(data =Bill13, aes(BilledAmt))  + 
  geom_histogram(alpha = .2, fill = 'blue', binwidth = 100) +
  theme(panel.background = element_rect(fill = "white")) 
p13

p13 <- p13 + geom_histogram(aes(y2), alpha = .2, fill = 'red', binwidth = 100) 
p13  

# so, y2 will be the simulation for 13 based on 12's parameters
# and Bill will be the actual

# plot and compare

# now you find out that prices increased 20%

exi <- exi * 1.2

Bill13$y2 <- rsn(nrow(Bill13), xi=exi , omega=eomega , alpha=ealpha )

p13 <- ggplot(data =Bill13, aes(BilledAmt))  + 
  geom_histogram(alpha = .2, fill = 'blue', binwidth = 100) +
  theme(panel.background = element_rect(fill = "white")) 
p13

p13 <- p13 + geom_histogram(aes(y2), alpha = .2, fill = 'red', binwidth = 100) 
p13  


# another good way to look at this kind of data and check assumptions

pBP <- ggplot(filter(Billing, Date < "2014-01-01"), aes(x=factor(year(Date)), y=BilledAmt)) + 
  geom_boxplot() + coord_flip()
pBP

tst <- data.frame(q5 = sort(Bill13$BilledAmt), q6 = sort(Bill13$y2))
tst <- rownames_to_column(tst, "SampleID")

# another qq

p3 <- ggplot(tst, aes(q5, q6)) + 
  geom_point() + 
  labs(x="Actual",y="Simulated") +
  theme(panel.background = element_rect(fill = "white")) 
p3

Bill13$Psn <- 1- psn(Bill13$BilledAmt, xi = exi, omega = eomega, alpha = ealpha) 

1e-13

tst2 <- Bill13 %>% arrange(desc(BilledAmt)) %>% filter(Psn < 1e-12)

p13 <- p13 + geom_histogram(data = tst2, aes(x = BilledAmt), fill = 'red', binwidth = 100) 
p13  

# zeroing in  
p13 + xlim(6500, 12500) 

