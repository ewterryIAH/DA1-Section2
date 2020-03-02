library(tidyverse)
library(lubridate)
library(car)
library(stats4)
library(KernSmooth)
library(flux)
library(sfsmisc)
library(Rmisc)

# load functions 

# function to visualize density to point
drawCumDen <- function(pnt1, md1, sd1){
  dfData2 <- data.frame(x = dfData$x, y  = dnorm(dfData$x, mean = md1, sd = sd1))  
  dfData2a <- filter(dfData2, x < pnt1) %>% arrange(x)
  dfData2a <- rbind( c( min( dfData2a$x), 0), 
                     + dfData2a, 
                     + c( max( dfData2a$x), 0))
  p1a <- ggplot(dfData2) + 
    geom_line(aes(x,y)) +
    xlab("value") +
    ylab("density") +
    geom_polygon(data = dfData2a, aes(x, y), fill = 'blue', alpha = 0.1) +
    theme(panel.background = element_rect(fill = "white")) 
  return (p1a)  
}


# lets create a function to host our data gathering (see chpt 19 of book)

getProb <- function(p, dVal)
{
  pg <- ggplot_build(p)
  pgData <- pg$data[[1]]
  pgData <- pgData %>% mutate(prob = (xmax- xmin)*density) %>% filter(x >=  dVal)
  totProb <- sum(pgData$prob, na.rm = T)
  return(totProb)
}

#---------------------------#

dfData <- data.frame(x = seq(-5, 5, by = .01))

p1 <- ggplot(dfData) + 
  geom_line(aes(x,y= dnorm(x, mean = 0, sd = .5))) + 
  geom_line(aes(x,y= dnorm(x, mean = 0, sd = 1)), color = "gray") +
  geom_line(aes(x,y= dnorm(x, mean = 0, sd = 2)), color = "red") +
  geom_line(aes(x,y= dnorm(x, mean = 2, sd = 1)), color = "yellow") +
  geom_line(aes(x,y= dnorm(x, mean = -2, sd = 1)), color = "green") +
  xlab("value") +
  ylab("density") +
  theme(panel.background = element_rect(fill = "white")) 
p1

# remember, that density number is density, not probability

p <- drawCumDen(-1, 0, 1) 
p
pnorm(-1, mean = 0, sd = 1)

# now we are talking probability (the area from -inf => -1)
# or to 0 (what do you think that will be?)

p <- drawCumDen(0, 0, 1) 
p
pnorm(0, mean = 0, sd = 1)

p <- drawCumDen(1, 0, 1) 
p
pnorm(1, mean = 0, sd = 1)

p <- drawCumDen(2, 0, 1) 
p
pnorm(2, mean = 0, sd = 1)

# with cummulative prob
# x axis becomes x values - y is Cummulative prob
# so we create a grid to map prob across values

dfData <- data.frame(x = seq(-5, 5, by = .01))

p1 <- ggplot(dfData) + 
  geom_line(aes(x,y= pnorm(x, mean = 0, sd = 1))) +
  xlab("Value") +
  ylab("Cummulative Prob") +
  theme(panel.background = element_rect(fill = "white")) 
p1




# let's plot out the probability at each of those points. (remember, we're talking about -inf => number)

pCD <- data.frame(x = c(-1, 0, 1, 2), y = pnorm(c(-1, 0, 1, 2), mean = 0, sd = 1))
ggplot(pCD, aes(x, y)) + geom_point() + xlim(c(-5, 5)) +
  theme(panel.background = element_rect(fill = "white"))

# normal distribution 
# plotting out pnorm at each point gives us a Cummulative Probability curve
# this is often more informative than the density

dfData <- data.frame(x = seq(-5, 5, by = .01))
# x axis becomes x values - y is Cummulative Prob
p1 <- ggplot(dfData) + 
  geom_line(aes(x,y= pnorm(x, mean = 0, sd = .5))) +
  geom_line(aes(x,y= pnorm(x, mean = 0, sd = 1)), color = "gray") +
  geom_line(aes(x,y= pnorm(x, mean = 0, sd = 2)), color = "red") +
  geom_line(aes(x,y= pnorm(x, mean = 2, sd = 1)), color = "yellow") +
  geom_line(aes(x,y= pnorm(x, mean = -2, sd = 1)), color = "green") +
  xlab("Value") +
  ylab("Cummulative Prob") +
  theme(panel.background = element_rect(fill = "white")) 
p1

# add the points above
p1 <- p1 + geom_point(data  = pCD, aes(x, y))
p1


# ok let's look at quantiles which isn't density OR probability,
# but the order of data

dfData <- data.frame(x = seq(0, 1, by = .01))
# x axis becomes x quantiles 
p1 <- ggplot(dfData) + 
  geom_line(aes(x,y= qnorm(x, mean = 0, sd = 2))) 

p1 <- p1 +
  geom_line(aes(x,y= qnorm(x, mean = 0, sd = 1)), color = "gray") +
  geom_line(aes(x,y= qnorm(x, mean = 0, sd = 2)), color = "red") +
  geom_line(aes(x,y= qnorm(x, mean = 2, sd = 1)), color = "yellow") +
  geom_line(aes(x,y= qnorm(x, mean = -2, sd = 2)), color = "green") +
  xlab("quantile") +
  ylab("value") +
  theme(panel.background = element_rect(fill = "white")) 
p1

# what is a quantile? do different data
dfData <- data.frame(x = seq(0, 10, by = .1))
# since we're using a sequence, it's already ordered
quantile(dfData$x) # these are quartiles (it orders it for you)

dfData <- data.frame(x = rnorm(n = 1000, mean = 0, sd = 1))
quantile(dfData$x) # so this orders it for you
# it doesn't have to be quartiles:
quantile(dfData$x, probs = c(0.05, 0.95))

# what if we wanted the 50th percentile
qnorm(.5, mean = 0, sd = 1)
qnorm(.25, mean = 0, sd = 1)

dfData %>% arrange(dfData$x) %>% slice(as.integer(round(nrow(dfData)*.25,0))) # remember, rnorm is random

pQ <- data.frame(x = c(.25, .5, .75, 1), 
                 y = qnorm(c(.25, .5, .75, 1), mean = 0, sd = 1))
p1 <- p1 + geom_point(data  = pQ, aes(x, y))
p1


# let's do a different dataset

dfData <- data.frame(x = rnorm(n = 100, mean = 50, sd = 10))
p1b <- ggplot(dfData, aes(x)) + geom_density(bw = 10) +
  theme(panel.background = element_rect(fill = "white")) +       
  xlim(c(0, 100))
p1b
p1b <- ggplot(dfData, aes(x)) + stat_ecdf() +
  theme(panel.background = element_rect(fill = "white")) +       
  xlim(c(0, 100))
p1b


# quantiles are just sorted data
dfData$x <- sort(dfData$x)
# what's the 50th percentile?
dfData %>% slice(as.integer(round(nrow(dfData)*.5,0)))
# remember, it's random

mX <- mean(dfData$x)
sdX <- sd(dfData$x)
# create a theoretical normal distribution of quantiles
dfData$y <- sort(qnorm(seq(0, 1, length.out = 100), mean = mX, sd = sdX))  #k and this is a theoretical norm distribution
# remove infs (like nas)
dfData <- filter(dfData, !is.infinite(y))
# and compare the two
ggplot(dfData, aes(x, y)) + geom_point() +
    theme(panel.background = element_rect(fill = "white")) 


# but here's the thing. The distribution we compare this with
# doesn't need to have the same mean, sd OR range
# this comparison still works

dfData$y <- sort(rnorm(98, 0, 1))
ggplot(dfData, aes(x, y)) + geom_point() +
  theme(panel.background = element_rect(fill = "white")) 


# see how this lines up?

set.seed(35)

# starting with your statistics texbook
round(1 - pnorm(1.5, mean = 0, sd = 1),3)


# let's set the sd @ 1.96
# what does this mean?
round(1 - pnorm(1.96, mean = 0, sd = 1),3)

# lets create a distribution

x <- data.frame(x = rnorm(100, mean = 10, sd = 2))
ggplot(x, aes(x = x)) + geom_density(bw = 2) +
  theme(panel.background = element_rect(fill = "white")) +
  scale_x_continuous(limits = c(0, 20))

# now let's get the mean and variance the hard way
# whats the probability of x being 1 or less

mean1 <- round(sum(x$x)/length(x$x),0)
mean1
sd1 <- round(sqrt(sum((x$x-mean1)^2)/(nrow(x)-1)),0)
sd1


# let's say x = 15
# compute standardized value for z
z1 <- abs(15-mean1)/sd1
z1
# look up in table (close to .006 probability)

# compare to pnorm - neg side of mean now
round(1 - pnorm(15, mean = mean1, sd = sd1),3)



# HISTOGRAMS
# set up your data and aes in ggplot initiation.
# added functions access the data and aesthetics from there.
# then you can MODIFY data and aesthetics in the added functions.
# keep it clean and build on

dfDiamonds <- data.frame(diamonds)

ggplot(diamonds, aes(x = cut)) + geom_bar()

diamonds %>% dplyr::count(cut)

# here's a very useful function in tidyverse called cut_width that lets you get see 
# the data that histogram bins are bulding on
# with historgrams you need to control the bins or misinterpretation will happen

ggplot( data = diamonds, aes(x = carat)) + geom_histogram(binwidth = 2)
# you can change the binwidth
# and you can set boundaries, but the hueristic in ggplot is generally sufficient for most
ggplot( data = diamonds, aes(x = carat)) + geom_histogram(binwidth = 1, boundary = 0) 
ggplot( data = diamonds, aes(x = carat)) + geom_histogram(binwidth = 2)

diamonds %>% dplyr::count(cut_width(carat, 2)) # this let's cut_width build the intervals
# same boundary parameter for cut_width
diamonds %>% dplyr::count(cut_width(carat, 2, boundary = 0)) # this let's cut_width build the intervals
# and you can set breaks
diamonds %>% dplyr::count(cut(carat,breaks=c(-1, 1, 3, 5,7))) # or you can define the intervals specifically

# or you can define the number of bins
p <- ggplot( data = diamonds, aes(x = carat)) + geom_histogram(bins = 4)
p

# or you can look at this by density
p <- ggplot( data = diamonds, aes(x = carat, y= ..density..)) + geom_histogram(bins = 4)
p

# same data - what's density? learn density!

# we can get the data from ggplot this way
pg <- ggplot_build(p)
# this creates an R list, which is a little different data structure:(http://www.r-tutor.com/r-introduction/list)
# basically a way to store a bunch of different objects
pgData <- pg$data[[1]]
pgData %>%  dplyr::select(x, density)

pgData <- pgData %>% mutate(prob = (xmax- xmin)*density)
totProb <- sum(pgData$prob, na.rm = T)
totProb
# note total prob is = 1

dplyr::select(pgData, xmin, xmax, count, density, prob)
# what's the probabilty of finding a diamond between .8 and 2.4 carats? it's 40%
# what's the probabilty of finding a diamond over 4 carats? it's .009%

# this is a very important concept. The value of the density function is used to compute
# the AREA of the range and that gives you the probability 
# we'll come back to this so park that idea

# back to plotting

# you can also define the breaks - the number of breaks
diamonds %>% dplyr::count(cut(carat, breaks=4)) 

# lets create smaller and smaller bins
ggplot( data = diamonds, aes(x = carat)) + geom_histogram(binwidth = 0.5)
diamonds %>% dplyr::count(cut_width(carat, 0.5))
ggplot( data = diamonds, aes(x = carat)) + geom_histogram(binwidth = 0.1)
diamonds %>% dplyr::count(cut_width(carat, 0.1))
# notice how you begin to see more about the data with smaller bins


# or you can focus in a section of the data
ggplot(filter(diamonds, carat <2), aes(x = carat )) + geom_histogram(binwidth = 0.1)

# OK, let's try another example with a different dataset

# new data

UN<- UN %>% filter(!is.na(infantMortality)) 


# NOTE: infantMortality is infant.mortality on last handout due to a change in datasource
# just use find/replace

ggplot( data = UN, mapping = aes( x = infantMortality)) + geom_histogram()

# now let's change aesthetics

ggplot( data = UN, mapping = aes( x = infantMortality)) + 
      geom_histogram(binwidth = 20, color = 'black', fill = 'white')+ 
      xlim(c(-10, 200))

ggplot( data = UN, mapping = aes( x = infantMortality)) + 
  geom_histogram(binwidth = 20)

# you can use formulas in the bins

ggplot( data = UN, mapping = aes( x = infantMortality)) + 
  geom_histogram(bins = round(max(UN$infantMortality)/2,0))


p <- ggplot(UN, aes( x = infantMortality, y= ..density.. )) + 
      geom_histogram(binwidth = 20, color = 'black', fill = 'gray')+ 
      xlim(c(-10, 200))
p

# plot the density (this is now recalcuated without bins - pure continuous function)
p <- p + geom_density(color = 'red')
p

# check density function again (trying to drill this in :) )

pg <- ggplot_build(p)
pgData <- pg$data[[1]]
pgData %>%  dplyr::select(x, density)
pgData <- pgData %>% mutate(prob = (xmax- xmin)*density)
totProb <- sum(pgData$prob, na.rm = T)
totProb
dplyr::select(pgData, xmin, xmax, count, density, prob)

# so what's the probablity of a value between -10 and 10? 
# it's  31%

# let's do one more - a normal distribution this time

# once created, check the Environment

# now lets test it out

ndist <- data.frame(x = rnorm(10000,0,10)) 
# note how r provides all these statistical functions that make it easy 

p <- ggplot(ndist, aes(x = x, y= ..density..)) + 
  geom_histogram(binwidth = 10, color = 'black', fill = NA)
p
# plot the density (this is now recalcuated without bins - pure continuous function)
p <- p + geom_density(color = 'red', bw = 5)
p
# now get the estimated probablity over the mean

estProb <- getProb(p, 0)
estProb

#hmmm not too good! 
#k what if we decrease bin size?

p <- ggplot(ndist, aes(x = x, y= ..density..)) + 
  geom_histogram(binwidth = 5, color = 'black', fill = NA)
p
# plot the density (this is now recalcuated without bins - pure continuous function)
p <- p + geom_density(color = 'red', bw = 5)
p

estProb <- getProb(p, 0)
estProb

# a little better
#Let's decrease binwidth by 1 using a loop (chpt 21 of book)
for (i in 10:1)
{
  p <- ggplot(ndist, aes(x = x, y= ..density..)) +  geom_histogram(binwidth = i, color = 'black', fill = NA)
  estProb <- getProb(p, 0)
  print(estProb)
  }
p

# so do you see how this gets closer and closer to .5 (which we know because it's a normal distribution)?
# that's how integral calculus works, and we can just make life easy and take the integral here
# at least 3 different ways to do this in R:

integrate(approxfun(density(ndist$x)), lower = 0, upper = 38, subdivisions=2000)$value

# breaking this down and looking at different ways to do it:
df <- approxfun(density(ndist$x))
DenDF <- data.frame(x = ndist$x, y = df(ndist$x))
df2 <- dplyr::filter(DenDF, x >= mean(x))
integrate.xy(df2$x, df2$y)
# OR
auc(df2$x, df2$y)

# the inteegrate(approxfun) uses linear interpolation, the AUC uses a trapazoid method


# we'll come back to all this - just wanted to introduce now


# ------------------- introducing distrubutions with cdf and qq --------------#

# let's generate 3 distributions - skew right, skew left and 'normal'

set.seed(33)
library(sn)
x <- rnorm(1000, mean = 0, sd = 4)
sr <- rsn(1000, xi=0, omega=10, alpha= 10)
sl <- rsn(1000, xi=0, omega=10, alpha=-10)
mP <- data.frame(x, sr, sl)

ggplot(mP, aes(x)) + 
  geom_density(bw = 4) +
  geom_density(aes(x= sr), bw = 4, color = "red") +
  geom_density(aes(x= sl), bw = 4, color = "blue") +
  theme(panel.background = element_rect(fill = "white")) +
  scale_x_continuous(limits = c(-30, 30)) 

ggplot(mP, aes(x  = x)) + 
  stat_ecdf() +
  stat_ecdf(aes(x = sr), color = "red") +
  stat_ecdf(aes(x = sl), color = "blue") +
  theme(panel.background = element_rect(fill = "white")) 

ggplot(mP, aes(sample  = x)) + 
  stat_qq() +
  stat_qq(aes(sample = sr), color = "red") +
  stat_qq(aes(sample = sl), color = "blue") +
  theme(panel.background = element_rect(fill = "white")) 

# sort for quantiles (that's really all quatiles are)
qq <- data.frame(x = sort(mP$x), sr = sort(mP$sr), sl = sort(mP$sl))

ggplot(qq, aes(x, sr))+ geom_point() + 
  theme(panel.background = element_rect(fill = "white")) 

ggplot(qq, aes(x, sl))+ geom_point() + 
  theme(panel.background = element_rect(fill = "white")) 


# ---------------------------- CDF and QQ Applied to UN data -----------------------------------#


p1 <- ggplot(UN, aes(x = infantMortality, y = ..density..)) + geom_density()
p2 <- ggplot( data = UN, mapping = aes( x = infantMortality)) + stat_ecdf()
q1 <- ggplot( data = UN, mapping = aes( sample = infantMortality)) + geom_qq() 
q1 <- q1 + geom_point(aes(3,4))
multiplot(p1, p2, q1, cols=2)

# let's look at the cumulative density
pg <- ggplot_build(p2)
pgData1 <- pg$data[[1]]
MidP2 <- pgData1 %>% dplyr::select(x, y) %>% arrange(x)  %>% slice(as.integer(round(nrow(qData1)*.5,0)))
p2 <- p2 + geom_point(aes(MidP2$x, MidP2$y), color = "red", size = 3)
p2 <- p2 + geom_vline(xintercept = MidP2$x, color  = "red")

pg <- ggplot_build(q1)
qData1 <- pg$data[[1]]
MidQ1 <- qData1 %>% dplyr::select(x, y) %>% arrange(x) %>% slice(as.integer(round(nrow(qData1)*.5,0)))
q1 <- q1 + geom_point(aes(MidQ1$x,MidQ1$y), color = "red", size = 3)
q1 <- q1 + geom_hline(yintercept = MidQ1$y, color  = "red")
  

multiplot(p1, p2, q1, cols=2)


qData1Mid <- filter(qData1, x == 0) # don't need to find the mid point because the qq is centered @ 0
qData1Mid

pgData1Mid2 <- filter(pgData1, x == qData1Mid$y)

# compare to normal

ndist <- data.frame(x=rnorm(length(UN$infantMortality),mean = pgData1Mid$x))
p3 <- ggplot(ndist, aes(x = x, y = ..density..)) + geom_density()
p4 <- ggplot( data = ndist, mapping = aes( x = ndist$x)) + stat_ecdf()
q2 <- ggplot( data = ndist, mapping = aes( sample = ndist$x)) + geom_qq()
multiplot(p3, p4, q2, cols=2)

pg <- ggplot_build(p4)
pgData2 <- pg$data[[1]]
pgData2 <- pgData2 %>% dplyr::select(x, y) %>% arrange(x)
pgData2Mid <- pgData2[round((nrow(pgData2)/2),0),]
pgData2Mid

pg <- ggplot_build(q2)
qData2 <- pg$data[[1]]
qData2 <- qData2 %>% dplyr::select(x, y) %>% arrange(x)
qData2Mid <- filter(qData2, round(x,3) == 0) # don't need to find the mid point because the qq is centered @ 0
qData2Mid

pgData2Mid2 <- filter(pgData2, x == qData2Mid$y)




# ---------------- a few more gg fuctions --------------------------#

ggplot( data = diamonds, mapping = aes( x = carat, color = cut)) + 
  geom_freqpoly( binwidth = 0.1)

ggplot( data = diamonds, mapping = aes( x = carat)) + geom_histogram( binwidth = 0.01)

# Outliers

ggplot( diamonds) + geom_histogram( mapping = aes( x = y), binwidth = 0.5)

ggplot( diamonds) + geom_histogram( mapping = aes( x = y), binwidth = 0.5) + 
  coord_cartesian( ylim = c( 0, 50))

# multiple histograms

ggplot(diamonds, mapping = aes(price, color = cut)) + geom_freqpoly(binwidth = 500)

ggplot(diamonds, aes(price, ..density.., colour = cut)) + geom_freqpoly(binwidth = 500)

# boxplots 

p2 <- ggplot(pgData, aes(y = pgData$x, x = pgData$y)) + geom_boxplot() + coord_flip()
p2
library(Rmisc)
multiplot(p, p2, cols=1)

ggplot(data = mpg) + 
  geom_boxplot( mapping = aes( x = reorder( class, hwy, FUN = median), y = hwy) ) +
  coord_flip()


ggplot(diamonds, aes(carat, price)) + 
  geom_boxplot(aes(group = plyr::round_any(carat, 0.1))) 

ggplot(diamonds, aes(carat, price)) + 
  geom_boxplot(aes(group = cut_width(carat, 0.25))) 

ggplot(diamonds, aes(x = carat)) + geom_histogram()
diamonds %>% dplyr::count( carat)


